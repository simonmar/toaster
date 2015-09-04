{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Toaster (
    -- * initialise the state
    initGlobalState,

    -- * requests for this data source
    ToasterReq(..),
    say, toast, temperature
  ) where

import Haxl.Prelude
import Prelude ()

import Haxl.Core

import Data.Typeable
import Data.Hashable
import Control.Concurrent
import System.IO
import System.Random

data ToasterReq a where
  Toast :: Int -> ToasterReq ()
  Say :: String -> ToasterReq ()
  Temperature :: ToasterReq Int
  deriving Typeable -- requests must be Typeable

toast :: Int -> GenHaxl u ()
toast = uncachedRequest . Toast

temperature :: GenHaxl u Int
temperature = uncachedRequest Temperature

say :: String -> GenHaxl u ()
say = uncachedRequest . Say

deriving instance Eq (ToasterReq a)

deriving instance Show (ToasterReq a)

instance Show1 ToasterReq where show1 = show

instance Hashable (ToasterReq a) where
   hashWithSalt s (Toast a) = hashWithSalt s (0::Int,a)
   hashWithSalt s (Say a)    = hashWithSalt s (1::Int,a)
   hashWithSalt s Temperature    = hashWithSalt s (2::Int)

instance StateKey ToasterReq where
  data State ToasterReq = ToasterState { }

instance DataSourceName ToasterReq where
  dataSourceName _ = "Toaster"

instance DataSource u ToasterReq where
  fetch _ _ _ bfs = SyncFetch $ do
    putStrLn "sending commands to toaster ..."
    mapM_ runToaster bfs
    putStrLn "toaster done."

runToaster :: BlockedFetch ToasterReq -> IO ()
runToaster (BlockedFetch Temperature m) =
  putSuccess m =<< randomRIO (50, 100 :: Int)
runToaster (BlockedFetch (Toast n) m) = do
  putStrLn ("Remote: Toasting...")
  threadDelay (1000 * 1000 * n)
  putStrLn ("Remote: Done!")
  putSuccess m ()
runToaster (BlockedFetch (Say s) m) = do
  putStrLn s
  putSuccess m ()

initGlobalState :: IO (State ToasterReq)
initGlobalState = return ToasterState { }

