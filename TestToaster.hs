module TestToaster where

import Haxl.Core
import Haxl.Prelude
import Toaster
import Prelude ()

test = do
  st <- Toaster.initGlobalState
  env <- initEnv (stateSet st stateEmpty) ()

  -- Run an example expression with two fetches:
  runHaxl env $ do
     sequence [say "do you want some toast?", toast 1, say "bye"]
     sequence_ (replicate 10 (temperature >>= say . show))
