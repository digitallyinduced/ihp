{-# LANGUAGE TemplateHaskell #-}
module Web.SlowLoad where

import Control.Concurrent (threadDelay)
import Language.Haskell.TH.Syntax (Dec, runIO)
import Prelude (Int, pure, (*))

$(do
    -- Keep GHCi in the initial :l Main.hs load long enough for the SIGTERM
    -- regression check to hit the orphaning window deterministically.
    runIO (threadDelay (10 * (1000000 :: Int)))
    pure ([] :: [Dec]))
