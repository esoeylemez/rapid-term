-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module DevelMain (update) where

import Control.Concurrent
import Rapid
import Rapid.Term
import System.IO


update :: IO ()
update =
    rapid 0 $ \r -> do
        t <- createRef r "term-ref" newTermRef
        start r "term" (runTerm (urxvtAt "./urxvt") t)
        restart r "test-app" . stats t . redirect t $ do
            putStrLn "blah"
            3^5000000 `seq` threadDelay 1000000
            putStrLn "blubb"
