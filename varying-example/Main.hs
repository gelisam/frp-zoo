{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Control.Varying
import Buttons

main :: IO ()
main = G.playIO (InWindow "Varying Example" (320, 240) (800, 200))
                white
                30
                (renderButtons 0 (Just 0) 1 (Just 5) 2 (Just 2))
                return
                (\e world -> return world) -- step events
                (\dt world -> return world) -- step time
