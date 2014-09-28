module Main where

import Graphics.Gloss

import Buttons


main :: IO ()
main = play (InWindow "Gloss Example" (320, 240) (800, 200))
            white
            300
            ()
            (const $ renderButtons (0,0,0))
            (\_ () -> ())
            (\_ () -> ())
