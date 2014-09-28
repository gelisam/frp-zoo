module Main where

import Graphics.Gloss


main :: IO ()
main = play (InWindow "Gloss Example" (200, 200) (800, 200))
            white
            300
            ()
            (const $ circle 10)
            (\_ () -> ())
            (\_ () -> ())
