module GlossInterface where

import Prelude hiding (id, (.))

import FRP.Yampa

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

import Data.IORef

playYampa :: 
    Display ->
    Color ->
    Int ->
    SF (Event G.Event) Picture ->
    IO ()

playYampa display color frequency network = 
  do
    vPic <- newIORef Blank

    handle <- reactInit 
        (return NoEvent)
        (\_ changed pic -> 
          do
            if changed then vPic `writeIORef` pic else return ()
            return False)
        network
    
    _ <- react handle (infts, Just NoEvent)

    -- Since `react` requires nonzero time intervals,
    -- we pass infinitesimal time intervals and accumulate them on
    -- the variable `t`. Then every frame `delta` is corrected by `t`.
    G.playIO
        display
        color
        frequency
        infts -- initial t. This is for initial step
        (const $ readIORef vPic)
        (\e t -> react handle (infts, Just (Event e)) >> return (t+infts))
        (\delta t ->
            let 
                delta' = realToFrac delta - t
              in
                if delta' > 0
                  then do
                    _ <- react handle (delta', Just NoEvent)
                    return 0.0
                  else
                    return (-delta'))
  where
    infts = 0.01 / fromIntegral frequency
