module GlossInterface where

import Data.IORef
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Bacon


type InputEvent = G.Event

playBacon
  :: Display
  -> Color
  -> Int
  -> (EventStream Float -> EventStream InputEvent -> IO (Property Picture))
  -> IO ()
playBacon display color frequency network = do
    (tickStream, pushTick) <- newPushStream
    (inputStream, pushInput) <- newPushStream
    
    pictureRef <- newIORef G.Blank
    pictureProp <- network tickStream inputStream
    pictureProp ==> writeIORef pictureRef
    
    G.playIO display
             color
             frequency
             ()
             (\() -> readIORef pictureRef)
             (\e () -> pushInput (Next e))
             (\e () -> pushTick (Next e))
