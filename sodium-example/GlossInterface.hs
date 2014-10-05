module GlossInterface where

import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Sodium


type InputEvent = G.Event

playSodium
  :: Display
  -> Color
  -> Int
  -> (Event Float -> Event InputEvent -> Reactive (Behavior Picture))
  -> IO ()
playSodium display color frequency network = do
    (tickEvent, fireTickEvent) <- sync newEvent
    (inputEvent, fireInputEvent) <- sync newEvent
    
    picture <- sync $ network tickEvent inputEvent
    
    G.playIO display
             color
             frequency
             ()
             (\_ -> sync $ sample picture)
             (\e _ -> sync $ fireInputEvent e)
             (\e _ -> sync $ fireTickEvent e)
