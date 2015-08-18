module GlossInterface where

import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Control.DysFRP


type InputEvent = G.Event

playDysFRP
  :: Display
  -> Color
  -> Int
  -> (Event Float -> Event InputEvent -> BehaviorGen Picture)
  -> IO ()
playDysFRP display color frequency network = do
    (fireTickEvent, tickEvent) <- mkE
    (fireInputEvent, inputEvent) <- mkE
    
    picture <- runBehavior $ network tickEvent inputEvent
    
    G.playIO display
             color
             frequency
             ()
             (\() -> runBehavior picture)
             (\e () -> fireInputEvent e)
             (\e () -> fireTickEvent e)
