module GlossInterface where

import FRP.Euphoria.Event
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G


playEuphoria
  :: Display
  -> Color
  -> Int
  -> (Event Float -> Event G.Event -> SignalGen (Signal Picture))
  -> IO ()
playEuphoria display color frequency network = do
  (tickEvent, fireTickEvent) <- externalEvent
  (inputEvent, fireInputEvent) <- externalEvent
  step <- start $ do
    tickE <- tickEvent
    inputE <- inputEvent
    network tickE inputE
  G.playIO display color frequency ()
    (const step)
    (\input _ -> fireInputEvent input)
    (\tick _ -> fireTickEvent tick)
