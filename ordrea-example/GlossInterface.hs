module GlossInterface where

import FRP.Ordrea
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G


playOrdrea
  :: Display
  -> Color
  -> Int
  -> (Event Float -> Event G.Event -> SignalGen (Behavior Picture))
  -> IO ()
playOrdrea display color frequency network = do
  tickEvent <- newExternalEvent
  inputEvent <- newExternalEvent
  step <- start $ do
    tickE <- externalE tickEvent
    inputE <- externalE inputEvent
    network tickE inputE
  G.playIO display color frequency ()
    (const step)
    (\input _ -> triggerExternalEvent inputEvent input)
    (\tick _ -> triggerExternalEvent tickEvent tick)
