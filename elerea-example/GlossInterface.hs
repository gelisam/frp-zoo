module GlossInterface where

import FRP.Elerea.Simple
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G


type InputEvent = G.Event

playElerea
  :: Display
  -> Color
  -> Int
  -> ( Signal (Maybe Float)
    -> Signal (Maybe InputEvent)
    -> SignalGen (Signal Picture))
  -> IO ()
playElerea display color frequency network = do
    (tickSignal, fireTickSignal) <- external Nothing
    (inputSignal, fireInputSignal) <- external Nothing
    
    recomputePicture <- start (network tickSignal inputSignal)
    initialPicture <- recomputePicture
    
    G.playIO display
             color
             frequency
             initialPicture
             return
             (\e _ -> do
               fireTickSignal Nothing
               fireInputSignal (Just e)
               recomputePicture)
             (\e _ -> do
               fireTickSignal (Just e)
               fireInputSignal Nothing
               recomputePicture)
