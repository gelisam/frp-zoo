{-# LANGUAGE Arrows, RankNTypes #-}
module GlossInterface where

import Data.IORef
import FRP.Grapefruit.Circuit
import FRP.Grapefruit.Setup
import FRP.Grapefruit.Signal
import FRP.Grapefruit.Signal.Discrete as D
import FRP.Grapefruit.Signal.Segmented as S
import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G

import CallbackSystem


setupCallback :: Hook a -> Callback a -> Setup
setupCallback hook callback = setup $ do
    registerCallback hook callback
    return (unregisterCallbacks hook)


type InputEvent = G.Event

playGrapefruit
  :: Display
  -> Color
  -> Int
  -> (forall era. DSignal era Float
               -> DSignal era InputEvent
               -> SSignal era Picture)
  -> IO ()
playGrapefruit display color frequency network = do
    -- Place in which to store the output signal
    
    pictureRef  <- newIORef undefined
    
    
    -- Places where callbacks can be registered
    
    onTick  <- newHook
    onInput <- newHook
    
    
    -- Link everything together
    
    let closedCircuit = closeCircuit pictureRef onTick onInput
    ((), finalize) <- create closedCircuit ()
    
    
    -- Gloss event loop
    
    G.playIO display
             color
             frequency
             ()
             (\() -> readIORef pictureRef)
             (\e () -> triggerCallbacks onInput e)
             (\e () -> triggerCallbacks onTick e)
    
    finalize
  where
    closeCircuit :: IORef Picture
                 -> Hook Float
                 -> Hook InputEvent
                 -> Circuit era () ()
    closeCircuit pictureRef onTick onInput = proc () -> do
        tick <- produce tickProducer -< ()
        input <- produce inputProducer -< ()
        let picture = network tick input
        consume pictureConsumer -< picture
      where
        tickProducer :: Producer DSignal Float
        tickProducer = D.producer (setupCallback onTick)
    
        inputProducer :: Producer DSignal InputEvent
        inputProducer = D.producer (setupCallback onInput)
        
        pictureConsumer :: Consumer SSignal Picture
        pictureConsumer = S.consumer (writeIORef pictureRef)
