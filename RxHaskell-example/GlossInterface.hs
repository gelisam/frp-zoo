{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module GlossInterface where

import Data.IORef
import Control.Monad.IO.Class (liftIO)

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

import Signal
import Signal.Channel (newChannel)
import Signal.Subscriber (send)
import Scheduler

type InputEvent = G.Event

playRx
  :: Display
  -> Color
  -> Int
  -> (forall s. Scheduler s =>
             Signal s Float -> Signal s InputEvent -> SchedulerIO s (Signal s Picture))
  -> IO ()

playRx display color frequency network =
  do
    picRef <- newIORef Blank

    sch <- newScheduler

    (tickSsc, tickSig) <- newChannel
    (inputSsc, inputSig) <- newChannel

    _ <- schedule sch $ 
      do
        picSig <- network tickSig inputSig
        _ <- picSig >>: 
            \case 
                NextEvent pic -> liftIO $ writeIORef picRef pic
                _ -> return ()
        return ()
    
    G.playIO
        display
        color
        frequency
        ()
        (\_ -> readIORef picRef)
        (\e _ -> schedule sch (send inputSsc (NextEvent e)) >> return ())
        (\delta _ -> schedule sch (send tickSsc (NextEvent delta)) >> return ())
