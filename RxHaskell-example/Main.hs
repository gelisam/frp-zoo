
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module
    Main
where

import Prelude hiding (filter)
import Data.Functor
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Control.Arrow

import Graphics.Gloss

import Disposable
import Signal
import Signal.Channel (newChannel)
import Signal.Subscriber
import Signal.Operators

import Buttons
import GlossInterface


-- Utility

newAccum ::
    Scheduler s =>
    a -> IO (Subscriber s (a->a), Signal s a)
newAccum i =
  do
    ref <- newIORef i
    (ssc, sigOutput) <- newChannel
    sscOutput <- subscriber $
        \case
            NextEvent f ->
              do
                x <- liftIO $ atomicModifyIORef' ref $ \x -> 
                    let y = f x in (y, y)
                send ssc (NextEvent x)
            ErrorEvent e -> send ssc (ErrorEvent e)
            CompletedEvent -> send ssc CompletedEvent
    return (sscOutput, sigOutput)


leadedBy ::
    Scheduler s =>
    v -> Signal s v -> Signal s v
leadedBy x sig =
    signal $ \ssc ->
      do
        send ssc (NextEvent x)
        subscribe sig ssc

withDisposable ::
    Scheduler s =>
    Disposable -> Signal s v -> Signal s v
withDisposable dp1 sig = signal $ \ssc ->
  do
    dset <- liftIO $ newDisposableSet
    dp2 <- subscribe sig ssc
    liftIO $ addDisposable dset dp1
    liftIO $ addDisposable dset dp2
    liftIO $ toDisposable dset

data FormStatus = FormStatus {
    fmOutput0 :: Int,
    fmDynamic0 :: Maybe Int,
    fmOutput5 :: Int,
    fmDynamic5 :: Maybe Int,
    fmOutput10 :: Int,
    fmDynamic10 :: Maybe Int
}

initFormStatus :: FormStatus
initFormStatus = FormStatus 0 Nothing 0 Nothing 0 Nothing

buttonsFromForm :: FormStatus -> Picture
buttonsFromForm FormStatus{..} =
    renderButtons 
        fmOutput0 fmDynamic0
        fmOutput5 fmDynamic5
        fmOutput10 fmDynamic10

outputFromPair :: (Int, Bool) -> Int
outputFromPair (count, mode) = if mode then count else -1

-- Main
mainRx ::
    Scheduler s =>
    Signal s Float -> Signal s InputEvent -> SchedulerIO s (Signal s Picture)
mainRx _ e =
  do
    -- Cannot decompose state variables into separated signals
    -- like other FRP libraries.
    -- That is because, in RxHaskell, all signals are discrete.
    (fms, fm) <- liftIO $ newAccum initFormStatus

    let
        click0 = filter e ((Just Click ==) . filter0)
        click5 = filter e ((Just Click ==) . filter5)
        click10 = filter e ((Just Click ==) . filter10)

        toggle0 = filter e ((Just Toggle ==) . filter0)
        toggle5 = filter e ((Just Toggle ==) . filter5)
        toggle10 = filter e ((Just Toggle ==) . filter10)

    -- 
    -- Part 1: static implementations
    --
    (pair0s, pair0) <- liftIO $ newAccum (0, True)

    _ <- subscribe (first (+1) <$ click0) pair0s
    _ <- subscribe (first (const 0) <$ toggle0) pair0s
    _ <- subscribe (second not <$ toggle0) pair0s

    _ <- subscribe ((\pair f -> f { fmOutput0 = outputFromPair pair}) <$> pair0) fms


    (pair5s, pair5) <- liftIO $ newAccum (0, True)
    _ <- subscribe ((\(count, mode) -> (if mode then count+1 else count, mode)) <$ click5) pair5s
    _ <- subscribe (second not <$ toggle5) pair5s

    _ <- subscribe ((\pair f -> f { fmOutput5 = outputFromPair pair}) <$> pair5) fms


    (pair10s, pair10) <- liftIO $ newAccum (0, True)
    _ <- subscribe (first (+1) <$ click10) pair10s
    _ <- subscribe (second not <$ toggle10) pair10s

    _ <- subscribe ((\pair f -> f { fmOutput10 = outputFromPair pair}) <$> pair10) fms

    --
    -- Part 2: dynamic implementation
    --
    (mode0s, mode0) <- liftIO $ newAccum True
    _ <- subscribe (not <$ toggle0) mode0s
    let 
        dyn0 = signal $ \ssc -> 
          do
            mode0 >>: \ev ->
              do
                case ev
                  of
                    NextEvent True ->
                      do
                        (count0s, count0) <- liftIO $ newAccum 0
                        dp <- subscribe ((+1) <$ click0) count0s 
                        send ssc $ NextEvent (withDisposable dp $ leadedBy 0 count0)
                    NextEvent False -> send ssc $ NextEvent (leadedBy (-1) never)
                    ErrorEvent expection -> send ssc (ErrorEvent expection)
                    CompletedEvent -> send ssc CompletedEvent
                              
    _ <- subscribe　((\out f -> f { fmDynamic0 = Just out }) <$> switch dyn0) fms
    
    return $ leadedBy (renderButtons 0 Nothing 0 Nothing 0 Nothing) $
        buttonsFromForm <$> fm


main :: IO ()
main = 
    playRx
        (InWindow "RxHaskell Example" (320, 240) (800, 200))
        white
        30
        mainRx

