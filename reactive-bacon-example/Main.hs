{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Bool
import Data.IORef
import Graphics.Gloss hiding (pictures)
import Reactive.Bacon

import Buttons
import GlossInterface


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: EventSource s => a -> s b -> IO (EventStream a)
replaceWith = mapE . const

filterEq :: (Eq a, EventSource s) => a -> s a -> IO (EventStream ())
filterEq x source = do
    filteredStream <- filterE (== x) source
    unitStream <- voidE filteredStream
    return unitStream

scanP :: EventSource s => (b -> a -> b) -> b -> s a -> IO (Property b)
scanP f x0 source = do
    updateStream <- scanE f x0 source
    fromEventSourceWithStartValue (Just x0) updateStream

filterP :: (PropertySource s1, EventSource s2)
        => Bool
        -> s1 Bool
        -> s2 a
        -> IO (EventStream a)
filterP initial propSource eventSource = do
    flagRef <- newIORef initial
    toProperty propSource ==> writeIORef flagRef
    
    return $ sinkMap (go flagRef) eventSource
  where
    go :: IORef Bool -> EventSink a -> EventSink a
    go flagRef sink event = do
        flag <- readIORef flagRef
        if flag
        then sink event
        else return More

switchP :: forall s a b. EventSource s
        => b
        -> Property b
        -> (a -> b)
        -> (a -> IO (Property b))
        -> s a
        -> IO (Property b)
switchP baseInitial baseProperty newInitial newProperty changeRequest = do
    baseChanges    <- changesP baseProperty
    initialChanges <- takeUntilE changeRequest baseChanges
    
    transitionChanges     <- mapE newInitial changeRequest
    postTransitionChanges <- switchE newChanges changeRequest
    laterChanges          <- transitionChanges `mergeE` postTransitionChanges
    
    allChanges <- initialChanges `mergeE` laterChanges
    fromEventSourceWithStartValue (Just baseInitial) allChanges
  where
    newChanges :: a -> IO (EventStream b)
    newChanges x = do
        property <- newProperty x
        changesP property

seqP :: Property a -> Property b -> Property b
seqP = combineWithP seq


-- FRP network

mainBacon :: EventStream Float
          -> EventStream InputEvent
          -> IO (Property Picture)
mainBacon _ glossEvent = do
    -- Part 1: static version

    -- Input
    
    click0  <- filterEq (Just Click) $ filter0  <$> glossEvent
    click5  <- filterEq (Just Click) $ filter5  <$> glossEvent
    click10 <- filterEq (Just Click) $ filter10 <$> glossEvent
    
    toggle0  <- filterEq (Just Toggle) $ filter0  <$> glossEvent
    toggle5  <- filterEq (Just Toggle) $ filter5  <$> glossEvent
    toggle10 <- filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behaviour
  
    mode0  <- scanP (\mode () -> not mode) True toggle0
    mode5  <- scanP (\mode () -> not mode) True toggle5
    mode10 <- scanP (\mode () -> not mode) True toggle10
    
    let count = scanP (\n () -> n + 1) 0
    count0 <- do
      reset     <- replaceWith (const 0) toggle0
      increment <- replaceWith (+1) click0
      update    <- mergeE reset increment
      scanP (\n f -> f n) 0 update
    count5 <- do
      filteredClick5 <- filterP True mode5 click5
      count filteredClick5
    count10 <- count click10
    
    
    -- Part 2: dynamic version
    
    let makeDynamicCounter0 = do
            firstCounter <- newCounter
            resetRequest <- filterP False (not <$> mode0) toggle0
            switchP 0 firstCounter
                    (const 0) (const newCounter)
                    resetRequest
          where
            newCounter :: IO (Property Int)
            newCounter = count click0
    dynamicCount0 <- makeDynamicCounter0
    
    let makeDynamicCounter5 = do
            (passiveCounterA, activeEventA) <- newCounter "A"
            (passiveCounterB, activeEventB) <- newCounter "B"
            let passiveCounter = if_then_else <$> mode5 <*> passiveCounterA <*> passiveCounterB
            
            toggleRequest <- changesP mode5
            baseActiveEvents <- takeUntilE toggleRequest activeEventA
            laterActiveEvents <- switchE (return . bool activeEventB activeEventA) toggleRequest
            allActiveEvents <- baseActiveEvents `mergeE` laterActiveEvents
            
            activeProperty <- fromEventSourceWithStartValue (Just ()) allActiveEvents
            let activatedCounter = activeProperty `seqP` passiveCounter
            
            return activatedCounter
          where
            newCounter :: String -> IO (Property Int, EventStream ())
            newCounter name = do
                (localEvent, pushLocalEvent) <- newPushStream
                
                passiveCounter <- count localEvent
                
                let triggerIfActive () = do
                      pushLocalEvent (Next ())
                      return neverE
                activeEvent <- switchE triggerIfActive click5
                
                return (passiveCounter, activeEvent)
    dynamicCount5 <- makeDynamicCounter5
    
    
    -- Output
    
    let minus1 = pure (-1)
    let output0        = if_then_else <$> mode0  <*> count0        <*> minus1
    let dynamicOutput0 = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
    let output5        = if_then_else <$> mode5  <*> count5        <*> minus1
    let dynamicOutput5 = if_then_else <$> mode5  <*> dynamicCount5 <*> minus1
    let output10       = if_then_else <$> mode10 <*> count10       <*> minus1
    
    return $ renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                           <*> output5  <*> (Just <$> dynamicOutput5)
                           <*> output10 <*> pure Nothing

    
-- Gloss event loop

main :: IO ()
main = playBacon
            (InWindow "Reactive-Bacon Example" (320, 240) (800, 200))
            white
            30
            mainBacon
