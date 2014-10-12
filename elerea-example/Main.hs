{-# LANGUAGE LambdaCase, RecursiveDo #-}
module Main where

import Control.Applicative
import FRP.Elerea.Simple
import Graphics.Gloss.Interface.IO.Game hiding (Event)

import Buttons
import GlossInterface


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

fmapMaybe :: (a -> Maybe b) -> Signal (Maybe a) -> Signal (Maybe b)
fmapMaybe f = fmap (>>= f)

whenEq :: Eq a => a -> Signal a -> Signal Bool
whenEq x = fmap (== x)

andAlso :: Signal Bool -> Signal Bool -> Signal Bool
andAlso = liftA2 (&&)

pair :: Signal a -> Signal b -> Signal (a,b)
pair = liftA2 (,)

updateWhen :: a -> Signal Bool -> (a -> SignalGen a) -> SignalGen (Signal a)
updateWhen initialValue updateSignal computeNextValue = mdo
  prev <- delay initialValue curr
  curr <- generator $ pure $ do
    prevValue <- snapshot prev
    shouldUpdate <- snapshot updateSignal
    if shouldUpdate
    then computeNextValue prevValue
    else return prevValue
  return curr

replaceWhen :: a -> Signal Bool -> SignalGen a -> SignalGen (Signal a)
replaceWhen initialValue updateSignal computeNextValue
  = updateWhen initialValue updateSignal (const computeNextValue)

-- a variant of
-- 
-- > withClock updateSignal (stateful initialValue computeNextValue)
-- 
-- where the newly-computed value is available during the tick in which
-- updateSignal is True instead of one tick afterwards.
modifyWhen :: a -> Signal Bool -> (a -> a) -> SignalGen (Signal a)
modifyWhen initialValue updateSignal computeNextValue
  = updateWhen initialValue updateSignal (return . computeNextValue)

switch :: Signal (Signal a) -> SignalGen (Signal a)
switch = generator . fmap snapshot

seqSignal :: Signal a -> Signal b -> Signal b
seqSignal = liftA2 seq


type ActiveSignal a = Signal (SignalGen a)

activeSnapshot :: ActiveSignal a -> SignalGen a
activeSnapshot activeSignal = do
    currentAction <- snapshot activeSignal
    currentValue <- currentAction
    return currentValue

activeWithClock :: a -> Signal Bool -> ActiveSignal a -> SignalGen (Signal a)
activeWithClock initialValue clock tick
  = replaceWhen initialValue clock (activeSnapshot tick)


-- FRP network

mainElerea :: Signal (Maybe Float)
           -> Signal (Maybe InputEvent)
           -> SignalGen (Signal Picture)
mainElerea _ glossEvent = do
    -- Part 1: static version

    -- Input
    
    let click0  = whenEq (Just Click) $ fmapMaybe filter0  glossEvent
    let click5  = whenEq (Just Click) $ fmapMaybe filter5  glossEvent
    let click10 = whenEq (Just Click) $ fmapMaybe filter10 glossEvent
    
    let toggle0  = whenEq (Just Toggle) $ fmapMaybe filter0  glossEvent
    let toggle5  = whenEq (Just Toggle) $ fmapMaybe filter5  glossEvent
    let toggle10 = whenEq (Just Toggle) $ fmapMaybe filter10 glossEvent
    
    
    -- Behaviour
    
    mode0  <- modifyWhen True toggle0  not
    mode5  <- modifyWhen True toggle5  not
    mode10 <- modifyWhen True toggle10 not
    
    count0  <- transfer 0 (\case (True, False) -> const 0
                                 (False, True) -> (+1)
                                 _             -> id)
                          (pair toggle0 click0)
    count5  <- modifyWhen 0 (mode5 `andAlso` click5) (+1)
    count10 <- modifyWhen 0 click10 (+1)
    
    
    -- Part 2: dynamic version
    
    -- Scenario 0: generate new graphs and switch to the latest one.
    let makeDynamicCounter0 = do
            initialCounter <- newCounter
            currentCounter <- replaceWhen initialCounter
                                          resetRequest
                                          newCounter
            switch currentCounter
          where
            resetRequest :: Signal Bool
            resetRequest = toggle0 `andAlso` mode0
            
            newCounter :: SignalGen (Signal Int)
            newCounter = modifyWhen 0 click0 (+1)
    dynamicCount0 <- makeDynamicCounter0
    
    -- Scenario 5: alternate between two active graphs.
    let makeDynamicCounter5 = do
            (passiveCounterA, activeCounterA) <- newCounter
            (passiveCounterB, activeCounterB) <- newCounter
            
            let currentPassiveCounter = if_then_else <$> mode5
                                                     <*> passiveCounterA
                                                     <*> passiveCounterB
            let currentActiveCounter = if_then_else <$> mode5
                                                    <*> activeCounterA
                                                    <*> activeCounterB
            
            lastActiveValue <- activeWithClock 0 click5 currentActiveCounter
            let currentValue = if_then_else <$> mode5
                                            <*> passiveCounterA
                                            <*> passiveCounterB
            
            return (seqSignal lastActiveValue currentValue)
          where
            newCounter :: SignalGen (Signal Int, ActiveSignal Int)
            newCounter = do
              (count, setCount) <- execute $ external 0
              let activeCounter = pure $ do
                    n <- snapshot count
                    let n' = n + 1
                    execute $ setCount n'
                    return n'
              return (count, activeCounter)
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
main = playElerea (InWindow "Elerea Example" (320, 240) (800, 200))
                  white
                  30
                  mainElerea
