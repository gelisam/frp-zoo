{-# LANGUAGE LambdaCase, RecursiveDo #-}
module Main where

import Control.Applicative
import FRP.Elerea.Clocked
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

switch :: Signal (Signal a) -> SignalGen (Signal a)
switch = generator . fmap snapshot


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
    
    let alternate = stateful True not
    mode0  <- withClock toggle0  alternate
    mode5  <- withClock toggle5  alternate
    mode10 <- withClock toggle10 alternate
    
    count0  <- transfer 0 (\case (True, False) -> const 0
                                 (False, True) -> (+1)
                                 _             -> id)
                          (pair toggle0 click0)
    count5  <- withClock (mode5 `andAlso` click5)
             $ stateful 0 (+1)
    count10 <- withClock click10
             $ stateful 0 (+1)
    
    
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
            resetRequest = toggle0 `andAlso` (not <$> mode0)
            
            newCounter :: SignalGen (Signal Int)
            newCounter = withClock click0 $ stateful 0 (+1)
    dynamicCount0 <- makeDynamicCounter0
    
    -- Scenario 5: alternate between two active graphs.
    let makeDynamicCounter5 = do
            counterA <- newCounter
            counterB <- newCounter
            let currentCounter = if_then_else <$> mode5
                                              <*> counterA
                                              <*> counterB
            activeWithClock 0 click5 currentCounter
          where
            newCounter :: SignalGen (ActiveSignal Int)
            newCounter = do
              (count, setCount) <- execute $ external 0
              return $ pure $ do
                n <- snapshot count
                let n' = n + 1
                execute $ setCount n'
                return n'
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
