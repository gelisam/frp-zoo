module Main where

import Control.Applicative
import Data.Bool
import Data.Monoid
import FRP.Sodium
import Graphics.Gloss.Interface.IO.Game hiding (Event)

import Buttons
import GlossInterface


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: a -> Event b -> Event a
replaceWith = fmap . const

filterEq :: Eq a => a -> Event a -> Event ()
filterEq x = replaceWith () . filterE (== x)

eachE :: Event () -> a -> Event a
eachE = flip replaceWith

executeB :: Behaviour a
         -> Event (Reactive (Behaviour a))
         -> Reactive (Behaviour a)
executeB initialBehaviour changeBehaviour = do
    currentBehaviour <- hold initialBehaviour (execute changeBehaviour)
    switch currentBehaviour


-- FRP network

mainSodium :: Event Float
           -> Event InputEvent
           -> Reactive (Behaviour Picture)
mainSodium _ glossEvent = do
    -- Part 1: static version

    -- Input
    
    let click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    let click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    let click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    let toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    let toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    let toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behaviour
  
    mode0  <- accum True (eachE toggle0  not)
    mode5  <- accum True (eachE toggle5  not)
    mode10 <- accum True (eachE toggle10 not)
    
    count0  <- accum 0 $ eachE toggle0 (const 0)
                      <> eachE click0  (+1)
    count5  <- accum 0 $ eachE (gate click5 mode5) (+1)
    count10 <- accum 0 $ eachE click10 (+1)
    
    
    -- Part 2: dynamic version
    
    -- Scenario 0: generate new graphs and switch to the latest one.
    let makeDynamicCounter0 = do
            firstCounter <- newCounter
            currentCounter <- hold firstCounter counterChange
            switch currentCounter
          where
            resetRequest :: Event ()
            resetRequest = gate toggle0 (not <$> mode0)
            
            newCounter :: Reactive (Behaviour Int)
            newCounter = accum 0 (eachE click0 (+1))
            
            counterChange :: Event (Behaviour Int)
            counterChange = execute (eachE resetRequest newCounter)
    dynamicCount0 <- makeDynamicCounter0
    
    -- Scenario 5: alternate between two active graphs.
    let makeDynamicCounter5 = do
            activeCounterA <- eachE click5 <$> newActiveCounter
            activeCounterB <- eachE click5 <$> newActiveCounter
            
            let activeCounter = bool activeCounterA activeCounterB <$> mode5
            
            executeB (pure 0) (switchE activeCounter)
          where
            newActiveCounter :: Reactive (Reactive (Behaviour Int))
            newActiveCounter = do
              (localEvent, fireLocalEvent) <- newEvent
              counter <- accum 0 (eachE localEvent (+1))
              return $ do
                fireLocalEvent ()
                return counter
    dynamicCount5 <- makeDynamicCounter5
    
    
    -- Output
    
    let minus1 = pure (-1)
    let output0         = if_then_else <$> mode0  <*> count0  <*> minus1
    let output5         = if_then_else <$> mode5  <*> count5  <*> minus1
    let output10        = if_then_else <$> mode10 <*> count10 <*> minus1
    let dynamicOutput0  = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
    let dynamicOutput5  = if_then_else <$> mode5  <*> dynamicCount5 <*> minus1
    
    return $ renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                           <*> output5  <*> (Just <$> dynamicOutput5)
                           <*> output10 <*> pure Nothing

    
-- Gloss event loop

main :: IO ()
main = playSodium (InWindow "Sodium Example" (320, 240) (800, 200))
                  white
                  30
                  mainSodium
