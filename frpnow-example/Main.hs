module Main where

import Control.Applicative
import Control.Monad (join)
import Data.Monoid
import Control.FRPNow
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Control.FRPNow.Gloss

import Buttons


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: a -> EvStream b -> EvStream a
replaceWith = fmap . const

filterEq :: Eq a => a -> EvStream a -> EvStream ()
filterEq x = replaceWith () . filterEs (== x)

eachEs :: EvStream () -> a -> EvStream a
eachEs = flip replaceWith

-- FRP network

mainFRPNow :: Behavior Time
           -> EvStream GEvent
           -> Behavior (Behavior Picture)
mainFRPNow _ glossEvent = do
    -- Part 1: static version

    -- Input
    
    let click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    let click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    let click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    let toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    let toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    let toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behavior
  
    mode0  <- foldEs (\x _ -> not x) True toggle0
    mode5  <- foldEs (\x _ -> not x) True toggle5
    mode10 <- foldEs (\x _ -> not x) True toggle10
    
    count0  <- foldEs (\x f -> f x) 0 $ eachEs toggle0 (const 0)
                      <> eachEs click0  (+1)
    count5  <- foldEs (\x _ -> x + 1) 0 $ click5 `during` mode5
    count10 <- foldEs (\x _ -> x + 1) 0 $ click10
    
    -- Part 2: dynamic version
    
    -- Scenario 0: generate new graphs and switch to the latest one.
    let makeDynamicCounter0 = do
            firstCounter <- newCounter
            currentCounter <-
                fromChanges firstCounter (newCounter `snapshots` resetRequest)
            return $ join currentCounter
          where
            resetRequest :: EvStream ()
            resetRequest = toggle0 `during` mode0
            
            newCounter :: Behavior (Behavior Int)
            newCounter = foldEs (\x _ -> x + 1) 0 click0
    dynamicCount0 <- makeDynamicCounter0

    -- Output
    
    let minus1 = pure (-1)::Behavior Int
    let output0         = if_then_else <$> mode0  <*> count0  <*> minus1
    let output5         = if_then_else <$> mode5  <*> count5  <*> minus1
    let output10        = if_then_else <$> mode10 <*> count10 <*> minus1
    let dynamicOutput0  = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
    
    return $ renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                           <*> output5  <*> pure Nothing
                           <*> output10 <*> pure Nothing

    
-- Gloss event loop

main :: IO ()
main = runNowGlossPure (InWindow "FRPNow Example" (320, 240) (800, 200))
                  white
                  30
                  mainFRPNow
