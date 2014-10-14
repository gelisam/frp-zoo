{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Main where

import Control.Applicative
import FRP.Grapefruit.Circuit
import FRP.Grapefruit.Setup
import FRP.Grapefruit.Signal
import FRP.Grapefruit.Signal.Discrete as D
import FRP.Grapefruit.Signal.Segmented as S
import Graphics.Gloss.Interface.IO.Game hiding (Event)

import Buttons
import GlossInterface


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: a -> DSignal era b -> DSignal era a
replaceWith = D.map . const

filterEq :: Eq a => a -> DSignal era a -> DSignal era ()
filterEq x = replaceWith () . D.filter (== x)

eachD :: DSignal era () -> a -> DSignal era a
eachD = flip replaceWith


-- FRP network

mainGrapefruit :: forall era. DSignal era Float
                           -> DSignal era InputEvent
                           -> SSignal era Picture
mainGrapefruit _ glossEvent = picture
  where
    -- Part 1: static version
    
    -- Input
    
    click0, click5, click10 :: DSignal era ()
    click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    toggle0, toggle5, toggle10 :: DSignal era ()
    toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behaviour
    
    mode0, mode5, mode10 :: SSignal era Bool
    mode0  = S.scan True (\mode () -> not mode) toggle0
    mode5  = S.scan True (\mode () -> not mode) toggle5
    mode10 = S.scan True (\mode () -> not mode) toggle10
    
    count0 :: SSignal era Int
    count0 = construct 0 updates0
      where
        modify0 :: DSignal era (Int -> Int)
        modify0 = eachD toggle0 (const 0)
          `union` eachD click0 (+1)
        
        updateState0 :: DSignal era (Int -> (Int, Int))
        updateState0 = fmap (dup .) modify0
          where
            dup x = (x,x)
        
        updates0 :: DSignal era Int
        updates0 = stateful 0 updateState0
    
    count5 :: SSignal era Int
    count5  = S.scan 0 (\count () -> count + 1) filteredClick5
      where
        annotatedClick5 :: DSignal era Bool
        annotatedClick5 = click5 #> mode5
        
        filteredClick5 :: DSignal era ()
        filteredClick5 = replaceWith () (D.filter id annotatedClick5)
    
    count10 :: SSignal era Int
    count10 = S.scan 0 (\count () -> count + 1) click10
    
    
    -- Part 2: dynamic version
    
    dynamicCount0 :: SSignal era Int
    dynamicCount0 = unOSF $ currentCount `sfApp` click0
      where
        newCounter :: PolySignalFun (DSignal `Of` ()
                                 :-> SSignal `Of` Int)
        newCounter = PolySignalFun
                   $ SSF $ \click ->
                     OSF $ S.scan 0 (\count () -> count + 1) click
        
        resetCounter :: DSignal era (PolySignalFun (DSignal `Of` ()
                                                :-> SSignal `Of` Int))
        resetCounter = eachD toggle0 newCounter
        
        currentCounter :: SSignal era (PolySignalFun (DSignal `Of` ()
                                                  :-> SSignal `Of` Int))
        currentCounter = construct newCounter resetCounter
        
        currentCount :: SignalFun era (DSignal `Of` ()
                                   :-> SSignal `Of` Int)
        currentCount = polySwitch currentCounter
    
    
    -- Output
    
    minus1 :: SSignal era Int
    minus1 = pure (-1)
    
    output0, dynamicOutput0, output5, output10 :: SSignal era Int
    output0        = if_then_else <$> mode0  <*> count0         <*> minus1
    dynamicOutput0 = if_then_else <$> mode0  <*> dynamicCount0  <*> minus1
    output5        = if_then_else <$> mode5  <*> count5         <*> minus1
    output10       = if_then_else <$> mode10 <*> count10        <*> minus1
    
    picture :: SSignal era Picture
    picture = renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                            <*> output5  <*> pure Nothing
                            <*> output10 <*> pure Nothing


-- Gloss event loop

main :: IO ()
main = playGrapefruit (InWindow "Grapefruit Example" (320, 240) (800, 200))
                      white
                      30
                      mainGrapefruit
