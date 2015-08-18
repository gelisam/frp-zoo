module Main where

import Control.Applicative
import Control.DysFRP
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


-- FRP network

mainDysFRP :: Event Float
           -> Event InputEvent
           -> BehaviorGen Picture
mainDysFRP _ glossEvent = do
    -- Part 1: static version
    --
    mode0   <- accumB True $ constE not toggle0
    mode5   <- accumB True $ constE not toggle5
    mode10  <- accumB True $ constE not toggle10

    count0  <- accumB 0 $ constE (+1) click0 `appendE` constE (const 0) toggle0
    count5  <- accumB 0 $ whenE mode5 $ constE (+1) click5
    count10 <- accumB 0 $ constE (+1) click10

    let output0        = if_then_else <$> mode0  <*> count0        <*> minus1
    let output5        = if_then_else <$> mode5  <*> count5        <*> minus1
    let output10       = if_then_else <$> mode10 <*> count10       <*> minus1
    
    let counterB click = accumB 0 $ constE (+1) click

    -- Part 2: dynamic version
    -- scenario 0: new counter created each time
    initCount0 <- counterB click0
    dynamicCount0 <- switchB initCount0 $ 
        genToE (\b -> if b then -1 else counterB click0) $
	snapshotE mode0 $ toggle0
    
    -- scenario 10: re-using the first counter
    initCount10 <- counterB click10
    dynamicCount10 <- switchB initCount10 $ 
        fmap (\b -> if b then -1 else initCount10) $
	snapshotE mode10 $ toggle10

    return $ renderButtons <$> output0  <*> (Just <$> dynamicCount0)
                           <*> output5  <*> pure Nothing
                           <*> output10 <*> (Just <$> dynamicCount10)
  where
    minus1 = constB (-1)
    
    -- Input
    
    click0, click5, click10 :: Event ()
    click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    toggle0, toggle5, toggle10 :: Event ()
    toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
-- Gloss event loop

main :: IO ()
main = playDysFRP
            (InWindow "DysFRP Example" (320, 240) (800, 200))
            white
            30
            mainDysFRP
