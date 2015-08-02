module Main where

import Control.Applicative
import Control.DysFrp
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
           -> Behavior Picture
mainDysFRP _ glossEvent = picture
  where
    -- Part 1: static version
    
    -- Input
    
    click0, click5, click10 :: Event ()
    click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    toggle0, toggle5, toggle10 :: Event ()
    toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behaviour
    
    mode0, mode5, mode10 :: Behavior Bool
    mode0  = accumB True (eachE toggle0 not)
    mode5  = accumB True (eachE toggle5  not)
    mode10 = accumB True (eachE toggle10 not)
    
    count0, count5, count10 :: Behavior Int
    count0  = accumB 0 $ eachE toggle0 (const 0)
                 `union` eachE click0 (+1)
    count5  = accumB 0 $ whenE mode5
                       $ eachE click5 (+1)
    count10 = accumB 0 $ eachE click10 (+1)
    
    
    -- -- Part 2: dynamic version
    -- -- (in a separate function due to scoping constraints)
    -- 
    -- go :: AnyMoment Event () -> Moment (Behavior Picture)
    -- go resetClick0 = return picture
    --   where
    --     resetRequest :: Event ()
    --     resetRequest = whenE (not <$> mode0) toggle0
    --     
    --     newCount0 :: AnyMoment Behavior Int
    --     newCount0 = anyMoment $ do
    --         recentClick0 <- now resetClick0
    --         return $ accumB 0 (eachE recentClick0 (+1))
    --     
    --     dynamicCount0 :: Behavior Int
    --     dynamicCount0 = switchB count0 (eachE resetRequest newCount0)
        
        
    -- Output
    
    minus1 :: Behavior Int
    minus1 = pure (-1)
    
    output0, dynamicOutput0, output5, output10 :: Behavior Int
    output0        = if_then_else <$> mode0  <*> count0        <*> minus1
    dynamicOutput0 = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
    output5        = if_then_else <$> mode5  <*> count5        <*> minus1
    output10       = if_then_else <$> mode10 <*> count10       <*> minus1
    
    picture :: Behavior Picture
    picture = renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                            <*> output5  <*> pure Nothing
                            <*> output10 <*> pure Nothing


-- Gloss event loop

main :: IO ()
main = playDysFRP
            (InWindow "DysFRP Example" (320, 240) (800, 200))
            white
            30
            mainDysFRP
