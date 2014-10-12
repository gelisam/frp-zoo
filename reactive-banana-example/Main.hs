{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.Gloss hiding (pictures)
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Reactive.Banana
import Reactive.Banana.Frameworks

import Buttons


-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

replaceWith :: a -> Event t b -> Event t a
replaceWith = fmap . const

filterEq :: Eq a => a -> Event t a -> Event t ()
filterEq x = replaceWith () . filterE (== x)

eachE :: Event t () -> a -> Event t a
eachE = flip replaceWith


-- FRP network

mainBanana :: forall t. Frameworks t
           => Event t Float
           -> Event t InputEvent
           -> Moment t (Behavior t Picture)
mainBanana _ glossEvent = do
    -- used in the dynamic version.
    resetClick0 <- trimE click0
    go resetClick0
  where
    -- Part 1: static version
    
    -- Input
    
    click0, click5, click10 :: Event t ()
    click0  = filterEq (Just Click) $ filter0  <$> glossEvent
    click5  = filterEq (Just Click) $ filter5  <$> glossEvent
    click10 = filterEq (Just Click) $ filter10 <$> glossEvent
    
    toggle0, toggle5, toggle10 :: Event t ()
    toggle0  = filterEq (Just Toggle) $ filter0  <$> glossEvent
    toggle5  = filterEq (Just Toggle) $ filter5  <$> glossEvent
    toggle10 = filterEq (Just Toggle) $ filter10 <$> glossEvent
    
    
    -- Behaviour
    
    mode0, mode5, mode10 :: Behavior t Bool
    mode0  = accumB True (eachE toggle0 not)
    mode5  = accumB True (eachE toggle5  not)
    mode10 = accumB True (eachE toggle10 not)
    
    count0, count5, count10 :: Behavior t Int
    count0  = accumB 0 $ eachE toggle0 (const 0)
                 `union` eachE click0 (+1)
    count5  = accumB 0 $ whenE mode5
                       $ eachE click5 (+1)
    count10 = accumB 0 $ eachE click10 (+1)
    
    
    -- Part 2: dynamic version
    -- (in a separate function due to scoping constraints)
    
    go :: AnyMoment Event () -> Moment t (Behavior t Picture)
    go resetClick0 = return picture
      where
        resetRequest :: Event t ()
        resetRequest = whenE (not <$> mode0) toggle0
        
        newCount0 :: AnyMoment Behavior Int
        newCount0 = anyMoment $ do
            recentClick0 <- now resetClick0
            return $ accumB 0 (eachE recentClick0 (+1))
        
        dynamicCount0 :: Behavior t Int
        dynamicCount0 = switchB count0 (eachE resetRequest newCount0)
        
        
        -- Output
        
        minus1 :: Behavior t Int
        minus1 = pure (-1)
        
        output0, dynamicOutput0, output5, output10 :: Behavior t Int
        output0        = if_then_else <$> mode0  <*> count0        <*> minus1
        dynamicOutput0 = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
        output5        = if_then_else <$> mode5  <*> count5        <*> minus1
        output10       = if_then_else <$> mode10 <*> count10       <*> minus1
        
        picture :: Behavior t Picture
        picture = renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                                <*> output5  <*> pure Nothing
                                <*> output10 <*> pure Nothing


-- Gloss event loop

main :: IO ()
main = playBanana
            (InWindow "Reactive-Banana Example" (320, 240) (800, 200))
            white
            30
            mainBanana
