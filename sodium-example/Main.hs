module Main where

import Control.Applicative
import Data.Bool.Extras
import FRP.Sodium
import Graphics.Gloss.Interface.IO.Game hiding (Event)

import Buttons


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

seqE :: Event () -> Event a -> Event a
seqE ignoredEvent keptEvent = filterJust $ merge (eachE ignoredEvent Nothing)
                                                 (Just <$> keptEvent)


main :: IO ()
main = do
    
    
    -- Input
    
    (event0,  fireEvent0)  <- sync newEvent
    (event5,  fireEvent5)  <- sync newEvent
    (event10, fireEvent10) <- sync newEvent
    
    let click0  = filterEq (Just Click) event0
    let click5  = filterEq (Just Click) event5
    let click10 = filterEq (Just Click) event10
    
    let toggle0  = filterEq (Just Toggle) event0
    let toggle5  = filterEq (Just Toggle) event5
    let toggle10 = filterEq (Just Toggle) event10
    
    
    picture <- sync $ do
      
      mode0  <- accum True (eachE toggle0  not)
      mode5  <- accum True (eachE toggle5  not)
      mode10 <- accum True (eachE toggle10 not)
      
      
      -- Scenario 0: generate new graphs and switch to the latest one.
      
      let resetRequest0 = gate toggle0 (not <$> mode0)
      let newCount0 = accum 0 (eachE click0 (+1))
      let graphChange0 = execute (eachE resetRequest0 newCount0)
      
      firstCount0  <- accum 0 (eachE click0  (+1))
      currentGraph0 <- hold firstCount0 graphChange0
      count0 <- switch currentGraph0
      
      
      -- Scenario 5: alternate between two graphs.
      
      (activeClick5, fireActiveClick5) <- newEvent
      (_,            firePassiveClick5) <- newEvent
      
      let activeGraph5 = eachE click5 (fireActiveClick5 ())
      let passiveGraph5 = eachE click5 (firePassiveClick5 ())
      let currentGraph5 = bool passiveGraph5 activeGraph5 <$> mode5
      let graphChange5 = execute (switchE currentGraph5)
      
      -- force graphChange5 to be part of the graph, otherwise its events won't fire
      let realActiveClick5 = seqE graphChange5 activeClick5
      
      count5 <- accum 0 (eachE realActiveClick5 (+1))
      
      
      -- Scenario 10: alternate between two graphs.
      
      activeCount10 <- accum 0 (eachE click10 (+1))
      passiveCount10 <- accum 0 (eachE click10 (+1))
      count10 <- switch (bool activeCount10 passiveCount10 <$> mode10)
      
      
      -- Output
      
      let output0  = if_then_else <$> mode0  <*> count0  <*> pure (-1)
      let output5  = if_then_else <$> mode5  <*> count5  <*> pure (-1)
      let output10 = if_then_else <$> mode10 <*> count10 <*> pure (-1)
      
      return (renderButtons <$> output0 <*> output5 <*> output10)
    
    let render = sync $ sample picture
    
    
    -- Gloss event loop
    
    playIO (InWindow "Sodium Example" (320, 240) (800, 200))
           white
           300
           ()
           (const render)
           (\e () -> sync $ do
               fireEvent0  (filter0  e)
               fireEvent5  (filter5  e)
               fireEvent10 (filter10 e))
           (\_ _ -> return ())
