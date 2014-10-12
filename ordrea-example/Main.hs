module Main where
import Control.Applicative
import Data.Monoid

import Graphics.Gloss hiding (pictures)
import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Ordrea

import Buttons
import GlossInterface

-- Utilities

if_then_else :: Bool -> a -> a -> a
if_then_else True  t _ = t
if_then_else False _ f = f

whenE :: TimeFunction s => s Bool -> Event a -> Event a
whenE sig ev = fmap snd $ filterE fst $ (,) <$> sig <@> ev

generatorD :: Discrete (SignalGen a) -> SignalGen (Discrete a)
generatorD dis = do
  ev <- generatorE =<< preservesD dis
  stepperD undefined ev


-- FRP network

mainOrdrea
  :: Event Float
  -> Event G.Event
  -> SignalGen (Behavior Picture)
mainOrdrea _ glossEvent = do
  -- Part 1: static version

  -- Input

  let click0  = filterE ((Just Click ==) . filter0)  glossEvent
      click5  = filterE ((Just Click ==) . filter5)  glossEvent
      click10 = filterE ((Just Click ==) . filter10) glossEvent

      toggle0  = filterE ((Just Toggle ==) . filter0)  glossEvent
      toggle5  = filterE ((Just Toggle ==) . filter5)  glossEvent
      toggle10 = filterE ((Just Toggle ==) . filter10) glossEvent


  -- Behaviour

  mode0  <- scanD True $ not <$ toggle0
  mode5  <- scanD True $ not <$ toggle5
  mode10 <- scanD True $ not <$ toggle10

  count0  <- scanD 0 $ (const 0 <$ toggle0)
                    <> ((+1)    <$ click0)
  count5  <- scanD 0 $ (+1) <$ whenE mode5 click5
  count10 <- scanD 0 $ (+1) <$ click10


  -- Part 2: dynamic version

  let newCount0 :: SignalGen (Discrete Int)
      newCount0 = scanD 0 $ (+1) <$ click0
  newCount <- stepperD newCount0 $ newCount0 <$ toggle0
  dynamicCount0 <- joinDD =<< generatorD newCount
  

  -- Output

  let minus1 = pure (-1)
      output0        = if_then_else <$> mode0  <*> count0        <*> minus1
      dynamicOutput0 = if_then_else <$> mode0  <*> dynamicCount0 <*> minus1
      output5        = if_then_else <$> mode5  <*> count5        <*> minus1
      output10       = if_then_else <$> mode10 <*> count10       <*> minus1

  return $ discreteToBehavior $
    renderButtons <$> output0  <*> (Just <$> dynamicOutput0)
                  <*> output5  <*> pure Nothing
                  <*> output10 <*> pure Nothing


-- Gloss event loop

main :: IO ()
main = playOrdrea
  (InWindow "Ordrea Example" (320, 240) (800, 200))
  white
  30
  mainOrdrea
