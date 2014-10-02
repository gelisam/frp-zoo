{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Control.Applicative

import Graphics.Gloss hiding (pictures)
import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Ordrea

import Buttons
import GlossInterface

-- FRP network

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
  let click0  = filterE ((Just Click ==) . filter0)  glossEvent
      click5  = filterE ((Just Click ==) . filter5)  glossEvent
      click10 = filterE ((Just Click ==) . filter10) glossEvent

      toggle0  = filterE ((Just Toggle ==) . filter0)  glossEvent
      toggle5  = filterE ((Just Toggle ==) . filter5)  glossEvent
      toggle10 = filterE ((Just Toggle ==) . filter10) glossEvent

  mode0  <- scanD True $ not <$ toggle0
  mode5  <- scanD True $ not <$ toggle5
  mode10 <- scanD True $ not <$ toggle10

  let newCount0 :: SignalGen (Discrete Int)
      newCount0 = scanD 0 $ (+1) <$ click0
  newCount <- stepperD newCount0 $ newCount0 <$ toggle0
  count0 <- joinDD =<< generatorD newCount
  count5  <- scanD 0 $ (+1) <$ whenE mode5 click5
  count10 <- scanD 0 $ (+1) <$ click10

  let output0  = if_then_else <$> mode0  <*> count0  <*> pure (-1)
      output5  = if_then_else <$> mode5  <*> count5  <*> pure (-1)
      output10 = if_then_else <$> mode10 <*> count10 <*> pure (-1)

  return $ discreteToBehavior $
    renderButtons <$> output0 <*> output5 <*> output10

-- Gloss event loop

main :: IO ()
main = playOrdrea
  (InWindow "Ordrea Example" (320, 240) (800, 200))
  white
  300
  mainOrdrea
