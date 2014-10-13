{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Arrow

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Wire

import Buttons
import GlossInterface


-- Utility
accumHold :: 
    (Monoid e, Monad m) => 
    a -> Wire s e m (Event (a->a)) a
accumHold x = (hold <|> pure x) . accumE (flip ($)) x


-- Main body
mainWire ::
    Wire (Timed Float ()) () IO (Event G.Event) Picture
mainWire = proc e ->
  do
    click0 <- filterE ((Just Click ==) . filter0) -< e
    click5 <- filterE ((Just Click ==) . filter5) -< e
    click10 <- filterE ((Just Click ==) . filter10) -< e

    toggle0 <- filterE ((Just Toggle ==) . filter0) -< e
    toggle5 <- filterE ((Just Toggle ==) . filter5) -< e
    toggle10 <- filterE ((Just Toggle ==) . filter10) -< e

    mode0 <- accumHold True -< not <$ toggle0
    mode5 <- accumHold True -< not <$ toggle5
    mode10 <- accumHold True -< not <$ toggle10

    -- Part 1: static version

    count0 <- accumHold 0  -< mergeL ((+1) <$ click0) (const 0 <$ toggle0)
    count5 <- accumHold 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- accumHold 0 -< (+1) <$ click10

    let show0 = if mode0 then count0 else -1
    let show5 = if mode5 then count5 else -1
    let show10 = if mode10 then count10 else -1

    -- Part 2: dynamic version

    --   Every toggle event causes switch of counters, 
    --   with every counter is newly created.
    let newCounter0 = if mode0 then counter else arr $ const (-1)
    dynamic0 <- rSwitch counter -< (click0, newCounter0 <$ toggle0)

    --   `modes` can switch multiple state with inactive one suspended.
    --   There's another solution using `kSwitch`. See Yampa-example.
    dynamic5 <- 
        modes True (\case {True -> counter; False -> pure (-1)})
            -< (click5, mode5 <$ toggle5)

    --   <|> operator is suitable for scenario 10.
    dynamic10 <- 
        pure (-1) . unless fst  <|> counter . arr snd
            -< (mode10, click10)

    returnA -< 
        renderButtons 
            show0 (Just dynamic0) 
            show5 (Just dynamic5)
            show10 (Just dynamic10)
  where
    counter = proc e -> accumHold 0 -< (+1) <$ e

main :: IO ()
main =
  do
    playNetwire
        (InWindow "Netwire Example" (320, 240) (800, 200))
        white
        30
        mainWire
