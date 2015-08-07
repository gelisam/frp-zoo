{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Applicative
import Control.Category
import Control.Arrow
import Data.Maybe

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Artery
       
import Buttons
import GlossInterface



filterA :: Monad m => (a -> Bool) -> Artery m (Maybe a) (Maybe a)
filterA cond = arr $ \mx -> do {x <- mx; if cond x then Just x else Nothing}

scanMaybe :: Monad m => (i -> a -> a) -> a -> Artery m (Maybe i) a
scanMaybe f a0 = scan (\mi a -> maybe a (f `flip` a) mi) a0

drSwitch :: Monad m => Artery m a b -> Artery m (a, Maybe (Artery m a b)) b
drSwitch ar0 = Artery $ step ar0
  where
    step ar (x, mNext) r =
      do
        (y, ar') <- runArtery ar x
        let next = drSwitch (fromMaybe ar' mNext)
        r y next

dkSwitch ::
    Monad m =>
    Artery m a b ->
    Artery m (a, b) (Maybe t) ->
    (Artery m a b -> t -> Artery m a b) ->
    Artery m a b
dkSwitch ar0 test0 fNext = Artery $ step ar0 test0
  where
    step ar test x r =
      do
        (y, ar') <- runArtery ar x
        (mt, test') <- runArtery test (x, y)
        let next = maybe (dkSwitch ar' test' fNext) (fNext ar') mt
        r y next

-- Main body
mainArtery ::
    Artery IO (Float, Maybe G.Event) Picture
mainArtery = proc (_, e) ->
  do
    click0 <- filterA ((Just Click ==) . filter0) -< e
    click5 <- filterA ((Just Click ==) . filter5) -< e
    click10 <- filterA ((Just Click ==) . filter10) -< e

    toggle0 <- filterA ((Just Toggle ==) . filter0) -< e
    toggle5 <- filterA ((Just Toggle ==) . filter5) -< e
    toggle10 <- filterA ((Just Toggle ==) . filter10) -< e

    mode0 <- scanMaybe (\_ b -> not b) True -< toggle0
    mode5 <- scanMaybe (\_ b -> not b) True -< toggle5
    mode10 <- scanMaybe (\_ b -> not b) True -< toggle10

    -- Part 1: static version

    count0 <- scan ($) 0  -<
        foldr (.) id $ catMaybes [(+1) <$ click0, (const 0) <$ toggle0]
    count5 <- scanMaybe ($) 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- scanMaybe ($) 0 -< (+1) <$ click10

    let show0 = if mode0 then count0 else -1
    let show5 = if mode5 then count5 else -1
    let show10 = if mode10 then count10 else -1


    -- Part 2: dynamic version

    --   Every toggle event causes switch of counters, 
    --   with every counter is newly created.
    let newCounter0 = if not mode0 then counter else pure (-1)
    dynamic0 <- drSwitch counter -< (click0, newCounter0 <$ toggle0)

    -- Every toggle event causes switch of a counter, 
    -- with one counter reused.
    dynamic5 <- 
        (let
            test = arr $ \((_, t), _) -> t
            active pa _ =  dkSwitch pa test inactive
            inactive pa _ = dkSwitch (pure (-1)) test (\_ -> active pa)
          in
            active (arr fst >>> counter) ())
        -< (click5, () <$ toggle5)


    returnA -< 
        renderButtons 
            show0 (Just dynamic0) 
            show5 (Just dynamic5)
            show10 Nothing
  where
    counter :: Artery IO (Maybe a) Int
    counter = proc e -> scanMaybe ($) 0 -< (+1) <$ e

main :: IO ()
main =
  do
    playArtery
        (InWindow "Artery Example" (320, 240) (800, 200))
        white
        30
        mainArtery
