{-# LANGUAGE Arrows #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Control.Monad
import Data.Functor ((<$))

import qualified Control.Arrow.Machine as P
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons
import GlossInterface


mainArrow :: P.ProcessA (Kleisli IO) (Float, P.Event G.Event) Picture
mainArrow = proc (_, e) ->
  do
    let
        click0 = P.filterEvent (Just Click ==) $ filter0 <$> e
        click5 = P.filterEvent (Just Click ==) $ filter5 <$> e
        click10 = P.filterEvent (Just Click ==) $ filter10 <$> e

        toggle0 = P.filterEvent (Just Toggle ==) $ filter0 <$> e
        toggle5 = P.filterEvent (Just Toggle ==) $ filter5 <$> e
        toggle10 = P.filterEvent (Just Toggle ==) $ filter10 <$> e

    mode0 <- P.accum True -< not <$ toggle0
    mode5 <- P.accum True -< not <$ toggle5
    mode10 <- P.accum True -< not <$ toggle10


    -- ---------------------------
    -- First order implementation
    -- ---------------------------

    count0 <- P.accum 0 <<< P.gather -< [(+1) <$ click0, const 0 <$ toggle0]
    count5 <- P.accum 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- P.accum 0 -< (+1) <$ click10

    let
        show0 = if mode0 then count0 else -1
        show5 = if mode5 then count5 else -1
        show10 = if mode10 then count10 else -1


    -- ---------------------------
    -- Higher order implementation
    -- ---------------------------

    -- Every toggle event causes switch of counters, with every counter is newly created.
    show0D <-
        (let
            active _ =
                P.switch (counter *** dropE 1) inactive
            inactive _ =
                P.switch (pure (-1) *** dropE 1) active
          in
            P.switch (counter *** id) inactive)
                -< (click0, () <$ toggle0)

    -- Every toggle event causes switch of a counter, with one counter reused.
    show5D <-
        (let
            test = proc ((_, toggle), _) ->
                returnA -< toggle

            active pa _ =
                P.kSwitch pa (test >>> dropE 1) inactive
            inactive pa _ =
                P.switch (pure (-1) *** dropE 1) (active pa)
          in
            P.kSwitch (arr fst >>> counter) test inactive)
                -< (click5, () <$ toggle5)

    returnA -<
        renderButtons show0 (Just show0D) show5 (Just show5D) show10 Nothing

  where
    counter = proc e ->
        P.accum 0 -< (+1) <$ e

    dropE n = P.construct $
      do
        replicateM_ n P.await
        forever (P.await >>= P.yield)


main :: IO ()
main =
    playMachinecell
        (InWindow "Machinecell Example" (320, 240) (800, 200))
        white
        300
        mainArrow

