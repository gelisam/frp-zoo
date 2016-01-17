{-# LANGUAGE Arrows #-}
module GlossInterface where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Functor ((<$))

import qualified Control.Arrow.Machine as P
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

type MainInput = Either Float G.Event
type MainState = P.ProcessA (Kleisli IO) (P.Event MainInput) (P.Event Picture)


feedEvent ::
    MainInput ->
    (MainState, Picture) ->
    IO (MainState, Picture)
feedEvent x (pa, pic) =
  do
    (P.ExecInfo {P.yields = l}, pa') <-
        runKleisli (P.stepRun pa) x
    return (pa', if null l then pic else last l)


playMachinecell ::
    Display ->
    Color ->
    Int ->
    P.ProcessA (Kleisli IO) (Float, P.Event G.Event) Picture ->
    IO ()
playMachinecell disp c freq network =
    G.playIO
        disp
        c
        freq
        (network', Blank)
        (return . snd)
        (feedEvent . Right)
        (feedEvent . Left)
  where
    network' = proc e ->
      do
        t <- P.accum 0.0 -< (+) <$> P.filterLeft e
        pic <- network -< (t, P.filterRight e)
        returnA -< pic <$ e
