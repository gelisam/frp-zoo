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

import Control.Auto
import Control.Auto.Blip
import Control.Auto.Switch
import Control.Auto.Interval
       
import Buttons
import GlossInterface



-- Main body
mainAuto ::
    Auto IO (Float, Blip G.Event) Picture
mainAuto = proc (_, e) ->
  do
    click0 <- filterB ((Just Click ==) . filter0) -< e
    click5 <- filterB ((Just Click ==) . filter5) -< e
    click10 <- filterB ((Just Click ==) . filter10) -< e

    toggle0 <- filterB ((Just Toggle ==) . filter0) -< e
    toggle5 <- filterB ((Just Toggle ==) . filter5) -< e
    toggle10 <- filterB ((Just Toggle ==) . filter10) -< e

    mode0 <- scanB (\b _ -> not b) True -< toggle0
    mode5 <- scanB (\b _ -> not b) True -< toggle5
    mode10 <- scanB (\b _ -> not b) True -< toggle10

    -- Part 1: static version

    count0 <- scanB (\x f -> f x) 0  -< merge (.) ((+1) <$ click0) (const 0 <$ toggle0)
    count5 <- scanB (\x f -> f x) 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- scanB (\x _ -> x + 1) 0 -< click10

    let show0 = if mode0 then count0 else -1
    let show5 = if mode5 then count5 else -1
    let show10 = if mode10 then count10 else -1


    -- Part 2: dynamic version

    --   Every toggle event causes switch of counters, 
    --   with every counter is newly created.
    --
    --   Actually, This can be rewrited int more netwire-like form,
    --   since `switchOnF_ id x` is equivalent for `rSwitch x` on netwire.
    --   But this is more desirable form for auto because `switchOnF_` is not serializable .
    dynamic0 <-
        switchOnF (\case {True -> counter; False -> pure (-1)}) counter
            -< (click0, mode0 <$ toggle0)
    {-
    --   There's no alternative for `kSwitch` or `modes` in netwire.
    --   So scenario 5 is hard to implement.
    dynamic5 <- 
        modes True (\case {True -> counter; False -> pure (-1)})
            -< (click5, mode5 <$ toggle5)
    -}

    -- <|!> operator is suitable for scenario 10.
    dynamic10 <- 
        arr ((-1) <$) . unlessI fst <|!> counter . arr snd
            -< (mode10, click10)


    returnA -< 
        renderButtons 
            show0 (Just dynamic0) 
            show5 Nothing
            show10 (Just dynamic10)
  where
    counter :: Auto IO (Blip a) Int
    counter = proc e -> scanB (\x f -> f x) 0 -< (+1) <$ e

main :: IO ()
main =
  do
    playAuto
        (InWindow "Auto Example" (320, 240) (800, 200))
        white
        30
        mainAuto
