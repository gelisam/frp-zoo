{-# LANGUAGE Arrows #-}

module
    Main
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Functor ((<$))

import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons
import GlossInterface


mainSF :: SF (Event G.Event) Picture
mainSF = proc e ->
  do
    let
        click0 = ((Just Click ==) . filter0) `filterE` e
        click5 = ((Just Click ==) . filter5) `filterE` e
        click10 = ((Just Click ==) . filter10) `filterE` e

        toggle0 = ((Just Toggle ==) . filter0) `filterE` e
        toggle5 = ((Just Toggle ==) . filter5) `filterE` e
        toggle10 = ((Just Toggle ==) . filter10) `filterE` e
        
    mode0 <- accumHold True -< not <$ toggle0 
    mode5 <- accumHold True -< not <$ toggle5 
    mode10 <- accumHold True -< not <$ toggle10

    -- ---------------------------
    -- First order implementation
    -- ---------------------------
    count0 <- accumHold 0 -< mergeBy (.) (const 0 <$ toggle0) ((+1) <$ click0)
    count5 <- accumHold 0 -< (if mode5 then (+1) else id) <$ click5
    count10 <- accumHold 0 -< (+1) <$ click10

    let
        show0 = if mode0 then count0 else -1
        show5 = if mode5 then count5 else -1
        show10 = if mode10 then count10 else -1

    -- ---------------------------
    -- Higher order implementation
    -- ---------------------------

    -- Every toggle event causes switch of counters, 
    -- with every counter is newly created.
    let newCounter0 = if mode0 then  counter else arr $ const (-1)
    dynamic0 <- rSwitch counter -< (click0, newCounter0 <$ toggle0)

    -- Every toggle event causes switch of a counter, 
    -- with one counter reused.
    dynamic5 <- 
        (let 
            active pa _ = 
                kSwitch 
                    pa 
                    (arr $ \((_, mode), _) -> (==False) `filterE` mode) 
                    inactive
            inactive pa _ = 
                switch 
                    (arr (const (-1)) *** (arr $ ((==True) `filterE`)))
                    (active pa)
          in
            active (arr fst >>> counter) True)
        -< (click5, mode5 <$ toggle5)


    returnA -< 
        renderButtons 
            show0 (Just dynamic0)
            show5 (Just dynamic5)
            show10 Nothing
  where
    counter = proc e -> accumHold 0 -< (+1) <$ e




main :: IO ()
main =
  do
    playYampa
        (InWindow "Yampa Example" (320, 240) (800, 200))
        white
        30
        mainSF
