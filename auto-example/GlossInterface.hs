module GlossInterface where

import Prelude hiding (id, (.))
import Control.Auto
import Control.Auto.Blip

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

-- Only framework codes are allowed to import.
import Control.Auto.Blip.Internal

playAuto ::
    Display -> 
    Color ->
    Int ->
    Auto IO (Float, Blip G.Event) Picture -> 
    IO ()
playAuto display color frequency network =
  do
     -- Calculate initial step
    world0 <- step (Blank, first (scanB (+) 0.0) >>> network) (NoBlip, NoBlip)

    -- Main body
    G.playIO
        display
        color
        frequency
        world0
        (\(pic, _) -> return pic)
        (\e world-> step world (NoBlip, Blip e))
        (\delta world -> step world (Blip delta, NoBlip))

  where
    step (oldpic, aut) arg =
      do
        (r, aut') <- stepAuto aut arg
        return (r, aut')
