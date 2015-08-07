module GlossInterface where

import Prelude hiding (id, (.))
import Control.Artery
import Data.Maybe (fromMaybe)       

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G


playArtery ::
    Display -> 
    Color ->
    Int ->
    Artery IO (Float, Maybe G.Event) Picture -> 
    IO ()
playArtery display color frequency network =
  do
    -- Calculate initial step
    let art0 = first (scan (\d x -> x + fromMaybe 0.0 d) 0.0) >>> network
    world0 <- step (Blank, art0) (Nothing, Nothing)

    -- Main body
    G.playIO
        display
        color
        frequency
        world0
        (\(pic, _) -> return pic)
        (\e world-> step world (Nothing, Just e))
        (\delta world -> step world (Just delta, Nothing))

  where
    step (oldpic, art) arg =
      do
        (r, art') <- runArtery art arg
        return (r, art')
