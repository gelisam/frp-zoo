module GlossInterface where

import Prelude hiding (id, (.))
import Control.Wire

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

-- Only framework codes are allowed to import.
import Control.Wire.Unsafe.Event (Event(Event, NoEvent))


playNetwire ::
    Display -> 
    Color ->
    Int ->
    Wire (Timed Float ()) a IO (Event G.Event) Picture -> 
    IO ()
playNetwire display color frequency network =
  do
     -- Calculate initial step
    world0 <- step 0.0 (network, Blank) NoEvent 

    -- Main body
    G.playIO
        display
        color
        frequency
        world0
        (\(_, pic) -> return pic)
        (\e world-> step 0.0 world (Event e))
        (\delta world -> step delta world NoEvent)

  where
    step delta (w, oldpic) ev =
      do
        (r, w') <- stepWire w (Timed delta ()) (Right ev)
        return (w', either (const oldpic) id r)
