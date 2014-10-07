module GlossInterface where

import Prelude hiding (id, (.))
import Control.Wire

import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons

playNetwire ::
    Display -> 
    Color ->
    Int ->
    Wire (Timed t ()) a IO G.Event Picture -> 
    t -> 
    IO ()
playNetwire display color frequency network period =
    G.playIO
        display
        color
        frequency
        (network, countSession_ period, renderButtons 0 Nothing 0 Nothing 0 Nothing)
        (\(_, _, pic) -> return pic)
        (\e (w, session, pic) ->
          do
            (s, session') <- stepSession session
            (r, w') <- stepWire w s (Right e)
            return (w', session', either (const pic) id r))
        (\_ w -> return w)
