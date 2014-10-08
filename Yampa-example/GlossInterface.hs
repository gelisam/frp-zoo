module GlossInterface where

import Prelude hiding (id, (.))

import FRP.Yampa

import Graphics.Gloss (Color, Display, Picture)
import qualified Graphics.Gloss.Interface.IO.Game as G

import Data.IORef

import Buttons

playYampa :: 
    Display ->
    Color ->
    Int ->
    SF (Event G.Event) Picture ->
    IO ()

playYampa display color frequency network = 
  do
    vPic <- newIORef $ renderButtons 0 Nothing 0 Nothing 0 Nothing

    handle <- reactInit 
        (return NoEvent)
        (\_ changed pic -> 
          do
            if changed then vPic `writeIORef` pic else return ()
            return False)
        network

    G.playIO
        display
        color
        frequency
        ()
        (const $ readIORef vPic)
        (\e _ -> react handle (1.0, Just (Event e)) >> return ())
        (\_ w -> return w)
