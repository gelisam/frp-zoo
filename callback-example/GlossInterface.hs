module GlossInterface where

import Data.IORef
import Graphics.Gloss.Interface.IO.Game

import CallbackSystem


playHook :: Display
         -> Color
         -> Int
         -> IORef Picture
         -> Hook Event
         -> IO ()
playHook display color frequency pictureRef hook = do
    playIO display
           color
           frequency
           ()
           (\() -> readIORef pictureRef)
           (\e () -> triggerCallbacks hook e)
           mempty
