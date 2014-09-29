-- | An extremely simple callback system
module CallbackSystem where

import Control.Monad
import Data.IORef


type Callback a = a -> IO ()
type Hook a = IORef [Callback a]

newHook :: IO (Hook a)
newHook = newIORef []

triggerCallbacks :: Hook a -> a -> IO ()
triggerCallbacks hook x = do
    callbacks <- readIORef hook
    forM_ callbacks $ \callback ->
      callback x

registerCallback :: Hook a -> Callback a -> IO ()
registerCallback hook callback = modifyIORef hook (callback:)

unregisterCallbacks :: Hook a -> IO ()
unregisterCallbacks hook = writeIORef hook []
