module Main where

import Control.Applicative
import Data.IORef
import Graphics.Gloss.Interface.IO.Game

import Buttons
import CallbackSystem


main :: IO ()
main = do
    
    -- Mutable count
    
    count0  <- newIORef 0
    count5  <- newIORef 0
    count10 <- newIORef 0
    
    mode0  <- newIORef True
    mode5  <- newIORef True
    mode10 <- newIORef True
    
    
    -- Places where callbacks can be registered
    
    onClick0  <- newHook
    onClick5  <- newHook
    onClick10 <- newHook
    
    onToggle0  <- newHook
    onToggle5  <- newHook
    onToggle10 <- newHook
    
    
    -- Delegate to a helper function, to help with scoping
    
    go count0  mode0  onClick0  onToggle0
       count5  mode5  onClick5  onToggle5
       count10 mode10 onClick10 onToggle10
  where
    go :: IORef Int -> IORef Bool -> Hook () -> Hook ()
       -> IORef Int -> IORef Bool -> Hook () -> Hook ()
       -> IORef Int -> IORef Bool -> Hook () -> Hook ()
       -> IO ()
    go count0  mode0  onClick0  onToggle0
       count5  mode5  onClick5  onToggle5
       count10 mode10 onClick10 onToggle10 = do
      
        
        -- Initial callback network
        
        registerCallback onClick0  clickHandler0
        registerCallback onClick5  clickHandler5
        registerCallback onClick10 clickHandler10
        
        registerCallback onToggle0  toggleHandler0
        registerCallback onToggle5  toggleHandler5
        registerCallback onToggle10 toggleHandler10
        
        
        -- Gloss event loop
        
        playIO (InWindow "Callback Example" (320, 240) (800, 200))
               white
               30
               ()
               render
               (\e () -> do processEvent (filter0  e) onClick0  onToggle0
                            processEvent (filter5  e) onClick5  onToggle5
                            processEvent (filter10 e) onClick10 onToggle10)
               (\_ _ -> return ())
      
      where
        
        
        -- Input
        
        processEvent :: Maybe ButtonClick -> Hook () -> Hook () -> IO ()
        processEvent (Just Click)  onClick  _ = triggerCallbacks onClick  ()
        processEvent (Just Toggle) _ onToggle = triggerCallbacks onToggle ()
        processEvent Nothing _ _ = return ()
        
        
        -- Behaviour
        
        clickHandler0, clickHandler5, clickHandler10 :: Callback ()
        clickHandler0  () = modifyIORef count0  (+1)
        clickHandler5  () = modifyIORef count5  (+1)
        clickHandler10 () = modifyIORef count10 (+1)
        
        toggleHandler0, toggleHandler5, toggleHandler10 :: Callback ()
        toggleHandler0 = toggleOff0
        toggleHandler5 = toggleOff5
        toggleHandler10 () = modifyIORef mode10 not
        
        toggleOff0 :: Callback ()
        toggleOff0 () = do
            writeIORef mode0 False
            writeIORef count0 0
            
            unregisterCallbacks onClick0
            unregisterCallbacks onToggle0
            
            registerCallback onToggle0 toggleOn0
        
        toggleOn0 :: Callback ()
        toggleOn0 () = do
            writeIORef mode0 True
            
            registerCallback onClick0 clickHandler0
            unregisterCallbacks onToggle0
            
            registerCallback onToggle0 toggleOff0
        
        toggleOff5 :: Callback ()
        toggleOff5 () = do
            writeIORef mode5 False
            
            unregisterCallbacks onClick5
            unregisterCallbacks onToggle5
            
            registerCallback onToggle5 toggleOn5
        
        toggleOn5 :: Callback ()
        toggleOn5 () = do
            writeIORef mode5 True
            
            registerCallback onClick5 clickHandler5
            unregisterCallbacks onToggle5
            
            registerCallback onToggle5 toggleOff5
        
        
        -- Output
        
        chooseLabel :: IORef Int -> IORef Bool -> IO Int
        chooseLabel countRef modeRef = do
            mode <- readIORef modeRef
            if mode
            then readIORef countRef
            else return (-1)
        
        render :: () -> IO Picture
        render () = renderButtons
                <$> chooseLabel count0  mode0  <*> pure Nothing
                <*> chooseLabel count5  mode5  <*> pure Nothing
                <*> chooseLabel count10 mode10 <*> pure Nothing
