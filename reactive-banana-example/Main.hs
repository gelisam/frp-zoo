{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Buttons

import Data.Bool (bool)
import Data.IORef (newIORef, readIORef, writeIORef)
import Graphics.Gloss hiding (pictures)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Graphics.Gloss.Interface.IO.Game as Gloss (Event, playIO)

-- FRP network

mainBanana :: Event Gloss.Event -> Moment (Behavior Picture)
mainBanana glossEvent = do
    -- Part 1: static version

    let button0, button5, button10 :: Event ButtonClick
        button0  = filterJust (filter0  <$> glossEvent)
        button5  = filterJust (filter5  <$> glossEvent)
        button10 = filterJust (filter10 <$> glossEvent)

        click0, click5, click10 :: Event ()
        click0  = () <$ filterE (== Click) button0
        click5  = () <$ filterE (== Click) button5
        click10 = () <$ filterE (== Click) button10

        toggle0, toggle5, toggle10 :: Event ()
        toggle0  = () <$ filterE (== Toggle) button0
        toggle5  = () <$ filterE (== Toggle) button5
        toggle10 = () <$ filterE (== Toggle) button10

    mode0  :: Behavior Bool <- accumB True (not <$ toggle0)
    mode5  :: Behavior Bool <- accumB True (not <$ toggle5)
    mode10 :: Behavior Bool <- accumB True (not <$ toggle10)

    count0  :: Behavior Int <- accumB 0 $ unions [const 0 <$ toggle0, (+1) <$ click0]
    count5  :: Behavior Int <- accumB 0 $ whenE mode5 ((+1) <$ click5)
    count10 :: Behavior Int <- accumB 0 $ ((+1) <$ click10)

    -- Part 2: dynamic version

    let toggleOn0 :: Event ()
        toggleOn0 = whenE (not <$> mode0) toggle0

        newCount0 :: Moment (Behavior Int)
        newCount0 = accumB 0 ((+1) <$ click0)

    dynamicCount0 :: Behavior Int <-
        switchB count0 (observeE (newCount0 <$ toggleOn0))

    -- Output

    let minus1 :: Behavior Int
        minus1 = pure (-1)

        output0, output5, output10 :: Behavior Int
        output0        = bool <$> minus1 <*> count0        <*> mode0
        dynamicOutput0 = bool <$> minus1 <*> dynamicCount0 <*> mode0
        output5        = bool <$> minus1 <*> count5        <*> mode5
        output10       = bool <$> minus1 <*> count10       <*> mode10

        picture :: Behavior Picture
        picture = renderButtons
            <$> output0  <*> (Just <$> dynamicOutput0)
            <*> output5  <*> pure Nothing
            <*> output10 <*> pure Nothing

    pure picture

-- Gloss event loop

main :: IO ()
main = do
  picRef <- newIORef blank
  (eventHandler, fireEvent) <- newAddHandler

  network <-
    compile $ do
      glossEvent <- fromAddHandler eventHandler
      picture <- liftMoment (mainBanana glossEvent)
      changes picture >>= reactimate' . fmap (fmap (writeIORef picRef))
      valueBLater picture >>= liftIO . writeIORef picRef

  actuate network

  Gloss.playIO
    (InWindow "Reactive-Banana Example" (320, 240) (800, 200))
    white
    30
    ()
    (\() -> readIORef picRef)
    (\e () -> fireEvent e)
    (\_ () -> pure ())
