{-# LANGUAGE LambdaCase, GADTs #-}
module Main where

import Control.Object
import Control.Applicative
import Buttons
import Control.Monad
import Graphics.Gloss.Interface.IO.Game

data Async a b r where
  Push :: a -> Async a b ()
  Pull :: Async a b b

foldO :: Monad m => (a -> r -> r) -> r -> Object (Async a r) m
foldO c r = Object $ \case
  Pull -> return (r, foldO c r)
  Push a -> return ((), foldO c (c a r))

bool :: a -> a -> Bool -> a
bool a _ False = a
bool _ b True = b

count0 :: IO (Instance (Async ButtonClick Int) IO)
count0 = do
  counter <- newSettle $ \case { Click -> (+1); Toggle -> const 0 } `foldO` (0 :: Int)
  mode <- newSettle $ \case { Click -> id; Toggle -> not } `foldO` True
  new $ liftO $ \case
    Push b -> do
      counter .- Push b
      mode .- Push b
    Pull -> pull mode >>= bool (return (-1)) (pull counter)

count0D :: IO (Instance (Async ButtonClick Int) IO)
count0D = do
  let counter = \case { Click -> (+1); Toggle -> id } `foldO` (0 :: Int)
  holder <- newSettle counter >>= newSettle . foldO const
  mode <- newSettle $ \case { Click -> id; Toggle -> not } `foldO` True
  new $ liftO $ \case
    Push b -> do
      pull holder >>= (.- Push b)
      mode .- Push b
      when (b == Toggle) $ new counter >>= (holder .-) . Push
    Pull -> pull mode >>= bool (return (-1)) (pull holder >>= pull)

count5 :: IO (Instance (Async ButtonClick Int) IO)
count5 = do
  counter <- newSettle $ \case { Click -> (+1); Toggle -> id } `foldO` (0 :: Int)
  mode <- newSettle $ \case { Click -> id; Toggle -> not } `foldO` True
  new $ liftO $ \case
    Push b -> do
      mode .- Push b
      pull mode >>= bool (return ()) (counter .- Push b)
    Pull -> pull mode >>= bool (return (-1)) (pull counter)

count10 :: IO (Instance (Async ButtonClick Int) IO)
count10 = do
  counter <- newSettle $ \case { Click -> (+1); Toggle -> id } `foldO` (0 :: Int)
  mode <- newSettle $ \case { Click -> id; Toggle -> not } `foldO` True
  new $ liftO $ \case
    Push b -> do
      counter .- Push b
      mode .- Push b
    Pull -> pull mode >>= bool (return (-1)) (pull counter)

pull :: Instance (Async a b) IO -> IO b
pull = (.-Pull)

main = do
  c0 <- count0
  c0' <- count0D
  c5 <- count5
  c10 <- count10
  let push i (Just a) = i .- Push a
      push _ _ = return ()

  playIO (InWindow "Objective Example" (320, 240) (800, 200))
           white
           30
           ()
           (const $ renderButtons <$> (c0 .- Pull) <*> (fmap Just $ c0' .- Pull)
            <*> (c5 .- Pull) <*> pure Nothing
            <*> (c10 .- Pull) <*> pure Nothing)
           (\e () -> do
              push c0  $ filter0 e
              push c0' $ filter0 e
              push c5  $ filter5 e
              push c10 $ filter10 e)
           (\_ _ -> return ())
