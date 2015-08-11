module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import Buttons
import Control.Varying

main :: IO ()
main = G.playIO (InWindow "Varying Example" (320, 240) (800, 200)) white 30
                (renderButtons 0 (Just 0) 0 (Just 0) 0 (Just 0), network)
                (return . fst) (\e (_, network') -> runVar network' e)
                (const $ return)

network :: Monad m => Var m G.Event Picture
network =
    renderButtons <$>           negWhenUntoggled static0 (toggled filter0)
                  <*> (Just <$> negWhenUntoggled dyn0    (toggled filter0))
                  <*>           negWhenUntoggled static5 (toggled filter5)
                  <*> (Just <$> negWhenUntoggled dyn5    (toggled filter5))
                  <*>           negWhenUntoggled static10(toggled filter10)
                  <*> (Just <$> negWhenUntoggled dyn10   (toggled filter10))

-- | Simply produces "-1" if the second signal produces False, or the value
-- of the first signal.
negWhenUntoggled :: Monad m
               => Var m G.Event Int -> Var m G.Event Bool -> Var m G.Event Int
negWhenUntoggled count mode = (\n on -> if on then n else -1) <$> count <*> mode
--------------------------------------------------------------------------------
-- Static
--------------------------------------------------------------------------------
static0 :: Monad m => Var m G.Event Int
static0 = events ~> collectWith append ~> var (foldl (flip ($)) 0)
    where append on xs = if on then (+1):xs else []
          events      = (<$) <$> toggled filter0 <*> clicked filter0

static5 :: Monad m => Var m G.Event Int
static5 = events ~> collectWith append ~> var (foldl (flip ($)) 0)
    where append on xs = if on then (+1):xs else xs
          events       = (<$) <$> toggled filter5 <*> clicked filter5

static10 :: Monad m => Var m G.Event Int
static10 = clicked filter10 ~> accumulate (\n e -> if isEvent e then n+1 else n)
                                          0
--------------------------------------------------------------------------------
-- Dynamic
--------------------------------------------------------------------------------
dyn0 :: Monad m => Var m G.Event Int
dyn0 = switchByMode (toggled filter0) $ \on -> if on then count else 0
    where count = clicked filter0 ~> collect ~> var length

dyn5 :: Monad m => Var m G.Event Int
dyn5 = (count `onlyWhenE` toggledOn) ~> startingWith 0
    where count = clicked filter5 ~> collect ~> var length
          toggledOn = toggled filter5 ~> onTrue

dyn10 :: Monad m => Var m G.Event Int
dyn10 = count `orE` pass
    where count = clicked filter10 ~> collect ~> var length
          pass  = toggled filter10 ~> onWhen not ~> var (0 <$)
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
clicked :: Monad m => (G.Event -> Maybe ButtonClick) -> Var m G.Event (Event ())
clicked f = var ((== Just Click)  . f) ~> onTrue

toggled :: Monad m => (G.Event -> Maybe ButtonClick) -> Var m G.Event Bool
toggled f = toggle f ~> accumulate (\on e -> if isEvent e then not on else on)
                                   True

toggle :: Monad m => (G.Event -> Maybe ButtonClick) -> Var m G.Event (Event ())
toggle f = var ((== Just Toggle) . f)  ~> onTrue
