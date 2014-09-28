module Main where

import Control.Arrow
import Graphics.Gloss

import Buttons


-- STATE

type State0  = Maybe Int
type State5  = (Bool, Int)
type State10 = (Bool, Int)

init0 :: State0
init0 = Just 0

init5 :: State5
init5 = (True, 0)

init10 :: State10
init10 = (True, 0)


-- BEHAVIOUR

step0 :: ButtonClick -> State0 -> State0
step0 Click Nothing = Nothing
step0 Click (Just count) = Just (count + 1)
step0 Toggle Nothing = init0
step0 Toggle (Just _) = Nothing

step5 :: ButtonClick -> State5 -> State5
step5 Click (True, count) = (True, count + 1)
step5 Toggle (mode, count) = (not mode, count)
step5 _ state = state

step10 :: ButtonClick -> State10 -> State10
step10 Click (mode, count) = (mode, count + 1)
step10 Toggle (mode, count) = (not mode, count)

processEvent :: Maybe ButtonClick -> (ButtonClick -> s -> s) -> s -> s
processEvent (Just event) step state = step event state
processEvent Nothing _ state = state


-- OUTPUT

output0 :: State0 -> Int
output0 Nothing = -1
output0 (Just count) = count

output5 :: State5 -> Int
output5 (False, _) = -1
output5 (True, count) = count

output10 :: State10 -> Int
output10 (False, _) = -1
output10 (True, count) = count


-- GLOSS EVENT LOOP

main :: IO ()
main = play (InWindow "Gloss Example" (320, 240) (800, 200))
            white
            300
            (init0, (init5, init10))
            (\(state0, (state5, state10)) -> renderButtons (output0  state0)
                                                           (output5  state5)
                                                           (output10 state10))
            (\e -> processEvent (event0  e) step0
               *** processEvent (event5  e) step5
               *** processEvent (event10 e) step10)
            (\_ -> id)
