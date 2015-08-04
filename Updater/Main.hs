module Main where

import Graphics.Gloss.Interface.IO.Game

import Buttons

import Updater
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
	-- fired when the button with the number is clicked
	inc0 <- newSignalIO ()
	inc5 <- newSignalIO ()
	inc10 <- newSignalIO ()

	-- changed when the toggle button is clicked
	toggle0 <- newSignalIO True
	toggle5 <- newSignalIO True
	toggle10 <- newSignalIO True

	-- output displayed on the number button
	output0 <-newSignalIO 0
	output5 <-newSignalIO 0
	output10 <-newSignalIO 0

	let
		run0 :: Updater ()
		run0 = do
			isOn <-getBehavior toggle0
			if not isOn
				then writeSignal output0 (-1)
				else do
			count <- newSignal 0
			-- the <|> return () is here so the output get's updated when
			-- toggle0 has changed but inc0 was not clicked yet
			(getEvent inc0 >> modifySignal count (+1)) <|> return ()
			readSignal count >>= writeSignal output0

		run5 :: Updater ()
		run5 = do
			count <- newSignal 0
			isOn <- getBehavior toggle5
			if not isOn
				then writeSignal output5 (-1)
				else do
			(getEvent inc5 >> modifySignal count (+1)) <|> return ()
			readSignal count >>= writeSignal output5

		run10 :: Updater ()
		run10 = do
			count <- newSignal 0
			isOn <- getBehavior toggle10
			(getEvent inc10 >> modifySignal count (+1)) <|> return ()
			if not isOn
				then writeSignal output10 (-1)
				else do
			readSignal count >>= writeSignal output10

	runGlobalUpdater ((run0 <|> run5 <|> run10) >> stop)

	-- START BOILER PLATE
	let
		processEvent :: Event -> Updater ()
		processEvent e =
			when (buttonC0 `isClickedBy` e) (writeSignal inc0 ()) <|>
			when (buttonC5 `isClickedBy` e) (writeSignal inc5 ()) <|>
			when (buttonC10 `isClickedBy` e) (writeSignal inc10 ()) <|>
			when (buttonT0 `isClickedBy` e) (readSignal toggle0 >>= putLine . show  >> modifySignal toggle0 not) <|>
			when (buttonT5 `isClickedBy` e) (modifySignal toggle5 not) <|>
			when (buttonT10 `isClickedBy` e) (modifySignal toggle10 not)

	playIO (InWindow "Updater FRPZoo" (320, 240) (800, 200))
		white
		30
		()
		(\_ -> do
			out0 <- runUpdater $ readSignal output0
			out5 <- runUpdater $ readSignal output5
			out10 <- runUpdater $ readSignal output10
			return $ renderButtons 
				out0  Nothing
				out5  Nothing
				out10 Nothing
			)
		(\e _ -> runUpdater $ processEvent e >> return ())
		(\_ -> return)
