
module SimpleUI where

import Data.Dynamic
import System.IO
import Control.Monad

import PatternT.All
import Events
import Util
import IUI

type EvalRecord = (Int, String, [(String, String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [EvalRecord]
	, longHistory          :: [[EvalRecord]]
	, outfile              :: String
	, refreshq             :: Bool
	} deriving (Eq, Show, Read)

data UICtx = UICtx
	{ outHandle :: Handle
	}

simpleUINew :: String -> IO (UICtx, UIState)
simpleUINew filepath = do
	handle <- openFile filepath WriteMode
	let state = UIState
		{ stopped = False
		, currentEvals = []
		, longHistory = []
		, outfile = filepath
		, refreshq = False
		}
	let ctx = UICtx
		{ outHandle = handle
		}
	return (ctx, state)

writeEvals :: Handle -> [EvalRecord] -> IO ()
writeEvals handle evals = do
	let formatted = unlines $ map formatEval (reverse evals)
	writeOut handle formatted

formatEval :: EvalRecord -> String
formatEval (id, line, history) =
	show id ++ ") " ++ line ++ " -> " ++ result
	where result = fst3 $ last history

writeOut :: Handle -> String -> IO ()
writeOut handle text = do
	hSeek handle AbsoluteSeek 0
	hPutStr handle text
	hFlush handle

instance Reactor UIState UIEvent UICtx where
	reactorStoppedQ = stopped
	reactorDelayMS = const 100

	reactorProcess = process

process :: UICtx -> UIState -> [UIEvent] -> IO (UIState, [Dynamic])
process ctx state events0 = do
	(newstate, rbuf) <- loop [] state events0
	when (refreshq newstate) $ do
		writeEvals (outHandle ctx) (currentEvals newstate)
	let newstate = newstate { refreshq = False }
	return (newstate, reverse rbuf)
	where
	loop buf state [] = return (state, buf)
	loop buf state (x : xs) = case x of
		(UIStopEvent) -> do
			let newstate = state { stopped = True }
			return (newstate, buf)

		(CompilerParseError errs) -> do
			putStrLn $ "PARSE ERROR: " ++ show x
			next

		(CompilerTokenizeError err) -> do
			putStrLn $ "TOKENIZE ERROR: " ++ show x
			next

		(ReaderNotify notify) -> do
			putStrLn $ "READER: " ++ notify
			next

		(CompilerNotify notify) -> do
			putStrLn $ "COMPILER: " ++ notify
			next

		(DebugLog log) -> do
			putStrLn $ "LOG: " ++ log
			next

		(ResetEvaluations) -> do
			let newstate = state
				{ currentEvals = []
				, longHistory = (currentEvals state) : (longHistory state)
				, refreshq = True
				}
			loop buf newstate xs

		(PushEvaluation id line history) -> do
			let record = (id, line, history)
			let newstate = state
				{ currentEvals = record : (currentEvals state)
				, refreshq = True
				}
			loop buf newstate xs

		(EvaluationStarted id) -> do
			putStrLn $ "EVALSTARTED: " ++ show id
			next

		where next = loop buf state xs
