
module SimpleUI where

import Data.Dynamic
import System.IO
import Control.Monad
import Data.List

import PatternT.All
import Events
import Util
import IUI

import Debug.Trace

type UIEvalRecord = (Int, String, [(String, String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [UIEvalRecord]
	, longHistory          :: [[UIEvalRecord]]
	, outfile              :: String
	, refreshq             :: Bool
	} deriving (Eq, Show, Read)

data UICtx = UICtx
	{ outHandle :: Handle
	}

simpleUINew :: String -> UIState
simpleUINew filepath = UIState
	{ stopped = False
	, currentEvals = []
	, longHistory = []
	, outfile = filepath
	, refreshq = False
	}

writeEvals :: Handle -> [UIEvalRecord] -> IO ()
writeEvals handle evals = do
	trace ("USING EVALS: " ++ show evals) $ writeOut handle text
	where
	-- sorted = sortBy (\ a b -> compare (fst3 a) (fst3 b)) evals
	sorted = evals
	formatted = map formatEval sorted
	text = unlines formatted

formatEval :: UIEvalRecord -> String
formatEval (id, line, history) =
	show id ++ ") " ++ line ++ " -> " ++ result
	where
	result = case history of
		[] -> line
		or -> fst3 $ last history

writeOut :: Handle -> String -> IO ()
writeOut handle text = do
	hSeek handle AbsoluteSeek 0
	hSetFileSize handle 0
	hPutStr handle text
	hFlush handle

instance Reactor UIState UIEvent UICtx where
	reactorStoppedQ = stopped
	reactorDelayMS = const 100

	reactorProcess = process
	reactorNewCtx = newctx

newctx :: EventsBin -> UIState -> IO UICtx
newctx ebin state = do
	handle <- openFile (outfile state) WriteMode
	return $ UICtx { outHandle = handle }

process :: UICtx -> UIState -> [UIEvent] -> IO (UIState, [Dynamic])
process ctx state events0 = do
	(state1, rbuf) <- loop [] state events0
	when (refreshq state1) $ do
		writeEvals (outHandle ctx) (currentEvals state1)
	let state2 = state1 { refreshq = False }
	return (state2, reverse rbuf)
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

		(ClearEvaluations) -> do
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
