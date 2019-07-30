
module SimpleUI where

import Data.Dynamic
import System.IO
import Control.Monad
import Data.List

import PatternT.All
import Events
import Util
import ICompiler
import IUI

import Debug.Trace

type UIEvalRecord = (Int, String, [(String, String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [UIEvalRecord]
	, longHistory          :: [[UIEvalRecord]]
	, outfile              :: String
	, refreshq             :: Bool
	, tracePadding         :: Int
	} deriving (Eq, Show, Read)

data UICtx = UICtx
	{ outHandle :: Handle
	}

simpleUINew :: String -> UIState
simpleUINew filepath = UIState
	{ stopped = True
	, currentEvals = []
	, longHistory = []
	, outfile = filepath
	, refreshq = False
	, tracePadding = 20
	}

writeEvals :: Handle -> [UIEvalRecord] -> IO ()
writeEvals handle evals = do
	unless (null evals) $ writeOut handle text
	where
	sorted = sortBy (\ a b -> compare (fst3 a) (fst3 b)) evals
	formatted = map formatEval sorted
	text = unlines formatted

formatEval :: UIEvalRecord -> String
formatEval (id, line, history) =
	(padLeft ' ' 6 (show id ++ ")")) ++ line ++ " -> " ++ result
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
	reactorStartFlag state = state { stopped = False }
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
			stdlog $ "PARSE ERROR: " ++ show x
			next

		(CompilerTokenizeError err) -> do
			stdlog $ "TOKENIZE ERROR: " ++ show x
			next

		(Notify message) -> do
			stdlog message
			next

		(DebugLog log) -> do
			stdlog $ "LOG: " ++ log
			next

		(ClearEvaluations) -> do
			let newstate = state
				{ currentEvals = []
				, longHistory = (currentEvals state) : (longHistory state)
				, refreshq = True
				}
			loop buf newstate xs

		(PushEvaluation rerunq id line history) -> do
			let record = (id, line, history)
			let newstate = state
				{ currentEvals = record : (currentEvals state)
				, refreshq = True
				}
			unless (rerunq) (showTrace (tracePadding newstate) history)
			loop buf newstate xs

		(EvaluationStarted id) -> do
			stdlog $ "EVALSTARTED: " ++ show id
			next

		(UISetPadding newpadding) -> do
			let newstate = state { tracePadding = newpadding }
			loop buf newstate xs

		(RemoveEvaluation i) -> do
			let newstate = state
				{ currentEvals = (filter ((/= i) . fst3) (currentEvals state))
				, refreshq = True
				}
			loop (appendDyn (CompilerRemoveEvalRecord i) buf) newstate xs

		where next = loop buf state xs

stdlog :: String -> IO ()
stdlog text = hPutStrLn stderr text

showTrace :: Int -> [(String, String, String)] -> IO ()
showTrace pad history = putStrLn reductions
	where
	reductions = unlines (map showReduction history)
	showReduction (tree, rule, ctx) = "\t" ++ (padLeft ' ' pad tree) ++ " [using] " ++ rule
