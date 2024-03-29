
module SimpleUI where

import Data.Dynamic
import System.IO
import Control.Monad
import Data.List

import Hacalc.PatternT
import Events
import Util
import IUI

import Debug.Trace

type UIEvalRecord = (Int, String, [(String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [UIEvalRecord]
	, outfile              :: String
	, refreshq             :: Bool
	, tracePadding         :: Int
	, showLimit            :: Int  -- ASSUMPTION: Must be the same as Compiler's `evalLimit`
	} deriving (Eq, Show, Read)

data UICtx = UICtx
	{ outHandle :: Handle
	}

simpleUINew :: String -> UIState
simpleUINew filepath = UIState
	{ stopped = True
	, currentEvals = []
	, outfile = filepath
	, refreshq = False
	, tracePadding = 20
	, showLimit = 1  -- ASSUMPTION: Must be the same as Compiler's `evalLimit`
	}

writeEvals :: Int -> Handle -> [UIEvalRecord] -> IO ()
writeEvals pad handle evals = do
	writeOut handle text
	where
	sorted = sortBy (\ a b -> compare (fst3 a) (fst3 b)) evals
	formatted = map (formatEval pad) sorted
	text = unlines formatted

formatEval :: Int -> UIEvalRecord -> String
formatEval pad (id, line, history) =
	(padLeft ' ' 6 (show id ++ ")")) ++ line ++ "\n\t" ++ result
	where
	result = case history of
		[] -> line
		or -> fst $ last history

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
		writeEvals (tracePadding state1) (outHandle ctx) (currentEvals state1)
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
			let limitedState = newstate { currentEvals = limitEvals (showLimit newstate) (currentEvals newstate) }
			loop buf limitedState xs

		(EvaluationStarted id) -> do
			stdlog $ "EVALSTARTED: " ++ show id
			next

		(UISetPadding newpadding) -> do
			let newstate = state { tracePadding = newpadding }
			loop buf newstate xs

		(UIRemoveEvaluation i) -> do
			let newstate = state
				{ currentEvals = (filter ((/= i) . fst3) (currentEvals state))
				, refreshq = True
				}
			loop buf newstate xs

		(UIChangeLimit n) -> do
			let newstate = state
				{ showLimit = n
				, currentEvals = limitEvals n (currentEvals state)
				, refreshq = True
				}
			loop buf newstate xs

		where next = loop buf state xs

limitEvals :: Int -> [UIEvalRecord] -> [UIEvalRecord]
limitEvals lim cur =
	if lim > 0 && lim < length cur
	then take lim cur
	else cur

stdlog :: String -> IO ()
stdlog text = hPutStrLn stderr text

showTrace :: Int -> [(String, String)] -> IO ()
showTrace pad history = putStrLn reductions
	where
	reductions = unlines (map showReduction history)
	showReduction (tree, rule) = "\t" ++ (padLeft ' ' pad tree) ++ " [using] " ++ rule
