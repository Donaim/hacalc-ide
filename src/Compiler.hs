
module Compiler where

import Data.Either
import Data.List
import Data.Char
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent as CONC
import Control.Exception

import PatternT.Types
import PatternT.Display
import Hacalc.Run
import Hacalc.Types
import Hacalc.Parser

import ICompiler
import IUI
import Events
import Util

type EvalRecord = (Int, String)

data CompilerState = CompilerState
	{ compilerStopped          :: Bool
	, compilerText             :: String
	, compilerPatterns         :: [[SimplifyPattern]]
	, evalCount                :: Int
	, currentEvals             :: [EvalRecord]
	, evalLimit                :: Int -- ASSUMPTION: Must be the same as UI's `showLimit`
	} deriving (Eq, Show, Read)

instance Reactor CompilerState CompilerEvent CompilerCtx where
	reactorStartFlag state = state { compilerStopped = False }
	reactorStoppedQ = compilerStopped
	reactorDelayMS = const 100
	reactorProcess = compilerProcess
	reactorNewCtx = newctx

data CompilerCtx = CompilerCtx
	{ execThreads   :: IORef [(Int, String, CONC.ThreadId)]
	, ebin          :: EventsBin
	}

newctx :: EventsBin -> CompilerState -> IO CompilerCtx
newctx ebin state = do
	ref <- newIORef []
	return $ CompilerCtx { execThreads = ref, ebin = ebin }

compilerNew :: CompilerState
compilerNew = CompilerState
	{ compilerStopped = True
	, compilerText = ""
	, compilerPatterns = []
	, evalCount = 0
	, currentEvals = []
	, evalLimit = 1 -- ASSUMPTION: Must be the same as UI's `showLimit`
	}

compilerProcess :: CompilerCtx -> CompilerState -> [CompilerEvent] -> IO (CompilerState, [Dynamic])
compilerProcess ctx state events0 = do
	(newstate, rbuf) <- loop [] state events0
	return (newstate, reverse rbuf)
	where
	loop buf state [] = return (state, buf)
	loop buf state (x : xs) = case x of
		CompilerStopEvent -> do
			threadsDoall ctx (killRunningSimplification ctx)
			return (state { compilerStopped = True }, buf)

		(SourceFileUpdated newtext) -> do
			let mpatterns = readPatterns newtext
			case mpatterns of
				Left e ->
					loop
						(appendDyn (CompilerParseError e) buf)
						state
						xs
				Right patterns -> do
					let newstate = state { compilerText = newtext, compilerPatterns = patterns }
					sendEvent (ebin ctx) (ClearEvaluations)
					threadsDoall ctx (killRunningSimplification ctx)
					mapM_ (runSimplification ctx True patterns) (currentEvals newstate)
					loop
						(appendDyn (DebugLog "Rule file updated -> rerunning evaluations") buf)
						newstate
						xs

		(AppendEvaluation line) -> do
			let record = (evalCount state, line)
			let newstate = state { evalCount = evalCount state + 1, currentEvals = record : currentEvals state }
			runSimplification ctx False (compilerPatterns state) record

			limitedRecords <- limitRecords ctx (evalLimit newstate) (currentEvals newstate)
			let limitedState = newstate { currentEvals = limitedRecords }

			loop
				(appendDyn (DebugLog $ "Appended new evaluation: " ++ line) buf)
				limitedState
				xs

		(CompilerRemoveEvalRecord i) -> do
			let newstate = state { currentEvals = (filter ((/= i) . fst) (currentEvals state)) }
			killRunningSimplification ctx i
			loop buf newstate xs

		(CompilerResetEvaluations) -> do
			let newstate = state { currentEvals = [] }
			threadsDoall ctx (killRunningSimplification ctx)
			loop buf newstate xs

		(CompilerChangeLimit n) -> do
			limitedRecords <- limitRecords ctx n (currentEvals state)
			let limitedState = state { currentEvals = limitedRecords, evalLimit = n }
			loop buf limitedState xs

limitRecords :: CompilerCtx -> Int -> [EvalRecord] -> IO [EvalRecord]
limitRecords ctx lim cur = do
	if lim <= 0 || lim > length cur
	then return cur
	else do
		let (stay, dropped) = partition lim cur
		mapM_ (killRunningSimplification ctx . fst) dropped
		return stay
	where
	partition n arr = (take n arr, drop n arr)

runSimplification :: CompilerCtx -> Bool -> [[SimplifyPattern]] -> EvalRecord -> IO ()
runSimplification ctx rerunq patterns (currentEvalIndex, line) = do

	newth <- CONC.forkIO safeSimpthread
	atomicModifyIORef' (execThreads ctx) (\ threads -> ((currentEvalIndex, line, newth) : threads, ())) -- FIXME: datarace possible - if `newth` finishes too quickly, the removeFunc will do nothing and this thread will hang in list forever
	sendEvent (ebin ctx) (EvaluationStarted currentEvalIndex)

	where
	simpthread = runSimplificationThread (ebin ctx) rerunq patterns (currentEvalIndex, line)
	safeSimpthread = do
		x <- try simpthread
		handleResult x

	handleResult :: Either SomeException () -> IO ()
	handleResult x = do
		removeFunc
		case x of
			Right () ->
				return ()
			Left ex ->
				sendEvent (ebin ctx) (DebugLog $ "Simplification thread#" ++ show currentEvalIndex ++ " with expr = " ++ line ++ " failed with: " ++ show ex)

	removeFunc = atomicModifyIORef' (execThreads ctx) modify
	modify threads = (filter ((/= currentEvalIndex) . fst3) threads, ())

threadsDoall :: CompilerCtx -> (Int -> IO ()) -> IO ()
threadsDoall ctx func = do
	threads <- readIORef (execThreads ctx)
	let indexes = map fst3 threads
	mapM_ func indexes

rerunSimplificationThread :: CompilerCtx -> [[SimplifyPattern]] -> Int -> IO ()
rerunSimplificationThread ctx patterns index = do
	matched <- atomicModifyIORef' (execThreads ctx) modify
	mapM_ todo matched
	where
	todo (id, line, th) = do
		CONC.killThread th
		runSimplification ctx True patterns (id, line)

	modify threads = partition ((== index) . fst3) threads

killRunningSimplification :: CompilerCtx -> Int -> IO ()
killRunningSimplification ctx index = do
	matched <- atomicModifyIORef' (execThreads ctx) modify
	mapM_ todo matched
	where
	todo (id, line, th) = do
		CONC.killThread th

	modify threads = partition ((/= index) . fst3) threads

runSimplificationThread :: EventsBin -> Bool -> [[SimplifyPattern]] -> EvalRecord -> IO ()
runSimplificationThread ebin rerunq patterns (index, line) = do
	case interpretLine patterns line of
		Left e ->
			sendEvent ebin (CompilerTokenizeError e)
		Right iohistory -> do
			history <- iohistory
			sendEvent ebin (PushEvaluation rerunq index line (showHistory history))
	where
	showHistory :: [(Tree, Either SimplifyPattern String, SimplifyCtx)] -> [(String, String, String)]
	showHistory = map f
		where
		f (t, traceElem, ctx) = (stringifyTree t, stringifyTraceElem traceElem, showCtx ctx)
