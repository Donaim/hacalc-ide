
module Compiler where

import Data.Either
import Data.List
import Data.Char
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent as CONC
import Control.Exception

import PatternT.All

import ICompiler
import IUI
import Events
import Util

type SimplifyMonad = IO
type SimplifyCtx = ()
type MonadicSimplifyT = MonadicSimplify SimplifyMonad SimplifyCtx
type SimlifyFT = SimplificationF SimplifyMonad SimplifyCtx

simplifyCtxInitial :: SimplifyCtx
simplifyCtxInitial = ()

showCtx :: SimplifyCtx -> String
showCtx = show

builtinRules :: [PureSimplificationF]
builtinRules =
	[ ("$add", ruleAdd "$add")
	, ("$mult", ruleMult "$mult")
	]

splitLines :: String -> [String]
splitLines = lines

readPatterns :: String -> Either [ParseMatchError] [SimplifyPattern]
readPatterns text = do
	unless (null badRules) (Left badRules)
	return okRules
	where
	lines       = splitLines text
	filtered    = filter (not . isWhiteSpace) lines
	mrules      = map parseMatch filtered
	partitioned = partitionEithers mrules
	okRules     = snd partitioned
	badRules    = fst partitioned

	isWhiteSpace str = all isSpace str

mixedRules :: [SimplifyPattern] -> [SimlifyFT]
mixedRules patterns = map Tuple32 builtinRules ++ map Tuple30 patterns

type EvalRecord = (Int, String)

data CompilerState = CompilerState
	{ compilerStopped          :: Bool
	, compilerText             :: String
	, compilerPatterns         :: [SimplifyPattern]
	, evalCount                :: Int
	, currentEvals             :: [EvalRecord]
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
	}

compilerProcess :: CompilerCtx -> CompilerState -> [CompilerEvent] -> IO (CompilerState, [Dynamic])
compilerProcess ctx state events0 = do
	(newstate, rbuf) <- loop [] state events0
	return (newstate, reverse rbuf)
	where
	loop buf state [] = return (state, buf)
	loop buf state (x : xs) = case x of
		CompilerStopEvent ->
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
			loop
				(appendDyn (DebugLog $ "Appended new evaluation: " ++ line) buf)
				newstate
				xs

		(CompilerRemoveEvalRecord i) -> do
			let newstate = state { currentEvals = (filter ((/= i) . fst) (currentEvals state)) }
			killRunningSimplification ctx i
			loop buf newstate xs

runSimplification :: CompilerCtx -> Bool -> [SimplifyPattern] -> EvalRecord -> IO ()
runSimplification ctx rerunq patterns (currentEvalIndex, line) = do

	newth <- CONC.forkIO safeSimpthread
	atomicModifyIORef' (execThreads ctx) (\ threads -> ((currentEvalIndex, line, newth) : threads, ())) -- FIXME: datarace possible - if `newth` finishes too quickly, the removeFunc will do nothing and this thread will hang in list forever
	sendEvent (ebin ctx) (EvaluationStarted currentEvalIndex)

	where
	simpthread = runSimplificationThread (ebin ctx) rerunq (mixedRules patterns) (currentEvalIndex, line)
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

rerunSimplificationThread :: CompilerCtx -> [SimplifyPattern] -> Int -> IO ()
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

runSimplificationThread :: EventsBin -> Bool -> [SimlifyFT] -> EvalRecord -> IO ()
runSimplificationThread ebin rerunq mixed (index, line) =
	case tokens of
		Left err ->
			sendEvent ebin (CompilerTokenizeError err)
		Right oktokens ->
			withTokens oktokens
	where
	tokens = tokenize line
	withTokens oktokens = do
		let tree = makeTree (Group oktokens)
		history <- mixedApplySimplificationsWithPureUntil0Debug mixed simplifyCtxInitial tree
		sendEvent ebin (PushEvaluation rerunq index line (showHistory history))

	showHistory :: [(Tree, Either SimplifyPattern String, SimplifyCtx)] -> [(String, String, String)]
	showHistory = map f
		where
		f (t, traceElem, ctx) = (stringifyTree t, stringifyTraceElem traceElem, showCtx ctx)
