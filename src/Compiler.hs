
module Compiler where

import Data.Either
import Control.Monad
import Data.IORef
import Data.Dynamic
import qualified Control.Concurrent.ParallelIO.Local as CONC
import qualified Control.Concurrent as CONC

import PatternT.Types
import PatternT.Util
import PatternT.Core
import PatternT.SimplifyInterface
import PatternT.Parsing
import PatternT.Display
import PatternT.MonadicRules

import ICompiler
import IUI
import Events

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
	mrules      = map parseMatch lines
	partitioned = partitionEithers mrules
	okRules     = snd partitioned
	badRules    = fst partitioned

mixedRules :: [SimplifyPattern] -> [SimlifyFT]
mixedRules patterns = map Tuple32 builtinRules ++ map Tuple30 patterns

data CompilerState = CompilerState
	{ compierStopped           :: Bool
	, compilerText             :: String
	, compilerPatterns         :: [SimplifyPattern]
	, compilerEvalRecords      :: [Tree]
	} deriving (Eq, Show, Read)

data CompilerCtx = CompilerCtx
	{ execThread    :: Maybe CONC.ThreadId
	, ebin          :: EventsBin
	}

runSimplifications :: CompilerCtx -> CompilerState -> IO (CompilerCtx, CompilerState)
runSimplifications ctx state = do

	case execThread ctx of
		Just th -> CONC.killThread th
		Nothing -> return ()

	sendEvent (ebin ctx) ResetEvaluations

	newth <- CONC.forkIO $ runSimplificationsThread (ebin ctx) state
	let newctx = ctx { execThread = Just newth }

	return (newctx, state)

runSimplificationsThread :: EventsBin -> CompilerState -> IO ()
runSimplificationsThread ebin state = mapM_ forF (compilerEvalRecords state)
	where
	mixed = mixedRules (compilerPatterns state)
	forF tree = do
		history <- mixedApplySimplificationsWithPureUntil0Debug mixed simplifyCtxInitial tree
		sendEvent ebin (PushEvaluation tree (showHistory history))

showHistory :: [(Tree, Either SimplifyPattern String, SimplifyCtx)] -> [(String, String, String)]
showHistory = map f
	where
	f (t, traceElem, ctx) = (stringifyTree t, stringifyTraceElem traceElem, showCtx ctx)
