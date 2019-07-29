
module Compiler where

import Data.Either
import Control.Monad
import Data.Dynamic

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
	, compilerEvalRecords      :: [(Tree, Maybe Tree)]
	} deriving (Eq, Show, Read)
