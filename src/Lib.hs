module Lib where

import Data.Either
import Control.Monad

import PatternT.Types
import PatternT.Util
import PatternT.Core
import PatternT.SimplifyInterface
import PatternT.Parsing
import PatternT.Display
import PatternT.MonadicRules

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

readRules :: String -> Either [ParseMatchError] [SimlifyFT]
readRules text = do
	unless (null badRules) (Left badRules)
	return mixedRules

	where
	lines = splitLines text

	mrules      = map parseMatch lines
	partitioned = partitionEithers mrules
	okRules     = snd partitioned

	mixedRules :: [SimlifyFT]
	mixedRules = map Tuple32 builtinRules ++ map Tuple30 okRules

	badRules = fst partitioned

someFunc :: IO ()
someFunc = putStrLn "someFunc"
