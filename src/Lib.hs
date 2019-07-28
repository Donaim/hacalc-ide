module Lib where

import Data.Either
import Control.Monad
import Data.IORef
import System.Directory
import Data.Time.Clock
import qualified Control.Concurrent as CONC

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

readerNotify :: String -> IO ()
readerNotify message = putStrLn message

readFileLoop :: ReplSessionCtx -> IO ()
readFileLoop ctx = do
	current <- getCurrentTime
	loop "" current
	where
	loop :: String -> UTCTime -> IO ()
	loop oldfile mtime = do
		CONC.threadDelay (100 * 1000)

		sess <- readIORef (session ctx)
		newmtime <- getModificationTime (currentFile sess)

		unless (stopped sess) $
			if (currentFile sess /= oldfile || newmtime > mtime)
			then do
				text <- readFile (currentFile sess)

				case readRules text of
					Left errors ->
						readerNotify $ "Errors during parsing rules: " ++ show errors
					Right {} -> do
						readerNotify $ "Read new rules"
						atomicModifyIORef' (session ctx) (setNewText text)
						atomicModifyIORef' (changedq ctx) (\ b -> (True, ()))

				loop (currentFile sess) newmtime
			else do
				loop oldfile mtime

	setNewText :: String -> ReplSession -> (ReplSession, ())
	setNewText newText sess = (sess { ruleText = newText }, ())

data ReplSession = ReplSession
	{ ruleText    :: String
	, currentFile :: String
	, stopped     :: Bool
	} deriving (Eq, Show, Read)

sessionNew :: String -> IO ReplSessionCtx
sessionNew filename = do
	let session = ReplSession
		{ ruleText = ""
		, currentFile = filename
		, stopped = False
		}
	ref <- newIORef session
	changedRef <- newIORef False
	return $ ReplSessionCtx { session = ref, changedq = changedRef }

data ReplSessionCtx = ReplSessionCtx
	{ session :: IORef ReplSession
	, changedq :: IORef Bool
	}

someFunc :: IO ()
someFunc = putStrLn "someFunc"
