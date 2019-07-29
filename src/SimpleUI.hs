
module SimpleUI where

import Data.Dynamic
import System.IO

import PatternT.All
import Util
import IUI

type EvalRecord = (Int, String, [(String, String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [EvalRecord]
	, longHistory          :: [EvalRecord]
	, outfile              :: String
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
		}
	let ctx = UICtx
		{ outHandle = handle
		}
	return (ctx, state)

writeEvals :: Handle -> [EvalRecord] -> IO ()
writeEvals handle evals = do
	let formatted = unlines $ map formatEval evals
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
