
module SimpleUI where

import Data.Dynamic
import System.IO

import PatternT.All
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

