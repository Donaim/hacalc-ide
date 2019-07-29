
module SimpleUI where

import Data.Dynamic

import PatternT.All
import IUI

type EvalRecord = (Int, String, [(String, String, String)])

data UIState = UIState
	{ stopped              :: Bool
	, currentEvals         :: [EvalRecord]
	, longHistory          :: [EvalRecord]
	, outfile              :: String
	} deriving (Eq, Show, Read)



