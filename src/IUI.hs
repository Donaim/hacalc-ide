
module IUI where

import Data.Dynamic
import PatternT.Types

data UIEvent
	= CompilerParseError [ParseMatchError]
	| ReaderNotify String
	| CompilerNotify String
	| DebugLog String
	| ResetEvaluations
	| PushEvaluation Tree [(String, String, String)]
	deriving (Eq, Show, Read, Typeable)
