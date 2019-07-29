
module IUI where

import Data.Dynamic
import PatternT.Types

data UIEvent
	= UIStopEvent
	| CompilerParseError [ParseMatchError]
	| CompilerTokenizeError ParseError
	| ReaderNotify String
	| CompilerNotify String
	| DebugLog String
	| ResetEvaluations
	| PushEvaluation Int String [(String, String, String)]
	| EvaluationStarted Int
	deriving (Eq, Show, Read, Typeable)
