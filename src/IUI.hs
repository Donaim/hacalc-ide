
module IUI where

import Data.Dynamic
import PatternT.Types

data UIEvent
	= UIStopEvent
	| CompilerParseError [ParseMatchError]
	| CompilerTokenizeError ParseError
	| Notify String
	| DebugLog String
	| ClearEvaluations
	| PushEvaluation Bool Int String [(String, String, String)]
	| EvaluationStarted Int
	| UISetPadding Int
	| UIRemoveEvaluation Int
	| UIChangeLimit Int
	deriving (Eq, Show, Read, Typeable)
