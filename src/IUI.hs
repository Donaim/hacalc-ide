
module IUI where

import Data.Dynamic
import PatternT.Types

data UIEvent
	= CompilerParseError [ParseMatchError]
	| CompilerTokenizeError ParseError
	| ReaderNotify String
	| CompilerNotify String
	| DebugLog String
	| ResetEvaluations
	| PushEvaluation String [(String, String, String)]
	deriving (Eq, Show, Read, Typeable)
