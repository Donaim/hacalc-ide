
module ICompiler where

import Data.Dynamic

data CompilerEvent
	= CompilerStopEvent
	| SourceFileUpdated String
	| AppendEvaluation String
	deriving (Eq, Show, Read, Typeable)
