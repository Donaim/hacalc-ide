
module ICompiler where

import Data.Dynamic

data CompilerEvent
	= CompilerStopEvent
	| SourceFileUpdated String
	| AppendEvaluation String
	| CompilerRemoveEvalRecord Int
	deriving (Eq, Show, Read, Typeable)
