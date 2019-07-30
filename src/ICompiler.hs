
module ICompiler where

import Data.Dynamic

data CompilerEvent
	= CompilerStopEvent
	| SourceFileUpdated String
	| AppendEvaluation String
	| CompilerRemoveEvalRecord Int
	| CompilerResetEvaluations -- REMOVE ALL
	deriving (Eq, Show, Read, Typeable)
