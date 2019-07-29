
module ICompiler where

import Data.Dynamic

data CompilerEvent
	= CompilerStopEvent
	| SourceFileUpdated String
	deriving (Eq, Show, Read, Typeable)
