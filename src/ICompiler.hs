
module ICompiler where

import Data.Dynamic

data CompilerEvent
	= SourceFileUpdated String
	deriving (Eq, Show, Read, Typeable)
