
module IUI where

import Data.Dynamic
import PatternT.Types

data UIEvent
	= CompilerParseError [ParseMatchError]
	| ReaderNotify String
	| CompilerNotify String
	| DebugLog String
	deriving (Eq, Show, Read, Typeable)
