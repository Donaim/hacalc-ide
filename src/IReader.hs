
module IReader where

import Data.Dynamic

data ReaderEvent
	= ReaderStopEvent
	| SourceFilepathChanged String
	deriving (Eq, Show, Read, Typeable)


