
module IReader where

import Data.Dynamic

data ReaderEvent
	= ReaderStopEvent
	deriving (Eq, Show, Read, Typeable)


