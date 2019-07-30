
module Util where

import Data.Dynamic

appendDyn :: (Typeable a) => a -> [Dynamic] -> [Dynamic]
appendDyn x xs = (toDyn x) : xs

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

padLeft :: Char -> Int -> String -> String
padLeft c n s = s ++ (replicate toappend c)
	where
	toappend = max (n - (length s)) 0
