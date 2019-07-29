
module Util where

import Data.Dynamic

appendDyn :: (Typeable a) => a -> [Dynamic] -> [Dynamic]
appendDyn x xs = (toDyn x) : xs

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
