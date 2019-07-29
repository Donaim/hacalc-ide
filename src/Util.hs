
module Util where

import Data.Dynamic

appendDyn :: (Typeable a) => a -> [Dynamic] -> [Dynamic]
appendDyn x xs = (toDyn x) : xs
