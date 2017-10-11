module Analyze.Series where

import Data.Vector as V
import Data.Proxy

as :: a
as = undefined

data Series k v = Series
    { _seriesKeys :: !(Vector k)
    , _seriesValues :: !(Vector (Maybe v))
    }

-- Accessing series data and lookup

countKeys :: Series k v -> Int
countKeys (Series k _) = V.length k

countValues :: Series k v -> Int
countValues (Series _ v) = V.length v

firstKey :: Series k v -> k
firstKey (Series k _)
	| (V.length k) == 0 = error "There are no keys to extract"
	| otherwise         = V.head k

firstValue :: Series k v -> Maybe v
firstValue (Series _ v)
	| (V.length v) == 0 = error "There are no values to extract"
	| otherwise         = V.head v

get :: Eq k => k -> Series k v -> Maybe v
get key (Series k v) = case (V.elemIndex key k) of
    Nothing -> Nothing
    Just keyIndex -> (v ! keyIndex)

