-- Establishing a bijection between the values of the type a and integers, with
-- the operations to retrieve the value given its key,
-- to find the key for the existing value, and to extend the
-- bijection with a new association.

-- The type 'a' of values should at least permit equality comparison;
-- In the present implementation, we require 'a' to be a member
-- of Ord.

-- There are many ways to implement bi-maps, for example, using hash tables,
-- or maps.
-- Our implementation uses Data.Map and Data.IntMap to record
-- both parts of the association.

module BiMap (
              BiMap, empty,
              lookupKey,
              lookupVal,
              insert,
              size,
             )
    where

import qualified Data.Map    as M
import qualified Data.IntMap as IM

data BiMap a = BiMap (M.Map a Int) (IM.IntMap a)

instance Show a => Show (BiMap a) where
  show (BiMap _ m) =  "BiMap" ++ show (IM.toList m)

-- Find a key for a value
lookupKey :: Ord a => a -> BiMap a -> Maybe Int
lookupKey v (BiMap m _) = M.lookup v m

-- Find a value for a key
lookupVal :: Int -> BiMap a -> a
lookupVal k (BiMap _ m) = m IM.! k

-- Insert the value and return the corresponding key
-- and the new map
insert :: Ord a => a -> BiMap a -> (Int, BiMap a)
insert v (BiMap m im) = (k, BiMap m' im')
 where m'  = M.insert v k m
       im' = IM.insert k v im
       k   = IM.size im

empty :: BiMap a
empty = BiMap M.empty IM.empty

size :: BiMap a -> Int
size (BiMap _ m) = IM.size m
