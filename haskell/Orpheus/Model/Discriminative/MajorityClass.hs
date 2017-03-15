module Orpheus.Model.Discriminative.MajorityClass where

import qualified Data.HashMap.Lazy as HM
import Data.Hashable

--------------------------------------------------------------------------------
--                          Majority Class Classifier                         --
--------------------------------------------------------------------------------

majorityClass :: (Eq y, Hashable y) => [(x,y)] -> (x -> y)
majorityClass = majorityClassLearn

majorityClassLearn :: (Eq y, Hashable y) => [(x,y)] -> (x -> y)
majorityClassLearn ds
  = let counts = foldr (\(_,y) hashmap -> HM.insertWith (+) y (1::Int) hashmap)
                       HM.empty
                       ds
        (c:cs) = HM.toList counts
    in  const . fst $ foldr (\n@(_,v) o@(_,v') -> if v > v' then n else o) c cs
