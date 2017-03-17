module Orpheus.Data.Feature where

import qualified Data.Set as Set
import Orpheus.DataSet
import Orpheus.Data.Music.Context

-- type DataSet = [(Score (KeySig,TimeSig), Composer)]
uniqueKeySig :: DataSet -> Set.Set KeySig
uniqueKeySig = foldr (\(Score ps,_) sigs -> (uniqueKeySig' ps) `Set.union` sigs)
                     Set.empty
  where uniqueKeySig' = undefined

uniqueTimeSig :: DataSet -> Set.Set TimeSig
uniqueTimeSig = foldr (\(Score parts,_) sigs -> sigs) Set.empty

uniquePrimitive :: DataSet -> Set.Set Primitive
uniquePrimitive = foldr (\(Score parts,_) sigs -> sigs) Set.empty
