module Orpheus.Data.Feature where

import qualified Data.Set as Set
import Orpheus.DataSet
import Orpheus.Data.Music.Context

-- type DataSet = [(Score (KeySig,TimeSig), Composer)]
uniqueKeySig :: DataSet -> Set.Set KeySig
uniqueKeySig = foldr (\(Score ps,_) sigs -> (uniqueKeySig' ps) `Set.union` sigs)
                     Set.empty
  where uniqueKeySig' = foldr (\(Part (Voice vs)) sigs -> (uniqueKeySig'' vs) `Set.union` sigs)
                              Set.empty
        uniqueKeySig'' = foldr (\((key,_),_) sigs -> Set.insert key sigs)
                               Set.empty

uniqueTimeSig :: DataSet -> Set.Set TimeSig
uniqueTimeSig = foldr (\(Score ps,_) sigs -> (uniqueTimeSig' ps) `Set.union` sigs)
                     Set.empty
  where uniqueTimeSig' = foldr (\(Part (Voice vs)) sigs -> (uniqueTimeSig'' vs) `Set.union` sigs)
                              Set.empty
        uniqueTimeSig'' = foldr (\((_,time),_) sigs -> Set.insert time sigs)
                               Set.empty


uniquePrimitive :: DataSet -> Set.Set Primitive
uniquePrimitive = foldr (\(Score ps,_) sigs -> (uniquePrimitive' ps) `Set.union` sigs)
                     Set.empty
  where uniquePrimitive' = foldr (\(Part (Voice vs)) sigs -> (uniquePrimitive'' vs) `Set.union` sigs)
                                 Set.empty
        uniquePrimitive'' = foldr (\((_,_),prims) sigs ->
                                   foldr (\ss acc -> foldr Set.insert Set.empty ss)
                                         Set.empty
                                         prims)
                                  Set.empty
