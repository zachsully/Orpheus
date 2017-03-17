module Orpheus.Data.Feature where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import Orpheus.DataSet
import Orpheus.Data.Music.Context

--------------------------------------------------------------------------------
--                              Unique Features                               --
--------------------------------------------------------------------------------
{- There is some catamorphic abstraction here that I am not capturing. -}

uniqueKeySig :: DataSet -> Set.Set KeySig
uniqueKeySig = foldr (\(Score ps,_) sigs -> (uniqueKeySig' ps) `Set.union` sigs)
                     Set.empty
  where uniqueKeySig' = foldr (\(Part (Voice vs)) sigs ->
                                (uniqueKeySig'' vs) `Set.union` sigs)
                              Set.empty
        uniqueKeySig'' = foldr (\((key,_),_) sigs -> Set.insert key sigs)
                               Set.empty

uniqueTimeSig :: DataSet -> Set.Set TimeSig
uniqueTimeSig = foldr (\(Score ps,_) sigs ->
                        (uniqueTimeSig' ps) `Set.union` sigs)
                     Set.empty
  where uniqueTimeSig' = foldr (\(Part (Voice vs)) sigs ->
                                 (uniqueTimeSig'' vs) `Set.union` sigs)
                              Set.empty
        uniqueTimeSig'' = foldr (\((_,time),_) sigs -> Set.insert time sigs)
                               Set.empty


uniquePrimitive :: DataSet -> Set.Set Primitive
uniquePrimitive = foldr (\(Score ps,_) sigs ->
                          (uniquePrimitive' ps) `Set.union` sigs)
                     Set.empty
  where uniquePrimitive' = foldr (\(Part (Voice vs)) sigs ->
                                   (uniquePrimitive'' vs) `Set.union` sigs)
                                 Set.empty
        uniquePrimitive'' = foldr (\((_,_),prims) sigs ->
                                   foldr (\ss sigs' ->
                                           foldr Set.insert sigs' ss)
                                         sigs
                                         prims)
                                  Set.empty


--------------------------------------------------------------------------------
--                               Feature Maps                                 --
--------------------------------------------------------------------------------
{-
After obtaining the set of unique features, traverse the dataset again an obtain
a the counts of these unique features
-}

type FeatureSet = [[Int]]

featureComposer :: Composer -> Int
featureComposer Bach      = 1
featureComposer Beethoven = 2
featureComposer Horetzky  = 3

featureKeySig :: DataSet -> FeatureSet
featureKeySig ds = fmap (\e@(_,c) ->
                          let counts' = counts e
                          in (fmap (\k -> case HM.lookup k counts' of
                                 Just x  -> x
                                 Nothing -> 0) unique)
                          ++ [featureComposer c])
                        ds
  where counts :: (Score (KeySig,TimeSig), Composer) -> HM.HashMap KeySig Int
        counts (Score ps,_) = (featureKeySig' ps)
        featureKeySig' = foldr (\(Part (Voice vs)) sigs ->
                                HM.unionWith (+) (featureKeySig'' vs) sigs)
                               HM.empty
        featureKeySig'' = foldr (\((key,_),_) sigs -> HM.insertWith (+) key 1 sigs)
                                HM.empty
        unique = Set.toList . uniqueKeySig $ ds

featureTimeSig :: DataSet -> FeatureSet
featureTimeSig ds = fmap (\e@(_,c) ->
                          let counts' = counts e
                          in (fmap (\k -> case HM.lookup k counts' of
                                 Just x  -> x
                                 Nothing -> 0) unique)
                          ++ [featureComposer c])
                        ds
  where counts :: (Score (KeySig,TimeSig), Composer) -> HM.HashMap TimeSig Int
        counts (Score ps,_) = (featureTimeSig' ps)
        featureTimeSig' = foldr (\(Part (Voice vs)) sigs ->
                                HM.unionWith (+) (featureTimeSig'' vs) sigs)
                               HM.empty
        featureTimeSig'' = foldr (\((_,time),_) sigs -> HM.insertWith (+) time 1 sigs)
                                HM.empty
        unique = Set.toList . uniqueTimeSig $ ds

featurePrimitive :: DataSet -> FeatureSet
featurePrimitive ds = fmap (\e@(_,c) ->
                             let counts' = counts e
                             in (fmap (\k -> case HM.lookup k counts' of
                                               Just x  -> x
                                               Nothing -> 0
                                      )
                                      unique
                                ) ++ [featureComposer c]
                          )
                          ds
  where counts :: (Score (KeySig,TimeSig), Composer) -> HM.HashMap Primitive Int
        counts (Score ps,_) = featurePrimitive' ps
        featurePrimitive' = foldr (\(Part (Voice vs)) sigs ->
                                    HM.unionWith (+) (featurePrimitive'' vs) sigs
                                  )
                                  HM.empty
        featurePrimitive'' = foldr (\((_,_),prims) hm ->
                                     foldr (\ss hm' ->
                                             foldr (\p -> HM.insertWith (+) p (1::Int))
                                                   hm'
                                                   ss
                                           )
                                           hm
                                           prims
                                   )
                                   HM.empty
        unique = Set.toList . uniquePrimitive $ ds

--------------------------------------------------------------------------------
--                                     IO                                     --
--------------------------------------------------------------------------------
{-
Once we have obtained the feature maps, we need support for reading and writing
these to files, we do not want to do this every time the program is run
-}

writeFeatureSet :: FilePath -> FeatureSet -> IO ()
writeFeatureSet fp fs =
  let csv = foldr (\entry file ->
                    init (foldr (\f row -> show f ++ "," ++ row)
                                ""
                                entry) ++ "\n" ++ file)
                  []
                  fs
  in writeFile fp csv

readFeatureSet :: FilePath -> IO FeatureSet
readFeatureSet = undefined
