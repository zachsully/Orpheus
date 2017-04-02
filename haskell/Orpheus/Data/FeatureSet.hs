module Orpheus.Data.FeatureSet where

import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as Set
import Orpheus.Data.DataSet
import Orpheus.Data.Music.Context
import Numeric.Natural
import Text.CSV (parseCSVFromFile)
import Data.Monoid ((<>))

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

uniqueDuration :: DataSet -> Set.Set Duration
uniqueDuration = error "TODO{uniqueDuration}"

uniquePitchclass :: DataSet -> Set.Set Pitchclass
uniquePitchclass = error "TODO{uniquePitchclass}"

--------------------------------------------------------------------------------
--                              Feature Buckets                               --
--------------------------------------------------------------------------------
{-
Buckets traverse an instance of the score and return a HashMap of counts of
some feature
-}
bucketKeySig
  :: (Score (KeySig,TimeSig), Composer)
  -> HM.HashMap KeySig Natural
bucketKeySig (Score ps,_) = featureKeySig' ps
  where featureKeySig' = foldr (\(Part (Voice vs)) sigs ->
                                HM.unionWith (+) (featureKeySig'' vs) sigs)
                               HM.empty
        featureKeySig'' = foldr (\((key,_),_) sigs -> HM.insertWith (+) key 1 sigs)
                                HM.empty

bucketTimeSig
  :: (Score (KeySig,TimeSig), Composer)
  -> HM.HashMap TimeSig Natural
bucketTimeSig (Score ps,_) = featureTimeSig' ps
  where featureTimeSig' = foldr (\(Part (Voice vs)) sigs ->
                                HM.unionWith (+) (featureTimeSig'' vs) sigs)
                               HM.empty
        featureTimeSig'' = foldr (\((_,time),_) sigs -> HM.insertWith (+) time 1 sigs)
                                HM.empty

bucketPrimitive
  :: (Score (KeySig,TimeSig), Composer)
  -> HM.HashMap Primitive Natural
bucketPrimitive (Score ps,_) = featurePrimitive' ps
  where featurePrimitive' = foldr (\(Part (Voice vs)) sigs ->
                                    HM.unionWith (+) (featurePrimitive'' vs) sigs
                                  )
                                  HM.empty
        featurePrimitive'' = foldr (\((_,_),prims) hm ->
                                     foldr (\ss hm' ->
                                             foldr (\p -> HM.insertWith (+) p (1::Natural))
                                                   hm'
                                                   ss
                                           )
                                           hm
                                           prims
                                   )
                                   HM.empty

bucketDuration
  :: (Score a, Composer)
  -> HM.HashMap Duration Natural
bucketDuration = error "TODO{bucketDuration}"

bucketPitchclass
  :: (Score a, Composer)
  -> HM.HashMap Pitchclass Natural
bucketPitchclass = error "TODO{bucketPitchclass}"

--------------------------------------------------------------------------------
--                               Feature Maps                                 --
--------------------------------------------------------------------------------
{-
After obtaining the set of unique features, traverse the dataset again an obtain
a the counts of these unique features
-}

type FeatureSet a b = [([a],b)]

featureComposer :: Composer -> Int
featureComposer Bach      = 0
featureComposer Beethoven = 1
featureComposer Horetzky  = 2

featureKeySig :: DataSet -> FeatureSet Bool Composer
featureKeySig ds =
  fmap (\e@(_,c) ->
         let counts' = bucketKeySig e
         in ((fmap (\k -> case HM.lookup k counts' of
                           Just _  -> True
                           Nothing -> False
                  )
                  unique
             )
            ,c)
       )
       ds
  where unique = Set.toList . uniqueKeySig $ ds

featureTimeSig :: DataSet -> FeatureSet Bool Composer
featureTimeSig ds = fmap (\e@(_,c) ->
                          let counts' = bucketTimeSig e
                          in ((fmap (\k -> case HM.lookup k counts' of
                                            Just _  -> True
                                            Nothing -> False
                                   )
                                   unique
                              )
                             ,c)
                         )
                         ds
  where unique = Set.toList . uniqueTimeSig $ ds

featurePrimitive :: DataSet -> FeatureSet Natural Composer
featurePrimitive ds = fmap (\e@(_,c) ->
                             let counts' = bucketPrimitive e
                             in ((fmap (\k -> case HM.lookup k counts' of
                                               Just x  -> x
                                               Nothing -> 0
                                      )
                                      unique
                                 )
                                ,c)
                          )
                          ds
  where unique = Set.toList . uniquePrimitive $ ds

featureDuration :: DataSet -> FeatureSet Natural Composer
featureDuration = error "TODO{featureDuration}"

featurePitchclass :: DataSet -> FeatureSet Natural Composer
featurePitchclass = error "TODO{featurePitchclass}"

featureAll :: DataSet -> FeatureSet Natural Composer
featureAll ds = fmap (\e@(_,c) ->
                       let cKeySig  = bucketKeySig e
                           cTimeSig = bucketTimeSig e
                           cPrims   = bucketPrimitive e
                       in ((fmap (\k -> case HM.lookup k cKeySig of
                                          Just x  -> x
                                          Nothing -> 0
                                 )
                                 uKeySig
                           ) ++
                           (fmap (\k -> case HM.lookup k cTimeSig of
                                          Just x  -> x
                                          Nothing -> 0
                                 )
                                 uTimeSig
                           ) ++
                           (fmap (\k -> case HM.lookup k cPrims of
                                          Just x  -> x
                                          Nothing -> 0
                                 )
                                 uPrimitive
                           )
                          ,c)
                     )
                     ds
  where uKeySig    = Set.toList . uniqueKeySig    $ ds
        uTimeSig   = Set.toList . uniqueTimeSig   $ ds
        uPrimitive = Set.toList . uniquePrimitive $ ds

--------------------------------------------------------------------------------
--                                     IO                                     --
--------------------------------------------------------------------------------
{-
Once we have obtained the feature maps, we need support for reading and writing
these to files, we do not want to do this every time the program is run
-}

readComposer :: String -> Composer
readComposer "0" = Bach
readComposer "1" = Beethoven
readComposer "2" = Horetzky

readBool :: String -> Bool
readBool "0" = False
readBool "1" = True

writeComposer :: Composer -> String
writeComposer Bach      = "0"
writeComposer Beethoven = "1"
writeComposer Horetzky  = "2"

writeBool :: Bool -> String
writeBool False = "0"
writeBool True  = "1"


intersperce :: String -> [String] -> String
intersperce x []     = []
intersperce x (y:[]) = y
intersperce x (y:ys) = concat [y,x,intersperce x ys]

writeBernoulliFeatureSet :: FilePath -> FeatureSet Bool Composer -> IO ()
writeBernoulliFeatureSet fp fs =
  let csv = intersperce "\n"
          $ fmap (\(fts,c) -> intersperce ","
                           $  (fmap writeBool fts)
                           <> [writeComposer $ c]
                 )
                 fs
  in writeFile fp (csv ++ "\n")

writeMultinomialFeatureSet :: FilePath -> FeatureSet Natural Composer -> IO ()
writeMultinomialFeatureSet = error "TODO{writeMultinomialFeatureSet}"

readBernoulliFeatureSet :: FilePath -> IO (FeatureSet Bool Composer)
readBernoulliFeatureSet fp = do
  erows <- parseCSVFromFile fp
  case erows of
    Left x -> error $ show x
    Right rows -> return
               $  fmap (\cols -> let len = length cols in
                         ( fmap readBool . take (len-1) $ cols
                         , readComposer . head . drop (len-1) $ cols
                         )
                       )
                       (init rows)

readMultinomialFeatureSet :: FilePath -> IO (FeatureSet Natural Composer)
readMultinomialFeatureSet = error "TODO{readMultinomialFeatureSet}"
