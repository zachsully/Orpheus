module Orpheus.Model.Discriminative where

import Orpheus.Model.Discriminative.MajorityClass
import Orpheus.Model.Type
import Orpheus.Data.Music.Context
import Orpheus.Data.DataSet

import Control.Monad

--------------------------------------------------------------------------------
--                        Learning Discriminitive Models                      --
--------------------------------------------------------------------------------
{-
Discriminitive Learning Goals:
> categorize piece
> categorize measure

TODO:
> data partitioning
> feature selection
> use or build SVM
-}

type Ctx = (KeySig,TimeSig)

-- the string here is just a nice label for errors and printing
-- classifiers :: [(String,Classifier [Score Ctx] Composer)]
-- classifiers = undefined --  [("Majority Class",majorityClass)]

-- takes in a labelled dataset, runs the classifiers on it, and returns the
-- classifiers correct and incorrect
runClassifiers :: (DataSet,DataSet) -> IO [(String,Int,Int)]
runClassifiers (trainSet,testSet) = do
  putStrLn $ "Training Majority Class..."
  let model = train majorityClass trainSet
  putStrLn $ "Running Majority Class..."
  let (right,wrong) = foldr (\(scr,c) (a,b) ->
                              if (predict majorityClass) model scr == c
                              then (succ a,b)
                              else (a,succ b))
                            (0,0)
                            testSet
  return [("Majority Class",right,wrong)]

classifierSummary :: DataSet -> IO ()
classifierSummary ds = do
  let (trainSet,testSet) = trainTestPartition ds
  results <- runClassifiers (trainSet,testSet)
  putStrLn "\nClassifier Summary:"
  putStrLn $ unwords ["  > Training Set: #",show (length trainSet)]
  putStrLn $ unwords ["  > Test Set: #"    ,show (length testSet) ]
  putStrLn . unCols $ ["  > Classifier","Right","Wrong","Percent"]
  forM_ results $ \(name,right,wrong) ->
    putStrLn $ "    " ++
      (unCols [name,show right,show wrong
              , show (((fromIntegral right) / (fromIntegral (right + wrong))) :: Double)
              ])
  where unCols = foldr (\x acc -> x ++ "\t" ++ acc) []
