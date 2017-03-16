module Orpheus.Model.Discriminative where

import Orpheus.Data.Music.Context
import Orpheus.DataSet

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

data Classifier x y model
  = Classifier { train   :: [(x,y)] -> model
               , predict :: model -> x -> y  }

type Ctx = (KeySig,TimeSig)

-- the string here is just a nice label for errors and printing
classifiers :: a -- [(String,Classifier [Score Ctx] Composer)]
classifiers = undefined --  [("Majority Class",majorityClass)]

-- takes in a labelled dataset, runs the classifiers on it, and returns the
-- classifiers correct and incorrect
runClassifiers :: DataSet -> IO [(String,Int,Int)]
runClassifiers ds
  = forM classifiers $ \(name,_) -> do
      putStrLn $ "Running " ++ name ++ "..."
      return (name,0,0)
  where (train,test) = trainTestPartition ds

classifierSummary :: [(Score Ctx, Composer)] -> IO ()
classifierSummary dataset = do
  results <- runClassifiers dataset
  forM_ results $ \(name,right,wrong) ->
    putStrLn $
      unwords [name,": (",show right,",",show wrong,") : "
              , show (((fromIntegral right) / (fromIntegral (right + wrong))) :: Double)
              ]
