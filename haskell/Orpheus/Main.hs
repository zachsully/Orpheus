--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Main
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
-- |
--
--------------------------------------------------------------------------------

module Main where

import Orpheus.Model.Discriminative
import Orpheus.Data.DataSet
import Orpheus.Data.FeatureSet
import Data.Monoid
import Data.MusicXML.Parser
import Options.Applicative
import qualified Data.HashMap.Lazy      as HM
import qualified Data.Vector.Generic    as G
import qualified Data.Vector.Unboxed    as U
import qualified Orpheus.Hakaru.Train   as Train
import qualified Orpheus.Hakaru.Predict as Predict


--------------------------------------------------------------------------------
--                             Executable Options                             --
--------------------------------------------------------------------------------
data Mode
  = Parse FilePath (Maybe FilePath)
  | Feature (Maybe String)
  | Total
  | DataSummary
  | Run FilePath Int (Maybe FilePath)
  deriving (Show,Eq)

data Options = Options { mode :: Mode }
  deriving (Show,Eq)

parseTest :: Parser Mode
parseTest = Parse
        <$> strArgument (metavar "INPUT" <> help "input musical file")
        <*> optional (strArgument (metavar "OUTPUT" <> help "xml file output"))

parseFeature :: Parser Mode
parseFeature = Feature
           <$> optional (strArgument
                          (  metavar "SEED"
                          <> help "random seed for feature permutation")
                        )

parseDataSummary :: Parser Mode
parseDataSummary = pure DataSummary

parseRun :: Parser Mode
parseRun = Run
       <$> strArgument (metavar "FEATURE_SET" <> help "input of labelled feature set")
       <*> (read <$> strArgument (metavar "NUM_CAT" <> help "number of categories in feature set"))
       <*> optional (strArgument (metavar "HAKARU_PROG"
                                 <> help "hakaru classifier program"))

parseTotal :: Parser Mode
parseTotal = pure Total

options' :: Parser Mode
options' = subparser
  $  (command "parse" (info (helper <*> parseTest)
                            (progDesc "xml parser test")))
  <> (command "feature" (info (helper <*> parseFeature)
                              (progDesc "extract features from dataset")))
  <> (command "total" (info (helper <*> parseTotal)
                              (progDesc "count number of notes")))
  <> (command "summary"
              (info (helper <*> parseDataSummary)
                      (progDesc "print a summary of xml data")))

  <> (command "run"
              (info (helper <*> parseRun)
                      (progDesc "given a feature set and hakaru program, classify")))

parseOpts :: IO Mode
parseOpts = execParser
          $ info (helper <*> options')
          $ fullDesc <> progDesc "Orpheus: disect musical scores"


--------------------------------------------------------------------------------
--                                  MAIN                                      --
--------------------------------------------------------------------------------

main :: IO ()
main = do
  m <- parseOpts
  case m of
    Parse fin fout -> do
      putStrLn "MODE: Parse"
      putStrLn $ "Parsing file " ++ fin ++ "..."
      score <- parseMusicXMLFile fin
      scoreSummary score
      case fout of
        Nothing -> putStrLn . show $ score
        Just f -> writeFile f . show $ score

    Feature mseed -> do
      putStrLn "MODE: Feature..."
      putStrLn "Parsing dataset..."
      ds <- getDataSet
      let ds' = case mseed of
                  Just x  -> rPermute (read x) (length ds) ds
                  Nothing -> rPermute 0 (length ds) ds
      putStrLn "Writing dataset/feature/keysig.csv"
      writeBernoulliFeatureSet "dataset/feature/keysig.csv" (featureKeySig ds')
      putStrLn "Writing dataset/feature/timesig.csv"
      writeBernoulliFeatureSet "dataset/feature/timesig.csv" (featureTimeSig ds')
      putStrLn "Writing dataset/feature/primitive.csv"
      writeMultinomialFeatureSet "dataset/feature/primitive.csv" (featurePrimitive ds')
      putStrLn "Writing dataset/feature/all.csv"
      writeMultinomialFeatureSet "dataset/feature/all.csv" (featureAll ds')

    Total -> do
      ds <- getDataSet
      let infos = fmap (\a@(_,c) -> (c,bucketPrimitive a)) ds
          cAndC = fmap (\(c,x) -> (c,sum . HM.elems $ x)) infos
          countC = foldr (\(c,f) (x,y,z) -> case c of
                                              Bach -> (f+x,y,z)
                                              Beethoven -> (x,f+y,z)
                                              Horetzky -> (x,y,z+f)
                         )
                         (0,0,0)
                         cAndC
      putStrLn . show $ countC

    DataSummary -> do
      putStrLn "MODE: Summary..."
      putStrLn "Parsing dataset..."
      ds <- getDataSet
      datasetSummary ds
      classifierSummary ds

    Run fp numCategories _ -> do
      putStrLn "MODE: Run..."
      putStrLn $ "Parsing feature set: " ++ fp
      fs <- readBernoulliFeatureSet fp
      let (trainSet,testSet) = trainTestPartition fs
          numFeatures = length . fst . head $ fs
          model = Train.prog numCategories numFeatures
                    (G.fromList
                    $ fmap (\(f,c) -> (G.fromList f
                                      , featureComposer c)
                           )
                           trainSet
                    )
          predictions = fmap (\(fts, c) ->
                               ( Predict.prog numCategories numFeatures model
                               $ G.convert $ U.fromList fts
                               , c
                               )
                             )
                             testSet
          (c,t) = foldr (\(pred,actual) (correct,total) ->
                          if pred == (featureComposer actual)
                          then (correct + 1, total + 1)
                          else (correct, total + 1)
                        )
                        (0,0)
                        predictions
      putStrLn $ concat ["Correct: ",show c," out of ",show t]
      putStrLn $ concat ["Accuracy: ",show (c/t),"\n"]
      putStrLn $ show model
