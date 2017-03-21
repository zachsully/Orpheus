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
import Orpheus.DataSet
import Orpheus.Data.Feature
import Data.Monoid
import Data.MusicXML.Parser
import Options.Applicative
import qualified Data.HashMap.Lazy as HM


--------------------------------------------------------------------------------
--                             Executable Options                             --
--------------------------------------------------------------------------------
data Mode
  = Parse FilePath (Maybe FilePath)
  | Feature (Maybe String)
  | Total
  | DataSummary
  | Run FilePath FilePath
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
       <$> strArgument (metavar "FEATURE SET" <> help "input of labelled feature set")
       <*> strArgument (metavar "HK PROG" <> help "hakaru classifier program")

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
      writeFeatureSet "dataset/feature/keysig.csv" (featureKeySig ds')
      putStrLn "Writing dataset/feature/timesig.csv"
      writeFeatureSet "dataset/feature/timesig.csv" (featureTimeSig ds')
      putStrLn "Writing dataset/feature/primitive.csv"
      writeFeatureSet "dataset/feature/primitive.csv" (featurePrimitive ds')
      putStrLn "Writing dataset/feature/all.csv"
      writeFeatureSet "dataset/feature/all.csv" (featureAll ds')

    Total -> do
      ds <- getDataSet
      let infos = fmap (\a@(_,c) -> (c,bucketPrimitive a)) ds
          cAndC = fmap (\(c,x) -> (c,sum . HM.elems $ x)) infos
          countC = foldr (\(c,f) (x,y,z) -> case c of
                                              Bach -> (f+x,y,z)
                                              Beethoven -> (x,f+y,z)
                                              Horetzky -> (x,y,z+f)) (0,0,0) cAndC
      putStrLn . show $ countC

    DataSummary -> do
      putStrLn "MODE: Summary..."
      putStrLn "Parsing dataset..."
      ds <- getDataSet
      datasetSummary ds
      classifierSummary ds

    Run _ _ -> do
      putStrLn "MODE: Run..."
      putStrLn "TODO"
      -- > This program should run a hakaru classifier
      -- > on a feature set
