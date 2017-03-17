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


--------------------------------------------------------------------------------
--                             Executable Options                             --
--------------------------------------------------------------------------------
data Mode
  = Test FilePath
  | Run
  deriving (Show,Eq)

data Options = Options { mode :: Mode }
  deriving (Show,Eq)

parseTest :: Parser Mode
parseTest = Test <$> strArgument (metavar "FILE" <> help "xml file to test")

parseRun :: Parser Mode
parseRun = pure Run

options :: Parser Options
options = Options <$> (parseTest <|> parseRun)

parseOpts :: IO Options
parseOpts = execParser
          $ info (helper <*> options)
          $ fullDesc <> progDesc "Orpheus, a musician, a poet"


--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- parseOpts
  case mode opts of
    Test fin -> do putStrLn $ "MODE: Test, " ++ fin ++ "..."
                   xmlParseTest fin
                   ds <- getDataSet
                   print $ uniqueKeySig ds
                   print $ uniqueTimeSig ds
                   print $ uniquePrimitive ds
    Run -> do putStrLn "MODE: Run..."
              putStrLn "Parsing dataset..."
              ds <- getDataSet
              datasetSummary ds
              classifierSummary ds

xmlParseTest :: FilePath -> IO ()
xmlParseTest fp = do
  score <- parseMusicXMLFile fp
  print score
  scoreSummary score
