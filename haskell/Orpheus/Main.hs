{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
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

-- import Language.Hakaru.Sample
import Language.Hakaru.Types.Sing
import Language.Hakaru.Syntax.Value
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Pretty.Concrete
import Language.Hakaru.Pretty.Haskell as HK

import Data.Monoid
import Data.MusicXML.Parser
import Orpheus.Data.Music.Context
import Orpheus.Model.Discriminative.MajorityClass
import Orpheus.DataSet

import qualified Text.PrettyPrint   as PP
import qualified System.Random.MWC  as MWC
import Control.Monad
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

type Classifier x y = [(x,y)] -> (x -> y)
type Ctx = (KeySig,TimeSig)

-- the string here is just a nice label for errors and printing
classifiers :: [(String,Classifier [Score Ctx] Composer)]
classifiers = [("Majority Class",majorityClass)]

-- takes in a labelled dataset, runs the classifiers on it, and returns the
-- classifiers correct and incorrect
runClassifiers :: [(Score Ctx, Composer)] -> IO [(String,Int,Int)]
runClassifiers _
  = forM classifiers $ \(name,_) -> do
      putStrLn $ "Running " ++ name ++ "..."
      return (name,0,0)

classifierSummary :: [(Score Ctx, Composer)] -> IO ()
classifierSummary dataset = do
  results <- runClassifiers dataset
  forM_ results $ \(name,right,wrong) ->
    putStrLn $
      unwords [name,": (",show right,",",show wrong,") : "
              , show (((fromIntegral right) / (fromIntegral (right + wrong))) :: Double)
              ]


--------------------------------------------------------------------------------
--                        Sampling From Generative Model                      --
--------------------------------------------------------------------------------
{-
  mDuration is defined recursively and never returns a sample. Should this just
  be modelled with a geometric distribution instead?

  BUG: geometric will run into an infinite loop when drawing samples

  What does this mean for mMusic which is recursive in several ways?
-}

illustrate :: Sing a -> MWC.GenIO -> Value a -> IO ()
illustrate (SMeasure s) g (VMeasure m) = do
    x <- m (VProb 1) g
    case x of
      Just (samp, _) -> illustrate s g samp
      Nothing        -> illustrate (SMeasure s) g (VMeasure m)
illustrate _ _ x = render x

render :: Value a -> IO ()
render = putStrLn . PP.renderStyle PP.style {PP.mode = PP.LeftMode} . prettyValue


prettyProg
  :: (ABT Term abt)
  => abt '[] a
  -> IO ()
prettyProg abt =
  putStrLn $ PP.renderStyle PP.style {PP.mode = PP.LeftMode}
    (PP.cat [ PP.text ("prog = ")
         , PP.nest 2 (HK.pretty abt)
         ])
