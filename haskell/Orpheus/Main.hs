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

import Data.MusicXML.Parser
import Orpheus.Data.Music.Context
import Orpheus.DataSet

import qualified Text.PrettyPrint   as PP
import qualified System.Random.MWC  as MWC
import System.Environment
-- import Data.Text (pack)

main :: IO ()
main = do
  (fp:[]) <- getArgs
  xmlParseTest fp
  datasetSummary

xmlParseTest :: FilePath -> IO ()
xmlParseTest fp = do
  score <- parseMusicXMLFile fp
  print score
  case score of
    Score parts -> do
      putStrLn $ "Number of parts: " ++ (show . length $ parts)
      case head parts of
        Part (Voice xs) -> putStrLn $ "Number of measures: " ++ (show . length $ xs)

--------------------------------------------------------------------------------
--                        Learning Discriminitive Models                      --
--------------------------------------------------------------------------------


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
