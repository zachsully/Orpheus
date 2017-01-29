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

import Language.Hakaru.Sample
import Language.Hakaru.Types.Sing
import Language.Hakaru.Syntax.Value
import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.Pretty.Concrete
import Orpheus.Data.Music.Diatonic
import Orpheus.Model.Diatonic
import Orpheus.Data.Pretty
import qualified Text.PrettyPrint   as PP
import qualified System.Random.MWC  as MWC
import Control.Monad

main :: IO ()
main = do
  let m = runEvaluate $ triv $ mPitchclass
  gen <- MWC.createSystemRandom
  forever $ illustrate (SMeasure sPitchclass) gen m

illustrate :: Sing a -> MWC.GenIO -> Value a -> IO ()
illustrate (SMeasure s) g (VMeasure m) = do
    x <- m (VProb 1) g
    case x of
      Just (samp, _) -> illustrate s g samp
      Nothing        -> illustrate (SMeasure s) g (VMeasure m)

illustrate _ _ x = render x

render :: Value a -> IO ()
render = putStrLn . PP.renderStyle PP.style {PP.mode = PP.LeftMode} . prettyValue
