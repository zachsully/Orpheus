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
import qualified Text.PrettyPrint   as PP
import qualified System.Random.MWC  as MWC
import Control.Monad
import System.Environment
import Data.Lilypond.Pretty
import Orpheus.Data.Music.Nursery
import Data.Vector

main :: IO ()
-- main = do
--   let -- m  = runEvaluate $ triv $ mDuration
--       m2 = runEvaluate $ triv $ categorical (array (nat_ 2) (\_ -> prob_ 0.5))
--       m3 = runEvaluate $ triv $ geometric (prob_ 0.5)
--   gen <- MWC.createSystemRandom
--   forever $ illustrate (SMeasure SNat) gen m3
main = do
  (x:_) <- getArgs
  let index = (read x) :: Int
  putStrLn . prettyPrintScore . Score . singleton $
    Prelude.head . Prelude.drop index $ rhymes


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
