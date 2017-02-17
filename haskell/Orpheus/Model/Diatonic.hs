{-# LANGUAGE DataKinds,
             FlexibleContexts #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.13
-- |
-- Module      :  Orpheus.Model.Diatonic
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing a probabilistic model for harmonic musical data
--
--------------------------------------------------------------------------------

module Orpheus.Model.Diatonic where

import Orpheus.Data.Music.Diatonic

import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT

import Prelude hiding ((>>=),(==),(/),fromRational)

measMusic
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HVoice)
measMusic _ _ _ _ _ = undefined

mPrimitive :: (ABT Term abt) => abt '[] ('HMeasure HPrimitive)
mPrimitive = undefined
  -- dirac $ datum_ (hRest hDWhole dFalse)

--------------------------------------------------------------------------------
catDuration
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HAccidental)
catDuration = undefined
  -- categorical prior >>= \c ->
  --   if_ (nat_ 0 == c)
  --       (dirac (datum_ hDWhole))
  --       (if_ (nat_ 1 == c)
  --            (dirac (datum_ hWhole))
  --            (if_ (nat_ 2 == c)
  --                 (dirac (datum_ hHalf))
  --                 (if_ (nat_ 3 == c)
  --                      (dirac (datum_ hQuarter))
  --                      (if_ (nat_ 4 == c)
  --                           (dirac (datum_ hEighth))
  --                           (if_ (nat_ 5 == c)
  --                                (dirac (datum_ hSixteenth))
  --                                (dirac (datum_ hG)))))))
-- hDWhole
-- hWhole
-- hHalf
-- hQuarter
-- hEighth
-- hSixteenth
-- hThirtySecond
-- hSixtyFourth
-- hOneTwentyEighth
-- hTwoFiftySixth

mUniformDuration
  :: (ABT Term abt)
  => abt '[] ('HMeasure HDuration)
mUniformDuration = undefined

--------------------------------------------------------------------------------
dirAccidental
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HAccidental)
dirAccidental prior =
  dirichlet prior >>= \ps ->
    catAccidental ps

catAccidental
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HAccidental)
catAccidental prior =
  categorical prior >>= \c ->
    if_ (nat_ 0 == c)
        (dirac (datum_ hSharp))
        (if_ (nat_ 1 == c)
             (dirac (datum_ hFlat))
             (dirac (datum_ hNatural)))


--------------------------------------------------------------------------------
dirPitchclass
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HPitchclass)
dirPitchclass prior =
  dirichlet prior >>= \ps ->
    catPitchclass ps

catPitchclass
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HPitchclass)
catPitchclass prior =
  categorical prior >>= \c ->
    if_ (nat_ 0 == c)
        (dirac (datum_ hA))
        (if_ (nat_ 1 == c)
             (dirac (datum_ hB))
             (if_ (nat_ 2 == c)
                  (dirac (datum_ hC))
                  (if_ (nat_ 3 == c)
                       (dirac (datum_ hD))
                       (if_ (nat_ 4 == c)
                            (dirac (datum_ hE))
                            (if_ (nat_ 5 == c)
                                 (dirac (datum_ hF))
                                 (dirac (datum_ hG)))))))

mUniformPitchclass
  :: (ABT Term abt)
  => abt '[] ('HMeasure HPitchclass)
mUniformPitchclass = catPitchclass (array (nat_ 7) (\_ -> prob_ 1 / prob_ 7))
