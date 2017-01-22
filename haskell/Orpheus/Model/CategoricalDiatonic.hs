{-# LANGUAGE DataKinds,
             FlexibleContexts #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.13
-- |
-- Module      :  Orpheus.Model.CatChromatic
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing a probabilistic model for harmonic musical data
--
--------------------------------------------------------------------------------

module Orpheus.Model.CategoricalDiatonic where

import Orpheus.Data.Music.Diatonic

import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT

import Prelude hiding ((>>=),(==))

measMusic
  :: (ABT Term abt)
  => abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HArray 'HProb)
  -> abt '[] ('HMeasure HMusic)
measMusic m_prior p_prior d_prior a_prior pc_prior =
  dirichlet m_prior >>= \m_ps ->
    categorical m_ps >>= \m_c ->
      undefined
      -- if_ (nat_ 0 == m_c)
      --     (dirac (datum_ hSharp))
          -- (if_ (nat_ 1 == m_c)
          --      (dirac (datum_ hFlat))
          --      (dirac (datum_ hNatural)))



measPrimitive :: (ABT Term abt) => abt '[] ('HMeasure HPrimitive)
measPrimitive = undefined

measDuration :: (ABT Term abt) => abt '[] ('HMeasure HDuration)
measDuration = undefined


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
