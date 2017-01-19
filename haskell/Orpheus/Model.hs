{-# LANGUAGE DataKinds,
             FlexibleContexts #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.13
-- |
-- Module      :  Orpheus.Model
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing a probabilistic model for harmonic musical data
--
--------------------------------------------------------------------------------

module Orpheus.Model where

import Orpheus.Data.Music

import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT

measMusic :: (ABT Term abt)
          => abt '[] ('HArray 'HProb)
          -> abt '[] ('HMeasure HMusic)
measMusic probs = undefined
  -- categorical probs >>= \c ->
  --   case_ c
  --     [ Branch 0 ()
  --     , Branch 1 ()
  --     , Branch 2 ()
  --     ]

measPrimitive :: (ABT Term abt) => abt '[] ('HMeasure HPrimitive)
measPrimitive = undefined

measDuration :: (ABT Term abt) => abt '[] ('HMeasure HDuration)
measDuration = undefined

measAccidental :: (ABT Term abt) => abt '[] ('HMeasure HAccidental)
measAccidental = undefined

measPitchclass :: (ABT Term abt) => abt '[] ('HMeasure HPitchclass)
measPitchclass = undefined
