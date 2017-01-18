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

measMusic :: (ABT Term abt)
          => abt '[] 'HArray 'HProb
          -> abt '[] ('HMeasure HMusic)
measMusic probs =
  -- categorical probs >>= \c ->
  --   case_ c
  --     [ Branch 0
  --     ]

measPrimitive :: (ABT Term abt) => abt '[] ('HMeasure HPrimitive)
measPrimitive = undefined

measDuration :: (ABT Term abt) => abt '[] ('HMeasure HDuration)
measDuration = undefined

measAccidental :: (ABT Term abt) => abt '[] ('HMeasure HAccidental)
measAccidental = undefined

measPitchclass :: (ABT Term abt) => abt '[] ('HMeasure HPitchclass)
measPitchclass = undefined
