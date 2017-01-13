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
import Language.Hakaru.Measure

measMusic :: Measure Music
measMusic = undefined

measPrimitive :: Measure Primitive
measPrimitive = undefined

measDuration :: Measure Duration
measDuration = undefined

measAccidental :: Measure Accidental
measAccidental = undefined

measPitchclass :: Measure Pitchclass
measPitchclass = undefined
