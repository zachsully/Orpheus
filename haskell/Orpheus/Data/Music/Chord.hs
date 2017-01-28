--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Data.Music.Chord
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Data description for Chords and Chordal harmony
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music.Chord where

import Orpheus.Data.Music.Diatonic

{-
  A Chord like CMaj signals a "Chord Space" (see Donya Quick) which includes
  every possible way C, E, and G in terms of rhythm and octive.

  Chords describe and infinite set of tones
-}

data Chord = Chord Pitchclass Quality

data Quality
  = Octive   -- tonics only
  | Power    -- tonic and dominant
  | Triadic  -- tonic, 3rd, and dominant
  | Sus2     -- tonic, subtonic, 3rd, and dominant
  | Sus4     -- tonic, 3rd, subdominant, and dominant
  | Sus6     -- tonic, 3rd, dominant, 6th
  | Sus7     -- tonic, 3rd, dominant, 7th
  | Flat7    -- tonic, 3rd, dominant, flat 7th
  deriving (Show,Eq,Ord)
