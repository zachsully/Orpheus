--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Data.Music.Progression
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Data description for Western music progressions
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music.Progression where

data Intervals = I | II | III | IV | V | VI | VII
  deriving (Show,Eq,Ord)
