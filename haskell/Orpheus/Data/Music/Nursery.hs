--------------------------------------------------------------------------------
--                                                                  2017.02.10
-- |
-- Module      :  Orpheus.Data.Music.Nursery
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some Nursery Rhymes
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music.Nursery where

import Orpheus.Data.Music.Diatonic
import Data.Vector

maryHadALittleLamb :: Voice
maryHadALittleLamb =
  let e4 = Note E empty 4 Quarter False
      e2 = Note E empty 4 Half False

      d4 = Note D empty 4 Quarter False
      d2 = Note D empty 4 Half False

      c4 = Note C empty 4 Quarter False
      c1 = Note C empty 4 Whole False

  in Voice . fromList . fmap singleton $
       [ e4, d4, c4, d4
       , e4, e4, e2
       , d4, d4, d2
       , e4, e4, e2
       , e4, d4, c4, d4
       , e4, e4, e4, c4
       , d4, d4, e4, d4
       , c1
       ]
