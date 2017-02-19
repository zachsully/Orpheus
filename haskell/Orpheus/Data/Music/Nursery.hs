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

rhymes :: [Voice]
rhymes =
  [ maryHadALittleLamb
  , baBaBlackSheep
  , twinkleTwinkleLittleStar
  ]

maryHadALittleLamb :: Voice
maryHadALittleLamb =
  let e4 = Note E empty cMajor commonTime 4 Quarter False
      e2 = Note E empty cMajor commonTime 4 Half False

      d4 = Note D empty cMajor commonTime 4 Quarter False
      d2 = Note D empty cMajor commonTime 4 Half False

      c4 = Note C empty cMajor commonTime 4 Quarter False
      c1 = Note C empty cMajor commonTime 4 Whole False

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

baBaBlackSheep :: Voice
baBaBlackSheep =
  let g4 = Note G empty cMajor commonTime 4 Quarter False
      d4 = Note D empty cMajor commonTime 5 Quarter False
      r4 = Rest commonTime Quarter False
      c4 = Note C empty cMajor commonTime 5 Quarter False
      b4 = Note B empty cMajor commonTime 4 Quarter False
      a4 = Note A empty cMajor commonTime 4 Quarter False
      e8 = Note E empty cMajor commonTime 5 Eighth  False
      d8 = Note D empty cMajor commonTime 5 Eighth  False
      b8 = Note B empty cMajor commonTime 4 Eighth  False
      c8 = Note C empty cMajor commonTime 5 Eighth  False
  in Voice . fromList . fmap singleton $
       [ g4, g4, d4, d4
       , e8, e8, e8, e8, d4, r4
       , c4, c4, b4, b4
       , a4, a4, g4, r4

       , d4, d8, d8, c4, c4
       , b4, b8, b8, a4, r4
       , d4, d8, d8, c8, c8, c8, c8
       , b4, b8, b8, a4, r4

       , g4, g4, d4, d4
       , e8, e8, e8, e8, d4, r4
       , c4, c4, b4, b4
       , a4, a4, Note G empty cMajor commonTime 4 Half False
       ]

twinkleTwinkleLittleStar :: Voice
twinkleTwinkleLittleStar =
  let c4 = Note C empty cMajor commonTime 4 Quarter False
      c2 = Note C empty cMajor commonTime 4 Half False
      g4 = Note G empty cMajor commonTime 4 Quarter False
      g2 = Note G empty cMajor commonTime 4 Half False
      a4 = Note A empty cMajor commonTime 4 Quarter False
      f4 = Note F empty cMajor commonTime 4 Quarter False
      e4 = Note E empty cMajor commonTime 4 Quarter False
      d4 = Note D empty cMajor commonTime 4 Quarter False
      d2 = Note D empty cMajor commonTime 4 Half False
  in Voice . fromList . fmap singleton $
       [ c4, c4, g4, g4
       , a4, a4, g2
       , f4, f4, e4, e4
       , d4, d4, c2

       , g4, g4, f4, f4
       , e4, e4, d2
       , g4, g4, f4, f4
       , e4, e4, d2

       , c4, c4, g4, g4
       , a4, a4, g2
       , f4, f4, e4, e4
       , d4, d4, c2
       ]
