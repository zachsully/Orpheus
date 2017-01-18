--------------------------------------------------------------------------------
--                                                                  2017.01.11
-- |
-- Module      :  Orpheus.Data.Music
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing Musical Data
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music where

import Language.Hakaru.Types.DataKind

{-
  We need two representations, one in Haskell and one in Hakaru
-}       
--------------------------------------------
data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord,Enum)

type HPitchclass =
  'HData ('TyCon "Pitchclass") '[ '[], '[], '[], '[], '[], '[], '[] ]
                                {- A |  B |  C |  D |  E |  F |  G -}



--------------------------------------------
data Accidental
  = Sharp
  | Flat
  | Natural
  deriving (Show,Eq,Ord,Enum)

type HAccidental
  = 'HData ('TyCon "Accidental") '[ '[], '[], '[] ]
                             {- Sharp | Flat | Natural -}



--------------------------------------------
data Duration
  = Whole
  | Half Duration
  | Dot  Duration
  deriving (Show,Eq,Ord,Enum)

type HDuration
  = 'HData ('TyCon "Duration") '[ '[] , '[ 'I ] , '[ 'I ] ]
                              {- Whole |   H D  |    D D -}


--------------------------------------------
data Primitive
  = Note Pitchclass [Accidental] Int Duration -- the Int is the octive
  | Rest Duration
  deriving (Show,Eq,Ord)

type HPrimitive
  = 'HData ('TyCon "Primitive") '[ '[ HPitchclass, HList HAccidental, 'HInt, HDuration ] , '[ HDuration ] ]
                                 {-                    Note                              |      Rest     -}

--------------------------------------------
data Music
  = Prim Primitive
  | Seq  Music Music
  | Par  Music Music
  deriving (Show,Eq,Ord)

type HMusic
  = 'HData ('TyCon "Music") '[ '[ HPrimitive ] , '[ 'I, 'I ] , '[ 'I, 'I ] ]
                             {-    Prim        |   Seq M M   |  Par M M   -}
