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

data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord,Enum)

data Accidental
  = Sharp
  | Flat
  | Natural
  deriving (Show,Eq,Ord,Enum)

data Duration
  = Breve
  | Semibreve
  | Minim
  | Crotchet
  | Quaver
  | Semiquaver
  | Demisemiquaver
  deriving (Show,Eq,Ord,Enum)

data Primitive
  = Note Pitchclass [Accidental] Int Duration -- the Int is the octive
  | Rest Duration
  deriving (Show,Eq,Ord)

data Music
  = Prim Primitive
  | Seq  Music Music
  | Par  Music Music
  deriving (Show,Eq,Ord)
