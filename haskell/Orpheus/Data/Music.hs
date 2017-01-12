--------------------------------------------------------------------------------
--                                                                  2017.01.11
-- |
-- Module      :  Data.Music
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing Musical Data
--
--------------------------------------------------------------------------------

module Data.Music where

data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord)

data Accidental
  = Sharp
  | Flat
  | Natural
  deriving (Show,Eq,Ord)

data Duration
  = Breve
  | Semibreve
  | Minim
  | Crotchet
  | Quaver
  | Semiquaver
  | Demisemiquaver
  deriving (Show,Eq,Ord)

data Note
  = NoteN Pitchclass [Accidental] Int Duration -- the Int is the octive
  | RestN Duration
  deriving (Show,Eq,Ord)

data Music
  = Prim Note
  | Seq  Music Music
  | Par  Music Music
  deriving (Show,Eq,Ord)
