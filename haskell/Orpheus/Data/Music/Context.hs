{-# LANGUAGE DeriveGeneric #-}
module Orpheus.Data.Music.Context where

import GHC.Generics (Generic)
import Data.Hashable

--------------------------------------------------------------------------------
--                                   Score                                    --
--------------------------------------------------------------------------------
{- Abstract over the type of contexts available
-}

data Score context = Score [Part context]
  deriving (Show,Eq)

data Part context = Part (Voice context)
  deriving (Show,Eq)

-- In pieces without KeySig or TimeSig changes the outer list will just have
-- a single element
-- For list of list of primitives: given a context, return a sequence of [prim]
-- [prim] could be a singleton, that being just a note, or a chord
data Voice context = Voice [(context,[[Primitive]])]
  deriving (Show,Eq)

--------------------------------------------------------------------------------
--                                 Contexts                                   --
--------------------------------------------------------------------------------
{- Types of contexts
-- There can only be one of each type of context live in a piece
-}

data Tempo = Tempo Int
  deriving (Show,Eq,Ord)

-- the int represents the number of fifths away from C and therefore the number
-- of sharps or flats in the key signature
data KeySig
  = Major Int
  | Minor Int
  deriving (Show,Eq,Ord,Generic)
instance Hashable KeySig


data TimeSig = TimeSig Int Int
  deriving (Show,Eq,Ord,Generic)
instance Hashable TimeSig

data Dynamic

data Instrument

--------------------------------------------------------------------------------
--                                 Pitchclass                                 --
--------------------------------------------------------------------------------

data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord,Generic)
instance Hashable Pitchclass


--------------------------------------------------------------------------------
--                                    Note                                    --
--------------------------------------------------------------------------------

data Accidental
  = Sharp Int
  | Flat Int
  | Natural
  deriving (Show,Eq,Ord,Generic)
instance Hashable Accidental

data Duration = Duration Rational
  deriving (Show,Eq,Ord,Generic)
instance Hashable Duration

type Octave = Int

data Primitive
  = Note Pitchclass Octave Accidental Duration
  | Rest Duration
  deriving (Show,Eq,Ord,Generic)
instance Hashable Primitive
