module Orpheus.Data.Music.Context where

-- Types of contexts
-- There can only be one of each type of context live in a piece
data Tempo = Tempo Int
  deriving (Show,Eq,Ord)

data KeySignature
  = Major Pitchclass
  | Minor Pitchclass
  deriving (Show,Eq)

data TimeSignature = TimeSignature Int Int
  deriving (Show,Eq)

data Dynamic

data Instrument
