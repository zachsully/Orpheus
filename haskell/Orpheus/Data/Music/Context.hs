module Orpheus.Data.Music.Context where


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
data Voice context = [(Context,[[Primitive]])]
  deriving (Show,Eq)

--------------------------------------------------------------------------------
--                                 Contexts                                   --
--------------------------------------------------------------------------------
{- Types of contexts
-- There can only be one of each type of context live in a piece
-}

data Tempo = Tempo Int
  deriving (Show,Eq,Ord)

data KeySig
  = Major Pitchclass
  | Minor Pitchclass
  deriving (Show,Eq)

data TimeSig = TimeSig Int Int
  deriving (Show,Eq)

data Dynamic

data Instrument

--------------------------------------------------------------------------------
--                                 Pitchclass                                 --
--------------------------------------------------------------------------------

data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq)

--------------------------------------------------------------------------------
--                                    Note                                    --
--------------------------------------------------------------------------------

data Accidental
  = Sharp Int
  | Flat Int
  | Natural
  deriving (Show,Eq)

data Duration = Duration Int
  deriving (Show,Eq,Ord)

data Primitive
  = Note Pitchclass Int Accidental Duration
  | Rest Duration
  deriving (Show,Eq)
