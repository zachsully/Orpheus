{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeFamilies,
             StandaloneDeriving #-}
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
import Language.Hakaru.Syntax.Datum
import Language.Hakaru.Types.Sing

-- import Data.Typeable (Typeable)
-- import GHC.TypeLits (Symbol)


{-
  Two representations, one in Haskell and one in Hakaru. The one in Haskell is
  a simple example of what the Hakaru one should look like.

  Much of this file is boilerplate. We could simplify the process by using
  template Haskell.
-}
--------------------------------------------------------------------------------
data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord,Enum)

type HPitchclass =
  'HData ('TyCon "Pitchclass") '[ '[], '[], '[], '[], '[], '[], '[] ]
                                {- A |  B |  C |  D |  E |  F |  G -}

type instance Code ('TyCon "Pitchclass") = '[ '[], '[], '[], '[], '[], '[], '[] ]

hA,hB,hC,hD,hE,hG :: Datum ast HPitchclass
hA = Datum "A" sPitchclass (Inl Done)
hB = Datum "B" sPitchclass (Inr . Inl $ Done)
hC = Datum "C" sPitchclass (Inr . Inr . Inl $ Done)
hD = Datum "D" sPitchclass (Inr . Inr . Inr . Inl $ Done)
hE = Datum "E" sPitchclass (Inr . Inr . Inr . Inr . Inl $ Done)
hF = Datum "F" sPitchclass (Inr . Inr . Inr . Inr . Inr . Inl $ Done)
hG = Datum "G" sPitchclass (Inr . Inr . Inr . Inr . Inr . Inr . Inl $ Done)

sPitchclass :: Sing HPitchclass
sPitchclass =
  SData (STyCon sSymbol_Pitchclass)
    (SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SVoid)

sSymbol_Pitchclass :: Sing "Pitchclass"
sSymbol_Pitchclass = SingSymbol



--------------------------------------------------------------------------------
data Accidental
  = Sharp
  | Flat
  | Natural
  deriving (Show,Eq,Ord,Enum)

type HAccidental
  = 'HData ('TyCon "Accidental") '[ '[], '[], '[] ]
                             {- Sharp | Flat | Natural -}

type instance Code ('TyCon "Accidental") = '[ '[], '[], '[] ]

hSharp, hFlat, hNatural :: Datum ast HAccidental
hSharp   = Datum "Sharp"   sAccidental (Inl Done)
hFlat    = Datum "Flat"    sAccidental (Inr . Inl $ Done)
hNatural = Datum "Natural" sAccidental (Inr . Inr . Inl $ Done)

sAccidental :: Sing HAccidental
sAccidental =
  SData (STyCon sSymbol_Accidental)
    (SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SVoid)

sSymbol_Accidental :: Sing "Accidental"
sSymbol_Accidental = SingSymbol


--------------------------------------------------------------------------------
data Duration
  = Whole
  | Half Duration
  | Dot  Duration
  deriving (Show,Eq,Ord)

type HDuration
  = 'HData ('TyCon "Duration") '[ '[] , '[ 'I ] , '[ 'I ] ]
                              {- Whole |   H D  |    D D -}


type instance Code ('TyCon "Duration") = '[ '[] , '[ 'I ] , '[ 'I ] ]

hWhole :: Datum ast HDuration
hWhole = Datum "Whole" sDuration (Inl Done)

hHalf, hDot :: ast HDuration -> Datum ast HDuration
hHalf d = Datum "Half" sDuration (Inr . Inl $ Ident d `Et` Done)
hDot  d = Datum "Half" sDuration (Inr . Inr . Inl $ Ident d `Et` Done)

sDuration :: Sing HDuration
sDuration =
  SData (STyCon sSymbol_Duration)
    (SDone `SPlus`
     (SIdent `SEt` SDone) `SPlus`
     (SIdent `SEt` SDone) `SPlus`
     SVoid)

sSymbol_Duration :: Sing "Duration"
sSymbol_Duration = SingSymbol



--------------------------------------------------------------------------------
data Primitive
  = Note Pitchclass [Accidental] Int Duration -- the Int is the octive
  | Rest Duration
  deriving (Show,Eq,Ord)

type HPrimitive
  = 'HData ('TyCon "Primitive")
           '[ '[ 'K HPitchclass
               , 'K (HList HAccidental)
               , 'K 'HInt
               , 'K HDuration ]
            , '[ 'K HDuration ] ]

type instance Code ('TyCon "Primitive") =
  '[ '[ 'K HPitchclass
      , 'K (HList HAccidental)
      , 'K 'HInt
      , 'K HDuration ]
   , '[ 'K HDuration ] ]

sPrimitive :: Sing HPrimitive
sPrimitive =
  SData (STyCon sSymbol_Primitive)
    ((      (SKonst sPitchclass)
      `SEt` (SKonst . sList $ sAccidental)
      `SEt` (SKonst SInt)
      `SEt` (SKonst sDuration)
      `SEt` SDone)
    `SPlus` (SKonst sDuration `SEt` SDone)
    `SPlus` SVoid)

sSymbol_Primitive :: Sing "Primitive"
sSymbol_Primitive = SingSymbol



--------------------------------------------------------------------------------
data Music
  = Prim Primitive
  | Seq  Music Music
  | Par  Music Music
  deriving (Show,Eq,Ord)

type HMusic
  = 'HData ('TyCon "Music") '[ '[ 'K HPrimitive ] , '[ 'I, 'I ] , '[ 'I, 'I ] ]
                             {-       Prim        |   Seq M M   |  Par M M   -}

type instance Code ('TyCon "Music") =
  '[ '[ 'K HPrimitive ] , '[ 'I, 'I ] , '[ 'I, 'I ] ]

sMusic :: Sing HMusic
sMusic =
  SData (STyCon sSymbol_Music)
    ( (SKonst sPrimitive `SEt` SDone)
     `SPlus` (SIdent `SEt` SIdent `SEt` SDone)
     `SPlus` (SIdent `SEt` SIdent `SEt` SDone)
     `SPlus` SVoid)

sSymbol_Music :: Sing "Music"
sSymbol_Music = SingSymbol
