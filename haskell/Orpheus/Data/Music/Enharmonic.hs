{-# LANGUAGE DataKinds,
             DeriveGeneric,
             OverloadedStrings,
             TypeFamilies,
             StandaloneDeriving #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.11
-- |
-- Module      :  Orpheus.Data.Music.Enharmonic
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Describing Musical Data
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music.Enharmonic where

import Language.Hakaru.Syntax.Datum
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing
import Data.Vector (Vector)
import GHC.Generics

{-
  Two representations, one in Haskell and one in Hakaru. The one in Haskell is
  a simple example of what the Hakaru one should look like.

  Much of this file is boilerplate. We could simplify the process by using
  template Haskell.
-}
--------------------------------------------------------------------------------
data Pitchclass = A | B | C | D | E | F | G
  deriving (Show,Eq,Ord,Enum,Generic)

type HPitchclass =
  'HData ('TyCon "Pitchclass") '[ '[], '[], '[], '[], '[], '[], '[] ]
                                {- A |  B |  C |  D |  E |  F |  G -}

type instance Code ('TyCon "Pitchclass") = '[ '[], '[], '[], '[], '[], '[], '[] ]

hA,hB,hC,hD,hE,hF,hG :: Datum ast HPitchclass
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
  deriving (Show,Eq,Ord,Enum,Generic)

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
{-
  Duration is an integer where 0 is a whole note, 2 is an eighth note. These
  represent note durations as:

                             dur(i) = 2 ^ i

                             quad   whole = 4
                             double whole = 2
                                    whole = 1
                                    half  = 1/2
                                  quarter = 1/4

                                                 *
                                            *    *    *
                                  *    *    *    *    *    *
                             <----|----|----|----|----|----|----->
                                  2    1   1/2  1/4  1/8  1/16

  A guess is that these notes are discretely normally distributed on the
  rationals

  BUT, for now I will just hard code a number of categories and hope that my
  data does not go outside
-}
data Duration
  = DWhole
  | Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | ThirtySecond
  | SixtyFourth
  | OneTwentyEighth
  | TwoFiftySixth
  deriving (Show,Eq,Ord,Enum,Generic)

type HDuration
  = 'HData ('TyCon "Duration") '[ '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[]
                                , '[] ]

type instance Code ('TyCon "Duration")
  =  '[ '[]
      , '[]
      , '[]
      , '[]
      , '[]
      , '[]
      , '[]
      , '[]
      , '[]
      , '[] ]

sDuration :: Sing HDuration
sDuration =
  SData (STyCon sSymbol_Duration)
    (SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SDone `SPlus`
     SVoid)

sSymbol_Duration :: Sing "Duration"
sSymbol_Duration = SingSymbol

-- Constructors
hDWhole,hWhole,hHalf,hQuarter,hEighth,hSixteenth,hThirtySecond,hSixtyFourth,hOneTwentyEighth,hTwoFiftySixth
  :: Datum ast HDuration
hDWhole          = Datum "2*1" sDuration (Inl $ Done)
hWhole           = Datum "1"   sDuration (Inr . Inl $ Done)
hHalf            = Datum "2"   sDuration (Inr . Inr . Inl $ Done)
hQuarter         = Datum "4"   sDuration (Inr . Inr . Inr . Inl $ Done)
hEighth          = Datum "8"   sDuration (Inr . Inr . Inr . Inr . Inl $ Done)
hSixteenth       = Datum "16"  sDuration (Inr . Inr . Inr . Inr . Inr . Inl $ Done)
hThirtySecond    = Datum "32"  sDuration (Inr . Inr . Inr . Inr . Inr . Inr . Inl $ Done)
hSixtyFourth     = Datum "64"  sDuration (Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inl $ Done)
hOneTwentyEighth = Datum "128" sDuration (Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inl $ Done)
hTwoFiftySixth   = Datum "256" sDuration (Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inr . Inl $ Done)

--------------------------------------------------------------------------------
data Primitive
  = Note Pitchclass
         (Vector Accidental)
         Int           -- octive
         Duration
         Bool          -- dotted?
  | Rest Duration
         Bool          -- dotted?
  deriving (Show,Eq,Ord,Generic)

type HPrimitive
  = 'HData ('TyCon "Primitive")
           '[ '[ 'K HPitchclass
               , 'K ('HArray HAccidental)
               , 'K 'HInt
               , 'K HDuration
               , 'K HBool ]
            , '[ 'K HDuration
               , 'K HBool ] ]

type instance Code ('TyCon "Primitive") =
  '[ '[ 'K HPitchclass
      , 'K ('HArray HAccidental)
      , 'K 'HInt
      , 'K HDuration
      , 'K HBool ]
   , '[ 'K HDuration
      , 'K HBool ] ]

sPrimitive :: Sing HPrimitive
sPrimitive =
  SData (STyCon sSymbol_Primitive)
    ((      (SKonst sPitchclass)
      `SEt` (SKonst . SArray $ sAccidental)
      `SEt` (SKonst SInt)
      `SEt` (SKonst sDuration)
      `SEt` (SKonst sBool)
      `SEt` SDone)
    `SPlus` (SKonst sDuration `SEt` (SKonst sBool) `SEt` SDone)
    `SPlus` SVoid)

sSymbol_Primitive :: Sing "Primitive"
sSymbol_Primitive = SingSymbol


hRest :: ast HDuration -> ast HBool -> Datum ast HPrimitive
hRest _ _ = undefined -- Datum "Rest" sDuration (Inl $ Done)




--------------------------------------------------------------------------------
{-

Similar to PiSigma types we have Vectors of Vectors of musical primitives. The
outer Vector is sequential composition. The inner vector is parallel
composition.

-}
newtype Voice = Voice (Vector (Vector Primitive))
  deriving (Show,Eq,Generic)

type HVoice
  = 'HData ('TyCon "Voice") '[ '[ 'K ('HArray ('HArray HPrimitive)) ] ]

type instance Code ('TyCon "Voice") = '[ '[ 'K ('HArray ('HArray HPrimitive)) ] ]

sVoice :: Sing HVoice
sVoice =
  SData (STyCon sSymbol_Voice)
    ( (SKonst (SArray (SArray sPrimitive)) `SEt` SDone)
     `SPlus` SVoid)

sSymbol_Voice :: Sing "Voice"
sSymbol_Voice = SingSymbol

--------------------------------------------------------------------------------
newtype Score = Score (Vector Voice)
  deriving (Show,Eq,Generic)

type HScore
  = 'HData ('TyCon "Score") '[ '[ 'K ('HArray HVoice) ] ]

type instance Code ('TyCon "Score") = '[ '[ 'K ('HArray HVoice) ] ]

sScore :: Sing HScore
sScore =
  SData (STyCon sSymbol_Score)
    ( (SKonst (SArray sVoice) `SEt` SDone) `SPlus` SVoid)

sSymbol_Score :: Sing "Score"
sSymbol_Score = SingSymbol


--------------------------------------------------------------------------------
