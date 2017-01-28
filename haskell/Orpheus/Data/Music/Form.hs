{-# LANGUAGE DataKinds,
             OverloadedStrings,
             TypeFamilies,
             StandaloneDeriving #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Data.Music.Form
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Data description for musical forms
--
--------------------------------------------------------------------------------

module Orpheus.Data.Music.Form where

import Language.Hakaru.Types.DataKind
import Language.Hakaru.Types.Sing

{-
  Structure for music forms. We need to represent and arbitrary number of
  symbols that can be augmented or complimented. For instance, we need to be
  able to represent these common forms:

     AB
     ABA
     ABA'BA
     ABCA

  There could also be more than one way to make a derived form. For example,
  think of a fugue, where variations of a form are played in parallel.

     A A_a A_b A_c
-}

data Form
  = Primary String
  | Derived String Form
  deriving (Show,Eq,Ord)

type HForm
  = 'HData ('TyCon "Form") '[ '[ 'K (HList 'HInt) ]
                            , '[ 'K (HList 'HInt)
                               , 'I
                               ]
                            ]

type instance Code ('TyCon "Form")
  = '[ '[ 'K (HList 'HInt) ]
     , '[ 'K (HList 'HInt)
        , 'I
        ]
     ]

sForm :: Sing HForm
sForm =
  SData (STyCon sSymbol_Form)
    ( (SKonst (sList SInt) `SEt` SDone)
    `SPlus` (SKonst (sList SInt) `SEt` SIdent `SEt` SDone)
    `SPlus` SVoid)

sSymbol_Form :: Sing "Form"
sSymbol_Form = SingSymbol
