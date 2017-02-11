{-# LANGUAGE TypeSynonymInstances #-}
--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Data.Lilypond.Pretty
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- For now, this pretty prints Orpheus.Data.Music.Diatonic into a Lilypond
-- file
--
--------------------------------------------------------------------------------

module Data.Lilypond.Pretty where

import Data.Vector (toList)
import Text.PrettyPrint
import Orpheus.Data.Music.Diatonic

prettyPrintScore :: Score -> String
prettyPrintScore = render . prettyTopLevel                 

prettyTopLevel :: Score -> Doc
prettyTopLevel m = vcat
  [ header
  , text ""
  , scoreWrap . pretty $ m
  ]

header :: Doc
header = vcat
  [ text "\\version \"2.18.2\""
  , headt
  ]
  where headt = foldr ($+$) mempty
                  [ text "\\header"
                  , lbrace
                  , nest 2 ( text "composer = Orpheus" )
                  , nest 2 ( text "tagline = \"\"" )
                  , rbrace
                  ]


scoreWrap :: Doc -> Doc
scoreWrap body = foldr ($+$) mempty
  [ text "\\score"
  , lbrace
  , nest 2 body
  , rbrace
  ]

absoluteWrap :: Doc -> Doc
absoluteWrap body = foldr ($+$) mempty
  [ text "\\absolute"
  , lbrace
  , nest 2 body
  , rbrace
  ]

class Pretty a where
  pretty :: a -> Doc
  prettyPrec :: Int -> a -> Doc

  pretty = prettyPrec 0
  prettyPrec _ = pretty


instance Pretty Pitchclass where
  pretty A = text "a"
  pretty B = text "b"
  pretty C = text "c"
  pretty D = text "d"
  pretty E = text "e"
  pretty F = text "f"
  pretty G = text "g"

instance Pretty Accidental where
  pretty Sharp   = text "is"
  pretty Flat    = text "es"
  pretty Natural = text "!"

instance Pretty Duration where
  pretty = text . show . durNumber

durNumber :: Duration -> Int
durNumber x =
  let x' = fromEnum x in
  if x' < 1
  then error "TODO: prett durations longer than whole"
  else 2 ^ x'

instance Pretty Primitive where
  pretty (Note pc acs oct dur dot) =
    hcat $ [ pretty pc
           , hcat . fmap pretty $ toList acs
           , let dis = octDistance oct in
             hcat . replicate dis $ if dis > 0 then text "'" else comma
           , pretty dur
           ]
        ++ (if dot then [text "."] else [])
  pretty (Rest dur dot) =
    hcat $ [ text "r" , pretty dur ]
        ++ (if dot then [text "."] else [])

-- specifies the number of octives from 4
octDistance :: Int -> Int
octDistance i = i - 3

instance Pretty Voice where
  pretty (Voice v) = hsep $ fmap prettyPar $ toList v
    where prettyPar ps =
            if length ps < 2
            then hcat . fmap pretty . toList $ ps
            else hsep $ [ text "<" ]
                     ++ (fmap pretty . toList $ ps)
                     ++ [ text ">" ]

instance Pretty Score where
  pretty (Score s) = vcat $
    [ text "\\new StaffGroup <<" ]
    ++ (fmap (absoluteWrap . pretty) . toList $ s)
    ++ [ text ">>" ]
