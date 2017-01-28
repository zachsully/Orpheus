--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Data.Pretty
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

module Orpheus.Data.Pretty where

import Text.PrettyPrint
import Orpheus.Data.Music.Diatonic

prettyTopLevel :: Music -> Doc
prettyTopLevel m = vcat
  [ header
  , text ""
  , scoreWrap . absoluteWrap . pretty $ m
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
durNumber Whole    = 1
durNumber (Half x) = 2 * (durNumber x)

instance Pretty Primitive where
  pretty (Note pc acs oct dur dot) =
    hcat $ [ pretty pc
           , hcat . fmap pretty $ acs
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

instance Pretty Music where
  pretty (Prim p)  = pretty p
  pretty (Seq m0 m1) = pretty m0 <+> pretty m1
  pretty (Par m0 m1) = hsep
    [ text "<"
    , pretty m0
    , pretty m1
    , text ">"
    ]
