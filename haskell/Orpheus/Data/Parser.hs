--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Data.Parser
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- For now, this parses a Lilypond file into the Data.Music.Diatonic data
-- type.
--
--------------------------------------------------------------------------------

module Orpheus.Data.Parser where


-- lexStructure :: Tok.GenLanguageDef ParserStream st Identity
-- lexStructure = ITok.makeIndentLanguageDef $ Tok.LanguageDef
--     { Tok.commentStart    = ""
--     , Tok.commentEnd      = ""
--     , Tok.nestedComments  = True
--     , Tok.identStart      = "" -- letter <|> char '_'
--     , Tok.identLetter     = "" -- alphaNum <|> oneOf "_'"
--     , Tok.opStart         = "" -- oneOf "\"
--     , Tok.opLetter        = "" -- oneOf
--     , Tok.caseSensitive   = True
--     , Tok.commentLine     = "%"
--     , Tok.reservedOpNames = ["a","b"]
--     , Tok.reservedNames   = names
--     }
