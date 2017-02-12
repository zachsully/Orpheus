{
module Data.Lilypond.Parser where

import Data.Lilypond.Lexer (Token(..))
import Orpheus.Data.Music.Diatonic
import qualified Data.Vector as V
import Control.Monad.State
}

%name lily
%tokentype { Token }
%monad { State Int } { (>>=) } { return }
%error { parseError }
%token
        "<<"   { TokenDoubleLeftAngleBracket }
        ">>"   { TokenDoubleRightAngleBracket }
        '<'    { TokenLeftAngleBracket }
        '>'    { TokenRightAngleBracket }
        '{'    { TokenLeftBrace }
        '}'    { TokenRightBrace }
        '('    { TokenLeftParen }
        ')'    { TokenRightParen }
        '%'    { TokenPercent }
        '\\'   { TokenBackSlash }
        int    { TokenDigit $$ }
        letter { TokenLetter $$ }
        sym    { TokenSym $$ }

%%

PitCl : letter                  { \p -> if elem $1 "abcdefg"
                                        then $1
                                        else parseError p }

Acc   : sym                     { const "foo" }

Dur   : int                     { const "foo" }

Prim  : int '%'                 { const "foo" }
      | sym '>'                 { const "foo" }

ScoreCmd : int '\\'             { const "foo" }

Staffs : sym '\\'               { const "foo" }

Score : ScoreCmd '{' Staffs '}' { const V.empty }

{
parseError :: [Token] -> a
parseError _ = error "Parse Error"
}
