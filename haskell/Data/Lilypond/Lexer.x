{
module Data.Lilypond.Lexer where
}

%wrapper "basic"

tokens :-
  $white+            ;
  [a-zA-z]           { TokenLetter . read }
  [0-9]+             { TokenDigit . read }
  \\[a-zA-z]+        { TokenCmd }
  \<\<               { const TokenDoubleLeftAngleBracket }
  \>\>               { const TokenDoubleRightAngleBracket }
  \<                 { const TokenLeftAngleBracket }
  \>                 { const TokenRightAngleBracket }
  \{                 { const TokenLeftBrace }
  \}                 { const TokenRightBrace }
  \(                 { const TokenLeftParen }
  \)                 { const TokenRightParen }
  \%                 { const TokenPercent }
  \\                 { const TokenBackSlash }
  [\"\'\,\.\^\#\|\-] { TokenSym . read }

{
data Token
  = TokenDoubleLeftAngleBracket
  | TokenDoubleRightAngleBracket
  | TokenLeftAngleBracket
  | TokenRightAngleBracket
  | TokenLeftBrace
  | TokenRightBrace
  | TokenLetter Char
  | TokenDigit Integer
  | TokenSym Char
  | TokenLeftParen
  | TokenRightParen
  | TokenPercent
  | TokenBackSlash
  | TokenDoubleBackSlash
  | TokenCmd String
  deriving (Show)
}
