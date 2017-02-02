{
-- module Lilypond.Lexer where
import System.Environment (getArgs)
}

%wrapper "basic"

tokens :-
  $white+            ;
  [a-zA-z]           { TokenLetter . read }
  [0-9]+             { TokenDigit . read }
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
  = TokenLeftAngleBracket
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
  deriving (Show)

main :: IO ()
main = do
  (f:[]) <- getArgs
  -- putStrLn f
  x <- case f == "-" of
         True  -> getContents
         False -> readFile f
  let toks = alexScanTokens x
  mapM_ (putStrLn . show) toks
}
