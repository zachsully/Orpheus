{
module Lilypond.Parser where
}

%name lily
%tokentype { Token }
%error { parseError }
%token
  pitchclass { TokenPitchclass $$ }

%%


{
main :: IO ()
main = return ()
}
