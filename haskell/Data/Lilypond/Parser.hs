module Data.Lilypond.Parser where

import Orpheus.Data.Music.Diatonic
import qualified Data.Vector as V
import Text.Parsec.Token

lexer :: GenTokenParser String st Identity
lexer = makeTokeParser $ LanguageDef
  { Tok.commentStart    = "%{"
  , Tok.commentEnd      = "%}"
  , Tok.nestedComments  = False
  , Tok.identStart      = letter <|> char '_'
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf "!$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf "!$%&*+./<=>?@\\^|-~"
  , Tok.caseSensitive   = True
  , Tok.commentLine     = "%"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = []
  }


{--

Notes for the lilypond parser:

* some lilypond statements require read in other files and tokenizing them
* we have to know the keysigniture in order to know what the sharps and flats
  are
--}

data MState = MState
  { clef             :: Clef
  , keySig           :: (Pitchclass,Bool) -- bool for major and minor
  , isRelativeOctave :: Maybe Int   }
  deriving Show

data Clef
  = G -- Treble, G4
  | C -- Alto,   C4
  | F -- Bass,   F3
  deriving (Show,Eq)

pPitchclass :: Pitchclass
pPitchclass = undefined

pAccidental :: Parser Accidental
pAccidental = undefined

pDuration :: Parser Duration
pDuration = undefined

-- | Which not is parsed will depend on \relative tags and the key signature
pNote :: MState -> Parser Prim
pNote = undefined

pRest :: Parser Prim
pRest = undefined

pNote :: Parser Note
pNote = undefined

pVoice :: Parser Voice
pVoice = undefined

pScore :: Score
pScore = undefined
