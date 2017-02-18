module Data.Lilypond.Parser where

import Orpheus.Data.Music.Diatonic
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Token
import Prelude hiding (lex)

type Parser = ParsecT String () IO

parseLilyString :: String -> IO Score
parseLilyString s = do
  ep <- runParserT pScore () "lilypond" s
  case ep of
    Left  e     -> error $ show e
    Right score -> return score

pitchclassNames :: [String]
pitchclassNames = ["a","b","c","d","e","f","g"]

lex :: GenTokenParser String st IO
lex = makeTokenParser $ LanguageDef
  { commentStart    = "%{"
  , commentEnd      = "%}"
  , nestedComments  = False
  , identStart      = letter <|> char '_'
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = oneOf "!$%&*+./<=>?@\\^|-~"
  , opLetter        = oneOf "!$%&*+./<=>?@\\^|-~"
  , caseSensitive   = True
  , commentLine     = "%"
  , reservedOpNames = []
  , reservedNames   = pitchclassNames
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
  = GClef -- Treble, G4
  | CClef -- Alto,   C4
  | FClef -- Bass,   F3
  deriving (Show,Eq)

pTimeSignature :: MState -> Parser MState
pTimeSignature = undefined

pKeySignature :: MState -> Parser MState
pKeySignature = undefined

pPitchclass :: Parser Pitchclass
pPitchclass = try (reserved lex "a" >> return A)
         <|>  try (reserved lex "b" >> return B)
         <|>  try (reserved lex "c" >> return C)
         <|>  try (reserved lex "d" >> return D)
         <|>  try (reserved lex "e" >> return E)
         <|>  try (reserved lex "f" >> return F)
         <|>  try (reserved lex "g" >> return G)

pAccidental :: Parser Accidental
pAccidental = try (reserved lex "is" >> return Sharp)
          <|> try (reserved lex "es" >> return Flat)
          <|> try (reserved lex "!" >> return Natural)

pDuration :: MState -> Parser Duration
pDuration = undefined

-- | Which not is parsed will depend on \relative tags and the key signature
pNote :: MState -> Parser Primitive
pNote = undefined

pRest :: MState -> Parser Primitive
pRest = undefined

pPrimitive :: Parser Primitive
pPrimitive = try (pNote undefined)
         <|> try (pRest undefined)

pVoice :: Parser Voice
pVoice = undefined
  -- Voice . V.fromList <$> many1 pPrimitive

pScore :: Parser Score
pScore = Score . V.fromList <$> many1 pVoice
