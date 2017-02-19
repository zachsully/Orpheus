module Data.Lilypond.Parser where

import Orpheus.Data.Music.Diatonic
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.Token
import Prelude hiding (lex)

type Parser = ParsecT String () IO

parseLilyString :: String -> IO Score
parseLilyString s = do
  ep <- runParserT pLilypond () "lilypond" s
  case ep of
    Left  e     -> error $ show e
    Right score -> return score

pitchclassNames,objs,cmdNames :: [String]
pitchclassNames = ["a","b","c","d","e","f","g"]
objs = ["StaffGroup","Staff","GrandStaff","bass","treble"]
cmdNames = ["time","absolute","relative","new","repeat"]

lex :: GenTokenParser String st IO
lex = makeTokenParser $ LanguageDef
  { commentStart    = "%{"
  , commentEnd      = "%}"
  , nestedComments  = False
  , identStart      = letter
  , identLetter     = alphaNum
  , opStart         = oneOf "\\'"
  , opLetter        = alphaNum
  , caseSensitive   = True
  , commentLine     = "%"
  , reservedOpNames = cmdNames
  , reservedNames   = pitchclassNames ++ objs
  }


{--

Notes for the lilypond parser:

* some lilypond statements require read in other files and tokenizing them
* we have to know the keysigniture in order to know what the sharps and flats
  are

--}

data MState = MState
  { clef             :: Clef
  , keySig           :: KeySig
  , timeSig          :: TimeSig
  , isRelativeOctave :: Maybe Int }
  deriving Show

data Clef
  = GClef -- Treble, G4
  | CClef -- Alto,   C4
  | FClef -- Bass,   F3
  deriving (Show,Eq)

pLilypond :: Parser Score
pLilypond = try (pHeader >> pLilypond)
        <|> try (pLayout >> pLilypond)
        <|> try (pMidi   >> pLilypond)
        <|> pScore

--------------------------------------------------------------------------------
{-

For now these bits of data are thrown out

-}

pHeader :: Parser ()
pHeader = undefined

pLayout :: Parser ()
pLayout = undefined

pMidi :: Parser ()
pMidi = undefined

pStaff :: Parser ()
pStaff = undefined

pNew :: Parser ()
pNew = undefined

pSet :: Parser ()
pSet = undefined

pOverride :: Parser ()
pOverride = undefined

pWith :: Parser ()
pWith = undefined

pPaper :: Parser ()
pPaper = undefined

pRemove :: Parser ()
pRemove = undefined

pInclude :: Parser ()
pInclude = undefined

pVersion :: Parser ()
pVersion = undefined

pScheme :: Parser ()
pScheme = undefined

--------------------------------------------------------------------------------

pVar :: Parser String
pVar = undefined

--------------------------------------------------------------------------------
--                           Musical Information                              --
--------------------------------------------------------------------------------

pRelative :: Parser MState
pRelative = do
  reserved lex "relative"
  return $ MState GClef (Major C) commonTime (Just 4)

pAbsolute :: Parser MState
pAbsolute = do
  reserved lex "absolute"
  return $ MState GClef (Major C) commonTime (Just 4)

-- will not consider this for now
pChordmode :: Parser ()
pChordmode = undefined

-- will not consider this for now
pTranspose :: Parser ()
pTranspose = undefined

-- clef does not effect the symbolic information
pClef :: Parser ()
pClef = undefined

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

{-
Absolute: The octave is given with every pitchclass
Relative:
* if no octive mark is given, the octave is given as less than a fifth
* within chords the next note is given relative to the preceding
-}
pNote :: MState -> Parser Primitive
pNote = undefined

pRest :: MState -> Parser Primitive
pRest = undefined

pPrimitive :: Parser Primitive
pPrimitive = try (pNote undefined)
         <|> try (pRest undefined)

pVoice :: Parser Voice
pVoice = undefined
  --  Voice . V.fromList <$> many1 pPrimitive

pScore :: Parser Score
pScore = Score . V.fromList <$> many1 pVoice
