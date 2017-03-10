module Data.MusicXML.Parser where

import Control.Monad.Identity
import Control.Monad
import Data.Vector (fromList, Vector)
import Orpheus.Data.Music.Diatonic
import Text.XML.Light
import Text.Parsec

--------------------------------------------------------------------------------
type Parser = ParsecT [Content] () Identity

prim :: Parser Content
prim = tokenPrim show (\pos _ _ -> pos) Just

satisfy' :: (Content -> Bool) -> Parser Content
satisfy' pred = prim >>= \p ->
  case pred p of
    True -> return p
    False -> parserFail "does not satisfy"


--------------------------------------------------------------------------------
parseMusicXMLFile :: FilePath -> IO Score
parseMusicXMLFile fp = do
  contents <- readFile fp
  let musXML = parseXML contents
      (Identity musXML') = runParserT parseTopLevel () fp musXML
  case musXML' of
    (Left e) -> error $ "MusicXML parser error: " ++ show e
    (Right s) -> return s

isUnused :: Content -> Bool
isUnused (Elem e) = elem (qName . elName $ e) ["?xml"]
isUnused (Text _) = True
isUnused (CRef _) = True
isUnused _ = False

parseUnused :: Parser ()
parseUnused = try $ do
  p <- prim
  if isUnused p
  then return ()
  else parserFail "marco"

-- throw away information we do not car about
munge :: Parser ()
munge = skipMany parseUnused

parseTopLevel :: Parser Score
parseTopLevel = munge >> parseScorePartwise


parseScorePartwise :: Parser Score
parseScorePartwise = do
  p <- prim
  case p of
    return . Score . (runParser parseScorePartwise') $ cs
  return (Score . fromList $ [])

  -- case qName q of
  --   "score-partwise" -> return . Score . (runParser parseScorePartwise') $ cs
  --   _ -> mzero

-- parseScorePartwise' :: Parser Content (Vector Voice)
-- parseScorePartwise' = do
--   -- text
--   -- _ <- element
--   -- text
--   -- _ <- parsePartList
--   -- many text
--   -- vs <- many element
--   return $ fromList []

-- parsePart :: Parser Content Voice
-- parsePart = do
--   e <- element
--   return (Voice (fromList []))
