module Data.MusicXML.Parser where

import Orpheus.Data.Music.Diatonic
import Text.XML.Light

newtype Parser s a = Parser { parse :: [s] -> [(a,[s])] }

runParser :: Parser s a -> [s] -> a
runParser p d =
  case parse p d of
    [(res,[])] -> res
    [(_,(_:_))] -> error "MusicXML parser did not consume entire string"
    _ -> error "MusicXML parser error"
   
parseMusicXMLFile :: FilePath -> IO Score
parseMusicXMLFile fp = do
  contents <- readFile fp
  let musXML = parseXML contents
      musXML' = head $ drop 4 musXML
  return (runParser parseScore [musXML'])

parseScore :: Parser Content Score
parseScore = undefined           
-- parseScore (Elem x) = parseScorePartwise x
-- parseScore (Text _) = mempty
-- parseScore (CRef _) = mempt

-- parseScorePartwise :: Element -> Parser Content Score
-- parseScorePartwise (Element qname _ content _) =
--   let partListC = head $ drop 3 content
--       num       = parseNumParts partListC
--       dataset   = head $ drop 11 content
--   in error $ show num -- error $ ppContent $

-- parseNumParts :: Content -> Parser Content Int
-- parseNumParts (Elem (Element _ _ c _)) =
--   let c' = head $ drop 3 c
--   in error $ ppContent c'
-- parseNumParts _ = error "Data.MusicXML.Parser.parseNumParts: should be XML elements"

-- parsePart :: Element -> Voice
-- parsePart = undefined
