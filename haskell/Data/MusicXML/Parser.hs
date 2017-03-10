{-# LANGUAGE Arrows #-}
module Data.MusicXML.Parser where

import Data.Vector (fromList, Vector)
import Orpheus.Data.Music.Diatonic
import Text.XML.HXT.Core

--------------------------------------------------------------------------------
parseMusicXMLFile :: FilePath -> IO Score
parseMusicXMLFile fp = do
  score <- runX $ readDocument [] fp
  return (Score $ fromList [])

--------------------------------------------------------------------------------

procScorePartwise :: (ArrowXml a) => a XmlTree Score
procScorePartwise = getChildren >>> hasName "score-partwise" >>> undefined

procNumParts :: (ArrowXml a) => a XmlTree Int
procNumParts = getChildren >>> hasName "part-list" >>> undefined

procPart :: (ArrowXml a) => a XmlTree Voice
procPart = getChildren >>> hasName "part" >>> undefined

procNote :: (ArrowXml a) => a XmlTree Primitive
procNote = getChildren >>> hasName "note" >>> undefined

procPitchclass :: (ArrowXml a) => a XmlTree Pitchclass
procPitchclass = getChildren >>> hasName "step" >>> undefined

procOctave :: (ArrowXml a) => a XmlTree Int
procOctave = getChildren >>> hasName "octave" >>> undefined

procDuration :: (ArrowXml a) => a XmlTree Duration
procDuration = getChildren >>> hasName "duration" >>> undefined
