{-# LANGUAGE Arrows #-}
module Data.MusicXML.Parser where

import Data.Vector (fromList, singleton)
import Orpheus.Data.Music.Diatonic
import Text.XML.HXT.Core

--------------------------------------------------------------------------------
parseMusicXMLFile :: FilePath -> IO Score
parseMusicXMLFile fp = do
  score <- runX $ readDocument [ withValidate         False
                               , withSubstDTDEntities False
                               , withRemoveWS         True
                               ] fp
              >>> arrScorePartwise
  case score of
    []     -> error "Failed to parse score"
    (s:[]) -> return s
    _ -> error $ show $ score

--------------------------------------------------------------------------------

arrScorePartwise :: (ArrowXml a) => a XmlTree Score
arrScorePartwise = arr id /> hasName "score-partwise"
               >>> listA arrPart
               >>> arr (Score . fromList)

arrPart :: (ArrowXml a) => a XmlTree Voice
arrPart = arr id /> hasName "part"
      >>> listA arrMeasure
      >>> arr (Voice . singleton . fromList . concat)

arrMeasure :: (ArrowXml a) => a XmlTree [Primitive]
arrMeasure = arr id /> hasName "measure"
         >>> listA arrNote

arrNote :: (ArrowXml a) => a XmlTree Primitive
arrNote = arr id /> hasName "note"
      >>> (arrPitch &&& arrDuration)
      >>> arr (\((p,oct), (dur,dot)) ->
                Note p mempty cMajor commonTime oct dur dot)

arrPitch :: (ArrowXml a) => a XmlTree (Pitchclass,Int)
arrPitch = arr id /> hasName "pitch"
       >>> (arrPitchclass &&& arrOctave)

arrPitchclass :: (ArrowXml a) => a XmlTree Pitchclass
arrPitchclass = arr id /> hasName "step" /> getText
            >>> arr (\s -> case s of
                            "A" -> A
                            "B" -> B
                            "C" -> C
                            "D" -> D
                            "E" -> E
                            "F" -> F
                            "G" -> G
                            _   -> error $ "Unrecognized pitchclass: " ++ s)

arrOctave :: (ArrowXml a) => a XmlTree Int
arrOctave = arr id /> hasName "octave" /> getText
        >>> arr (read :: String -> Int)

-- duration depends of divisions tag, which we will ignore here
arrDuration :: (ArrowXml a) => a XmlTree (Duration,Bool)
arrDuration = arr id /> hasName "duration" /> getText
          >>> arr (\s -> case s of
                          "24"  -> (DWhole,True)
                          "16"  -> (DWhole,False)
                          "12"  -> (Whole,True)
                          "8"   -> (Whole,False)
                          "6"   -> (Half,True)
                          "4"   -> (Half,False)
                          "3"   -> (Quarter,True)
                          "2"   -> (Quarter,False)
                          "1"   -> (Eighth,False)
                          _     -> error $ "Unrecognized duration: " ++ s)
