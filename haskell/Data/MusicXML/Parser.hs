{-# LANGUAGE Arrows #-}
module Data.MusicXML.Parser where

import Data.Vector (fromList, singleton)
import Orpheus.Data.Music.Context
import Text.XML.HXT.Core

--------------------------------------------------------------------------------
parseMusicXMLFile :: FilePath -> IO (Score (KeySig,TimeSig))
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

arrScorePartwise :: (ArrowXml a) => a XmlTree (Score (KeySig,TimeSig))
arrScorePartwise = arr id /> hasName "score-partwise"
               >>> listA arrPart
               >>> arr (Score . fromList)

arrPart :: (ArrowXml a) => a XmlTree (Part (KeySig,TimeSig))
arrPart = arr id /> hasName "part"
      >>> listA arrMeasure
      >>> arr (Part . singleton . fromList . concat)

arrContext :: (ArrowXml a) => a XmlTree (KeySig,TimeSig,Int)
arrContext = arr id /> hasName "attributes"
         >>> (arrKey &&& arrTime &&& arrDivisions)

arrDivision :: (ArrowXml a) => a XmlTree Int
arrDivision = arr id /> hasName "divisions"
          >>> getText
          >>> arr (read :: a -> Int)

arrKey :: (ArrowXml a) => a XmlTree KeySig
arrKey = arr id /> hasName "key"
     >>> ((arr id /> hasName "fifths" /> getText) &&&
          (arr id /> hasName "mode" /> getText))
     >>> arr (\(_,_) -> undefined)

arrTime :: (ArrowXml a) => a XmlTree TimeSig
arrTime = arr id /> hasName "time"
     >>> ((arr id /> hasName "beats" /> getText) &&&
          (arr id /> hasName "beat-type" /> getText))
     >>> arr (\(b,bt) -> TimeSig b bt)

arrVoice :: (ArrowXml a) => a XmlTree (Voice (KeySig,TimeSig))
arrVoice = undefined

arrMeasure :: (ArrowXml a) => a XmlTree [Primitive]
arrMeasure = arr id /> hasName "measure"
         >>> listA arrPrimitive

arrPrimitive :: (ArrowXml a) => a XmlTree Primitive
arrPrimitive = arr id /> hasName "note"
           >>> (arrNote ||| arrRest)

arrRest :: (ArrowXml a) => a XmlTree Primitive
arrRest = arr id /> hasName "note"
      >>> hasName "rest"
      >>> arrDuration
      >>^ Rest

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

-- divisions per quarter note
arrDuration :: (ArrowXml a) => Int -> a XmlTree Duration
arrDuration divisions = arr id /> hasName "duration" /> getText
          >>> arr (\s -> case (read s :: Rational) of
                          x -> Duration x
                          _ -> error $ "Unrecognized duration: " ++ s)
