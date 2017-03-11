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
-- Score Partwise captures the whole score

arrScorePartwise :: (ArrowXml a) => a XmlTree (Score (KeySig,TimeSig))
arrScorePartwise = arr id /> hasName "score-partwise"
               >>> listA arrPart
               >>> arr (Score . fromList)

-- A part is probably a single instrument
arrPart :: (ArrowXml a) => a XmlTree (Part (KeySig,TimeSig))
arrPart = arr id /> hasName "part"
      >>> listA arrMeasure
      >>> arr (Part . singleton . fromList . concat)

-- A voice will be a single instrument and can contain chords
arrVoice :: (ArrowXml a) => a XmlTree (Voice (KeySig,TimeSig))
arrVoice = undefined

-- A context here, represents the KeySignature,TimeSignature and the base note
-- divisions.
arrContext :: (ArrowXml a) => a XmlTree (KeySig,TimeSig,Int)
arrContext = arr id /> hasName "attributes"
         >>> (arrKey &&& arrTime &&& arrDivisions)

arrDivisions :: (ArrowXml a) => a XmlTree Int
arrDivisions = arr id /> hasName "divisions"
           >>> getText
           >>^ read

arrKey :: (ArrowXml a) => a XmlTree KeySig
arrKey = arr id /> hasName "key"
     >>> ((arr id /> hasName "fifths" /> getText) &&&
          (arr id /> hasName "mode" /> getText))
     >>> arr (\(_,_) -> undefined)

arrTime :: (ArrowXml a) => a XmlTree TimeSig
arrTime = arr id /> hasName "time"
     >>> ((arr id /> hasName "beats" /> getText >>^ read) &&&
          (arr id /> hasName "beat-type" /> getText >>^ read))
     >>> arr (\(b,bt) -> TimeSig b bt)

-- measures are sequences of notes, they can contain attributes including
-- contexts for the next sequence of notes
arrMeasure :: (ArrowXml a) => Int -> a XmlTree [Primitive]
arrMeasure divs = arr id /> hasName "measure"
              >>> listA (arrPrimitive divs)

-- primitives are just notes and rests, will probably need to handle
-- chords here as well
arrPrimitive :: (ArrowXml a) => Int -> a XmlTree Primitive
arrPrimitive divs = arr id /> hasName "note"
                >>> ((arrNote divs) <+> (arrRest divs))

arrNote :: (ArrowXml a) => Int -> a XmlTree Primitive
arrNote divs = arr id /> hasName "note"
          >>> hasName ""
          >>> (arrPitch &&& arrDuration divs)
          >>^ (\((pc,oct),dur) -> Note pc oct Natural dur)


arrRest :: (ArrowXml a) => Int -> a XmlTree Primitive
arrRest divs = arr id /> hasName "note"
      >>> hasName "rest"
      >>> arrDuration divs
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
