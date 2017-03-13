{-# LANGUAGE Arrows #-}
module Data.MusicXML.Parser where

import Orpheus.Data.Music.Context
import Text.XML.HXT.Core

--------------------------------------------------------------------------------
--                                  Top Level                                 --
--------------------------------------------------------------------------------
-- for now we will only be concerned with this simple musical context
type Ctx = (KeySig,TimeSig)

parseMusicXMLFile :: FilePath -> IO (Score Ctx)
parseMusicXMLFile fp = do
  score <- runX $ readDocument [ withValidate         False
                               , withSubstDTDEntities False
                               , withRemoveWS         True
                               ] fp
               /> arrScorePartwise
  case score of
    []     -> error "Failed to parse score"
    (s:[]) -> return s
    _ -> error $ "Found multiple scores: " ++ (show score)


--------------------------------------------------------------------------------
--                               Score Partwise                               --
--------------------------------------------------------------------------------
arrScorePartwise :: (ArrowXml a) => a XmlTree (Score Ctx)
arrScorePartwise
  =   hasName "score-partwise"
  />  arrParts
  >>^ Score

-- A part is probably a single instrument
arrParts :: (ArrowXml a) => a XmlTree [Part Ctx]
arrParts
  =   hasName "part"
  />  arrVoice
  >>> listA (arr Part)

-- A voice will be a single instrument and can contain chords
-- we will need to coalesce measures in the same context, eventually
arrVoice :: (ArrowXml a) => a XmlTree (Voice Ctx)
arrVoice = listA arrMeasure >>^ Voice

-- > measures are sequences of notes,
-- > they can also contain attributes including contexts for the next sequence
--   of notes
arrMeasure :: (ArrowXml a) => a XmlTree (Ctx,[[Primitive]])
arrMeasure
  =   hasName "measure"
  />  arrContext
  >>^ (\c -> ((fst c), []))
  -- >>^ (\c -> (fst c, fromSLA (snd c) (listA arrParPrimitive)))


-- A context here, represents the KeySignature,TimeSignature
-- we also attach a divisions integer that will be used for deciding note length
arrContext :: (ArrowXml a) => a XmlTree (Ctx,Int)
arrContext
  =   hasName "attributes"
  />  ((arrKey &&& arrTime) &&& arrDivisions)

arrDivisions :: (ArrowXml a) => a XmlTree Int
arrDivisions
  =   hasName "divisions"
  />  getText
  >>^ read

arrKey :: (ArrowXml a) => a XmlTree KeySig
arrKey
  =   hasName "key"
  />  ((hasName "fifths" /> getText) &&&
       (hasName "mode" /> getText))
  >>^ (\(_,_) -> Major C)

arrTime :: (ArrowXml a) => a XmlTree TimeSig
arrTime
  =   hasName "time"
  />  ((hasName "beats" /> getText >>^ read) &&&
       (hasName "beat-type" /> getText >>^ read))
  >>^ (\(b,bt) -> TimeSig b bt)

--------------------------------------------------------------------------------
--                                  Primitives                                --
--------------------------------------------------------------------------------
-- A chord in MusicXml is a series of notes that where the 2nd,...,nth
-- note also contains the element <chord/>
arrParPrimitive :: SLA Int XmlTree [Primitive]
arrParPrimitive = constA []

-- primitives are just notes and rests, will probably need to handle
-- chords here as well
arrPrimitive :: SLA Int XmlTree Primitive
arrPrimitive
  =   hasName "note"
  >>> constA (Rest (Duration 1))
  -- /> (arrNote <+> arrRest)

arrNote :: SLA Int XmlTree Primitive
arrNote
  =   hasName "note"
  />  (arrPitch &&& arrDuration)
  >>^ (\((pc,oct),dur) -> Note pc oct Natural dur)


arrRest :: SLA Int XmlTree Primitive
arrRest
  =   hasName "note"
  />  hasName "rest"
  >>> arrDuration
  >>^ Rest

-- divisions per quarter note
arrDuration :: SLA Int XmlTree Duration
arrDuration
  =   hasName "duration"
  />  getText
  >>^ (\s -> case (read s :: Rational) of
               x -> Duration x)
               -- _ -> error $ "Unrecognized duration: " ++ s)

-------------
-- Pitches --
-------------

arrPitch :: (ArrowXml a) => a XmlTree (Pitchclass,Int)
arrPitch
  =   hasName "pitch"
  >>> (arrPitchclass &&& arrOctave)

arrPitchclass :: (ArrowXml a) => a XmlTree Pitchclass
arrPitchclass
  =   hasName "step" /> getText
  >>^ (\s -> case s of
               "A" -> A
               "B" -> B
               "C" -> C
               "D" -> D
               "E" -> E
               "F" -> F
               "G" -> G
               _   -> error $ "Unrecognized pitchclass: " ++ s)

arrOctave :: (ArrowXml a) => a XmlTree Int
arrOctave
  =   hasName "octave"
  />  getText
  >>^ read
