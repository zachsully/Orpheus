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
  =   listA (hasName "score-partwise" /> arrPart)
  >>^ Score

-- A part is probably a single instrument
arrPart :: (ArrowXml a) => a XmlTree (Part Ctx)
arrPart
  =   hasName "part"
  />  arrVoice
  >>^ Part

-- A voice will be a single instrument and can contain chords
-- we will need to coalesce measures in the same context, eventually
arrVoice :: (ArrowXml a) => a XmlTree (Voice Ctx)
arrVoice
  =   listA (fromSLA (error "no state set" :: Rational) arrMeasure)
  >>^ Voice

-- > measures are sequences of notes,
-- > they can also contain attributes including contexts for the next sequence
--   of notes
arrMeasure :: SLA Rational XmlTree (Ctx,[[Primitive]])
arrMeasure
  =   hasName "measure"
  />  (arrContext >>^ (error . show))
  >>> (changeState $ \_ (_,divs) -> divs)
  >>^ (\(ctx,_) -> (ctx,[]))

-- A context here, represents the KeySignature,TimeSignature
-- we also attach a divisions integer that will be used for deciding note length
arrContext :: (ArrowXml a) => a XmlTree (Ctx,Rational)
arrContext
  =   (listA (hasName "attributes" />  returnA))
  >>> ((arrKey &&& arrTime) &&& arrDivisions)

arrDivisions :: (ArrowXml a) => a [XmlTree] Rational
arrDivisions
  =   unlistA
  >>> hasName "divisions"
  />  getText
  >>^ (read :: String -> Int)
  >>^ fromIntegral

arrKey :: (ArrowXml a) => a [XmlTree] KeySig
arrKey
  =   unlistA
  >>> (hasName "key" >>> listA getChildren)
  >>> (arrFifths &&& arrMode)
  >>^ (\(fs,mode) ->
         let m = case mode of
                   "minor" -> Minor
                   "major" -> Major
                   _ -> error $ "Unrecognized mode: " ++ mode
         in m . read $ fs)
  where arrFifths = unlistA >>> hasName "fifths" /> getText
        arrMode   = unlistA >>> hasName "mode" /> getText

arrTime :: (ArrowXml a) => a [XmlTree] TimeSig
arrTime
  =   unlistA
  >>> (hasName "time" >>> listA getChildren)
  >>> (arrBeats &&& arrBeatType)
  >>^ (\(b,bt) -> TimeSig b bt)
  where arrBeats = unlistA >>> hasName "beats" /> getText >>^ read
        arrBeatType = unlistA >>> hasName "beat-type" /> getText >>^ read

--------------------------------------------------------------------------------
--                                  Primitives                                --
--------------------------------------------------------------------------------

-- A chord in MusicXml is a series of notes that where the 2nd,...,nth
-- note also contains the element <chord/>
arrParPrimitive :: SLA Rational XmlTree [Primitive]
arrParPrimitive = listA arrPrimitive

-- primitives are just notes and rests, will probably need to handle
-- chords here as well
arrPrimitive :: SLA Rational XmlTree Primitive
arrPrimitive
  =   hasName "note"
  >>> constA (Rest (Duration 1))
  -- /> (arrNote <+> arrRest)

arrNote :: SLA Rational XmlTree Primitive
arrNote
  =   hasName "note"
  />  (arrPitch &&& arrDuration)
  >>^ (\((pc,oct),dur) -> Note pc oct Natural dur)


arrRest :: SLA Rational XmlTree Primitive
arrRest
  =   hasName "note"
  />  hasName "rest"
  >>> arrDuration
  >>^ Rest

-- divisions per quarter note
arrDuration :: SLA Rational XmlTree Duration
arrDuration
  =   hasName "duration"
  />  getText
  >>> (accessState $ \divs t ->
         case (read t :: Rational) of
            x -> Duration (4 * x / divs))

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
