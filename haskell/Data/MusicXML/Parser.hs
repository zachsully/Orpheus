module Data.MusicXML.Parser (parseMusicXMLFile,tryFixXMLFile) where

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

tryFixXMLFile :: FilePath -> FilePath -> IO ()
tryFixXMLFile fin fout = do
  runX $ readDocument [ withValidate         False
                      , withSubstDTDEntities False
                      , withRemoveWS         True
                      ] fin
     >>> putXmlDocument True fout
  return ()


errorA :: (ArrowXml a, Show b) => a b c
errorA = arr (error . show)

--------------------------------------------------------------------------------
--                               Score Partwise                               --
--------------------------------------------------------------------------------
-- ^ Takes the top level XmlTree and returns a Score from it
arrScorePartwise :: (ArrowXml a) => a XmlTree (Score Ctx)
arrScorePartwise
  =   (listA (hasName "score-partwise" /> hasName "part"
              >>> (listA getChildren)
              >>> arrPart))
  >>^ Score

-- A part is probably a single instrument
-- ^ Takes a part node and returns Part data from them
arrPart :: (ArrowXml a) => a [XmlTree] (Part Ctx)
arrPart = arrVoice >>^ Part

-- A voice will be a single instrument and can contain chords
-- we will need to coalesce measures in the same context, eventually
-- ^ Takes a part node and extracts all the measures from it using a list arrow
--   monad that holds a parasing context
arrVoice :: (ArrowXml a) => a [XmlTree] (Voice Ctx)
arrVoice
  =   fromSLA initState
              (listA (unlistA >>> hasName "measure" /> arrMeasure))
  >>^ Voice
  where initState = error "no state set" :: (Ctx,Rational)

-- > measures are sequences of notes,
-- > they can also contain attributes including contexts for the next sequence
--   of notes
-- ^ Takes children of a measure node and a state
arrMeasure :: SLA (Ctx,Rational) XmlTree (Ctx,[[Primitive]])
arrMeasure
  =   perform (arrContext >>> setState)
  >>> arrParPrimitive

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
         in m . read $! fs)
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
arrParPrimitive :: SLA (Ctx,Rational) XmlTree (Ctx,[[Primitive]])
arrParPrimitive = (getState >>^ fst) &&& listA (listA arrPrimitive)

-- primitives are just notes and rests, will probably need to handle
-- chords here as well
arrPrimitive :: SLA (Ctx,Rational) XmlTree Primitive
arrPrimitive
  =   hasName "note"
  -- >>> constA (Rest (Duration 1))
  /> (arrNote <+> arrRest)

arrNote :: SLA (Ctx,Rational) XmlTree Primitive
arrNote
  =   hasName "note"
  />  (arrPitch &&& arrDuration)
  >>^ (\((pc,oct),dur) -> Note pc oct Natural dur)


arrRest :: SLA (Ctx,Rational) XmlTree Primitive
arrRest
  =   hasName "note"
  />  hasName "rest"
  >>> arrDuration
  >>^ Rest

-- divisions per quarter note
arrDuration :: SLA (Ctx,Rational) XmlTree Duration
arrDuration
  =   hasName "duration"
  />  getText
  >>> (accessState $ \(_,divs) t ->
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
