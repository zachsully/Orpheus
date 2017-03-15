module Data.MusicXML.Parser
  ( parseMusicXMLFile
  , prettyXMLFile
  ) where

import Orpheus.Data.Music.Context
import Text.XML.HXT.Core

{- Currently Failing to Parse:
025000B_.xml
025100B_.xml
025200B_.xml
026700B_.xml
027700B_.xml
028100B_.xml
028200B_.xml
028400B_.xml
030300B_.xml
030700B_.xml
030800B_.xml
031000B_.xml
031800B_.xml
032300B_.xml
032400B_.xml
032500B_.xml
032800B_.xml
033200B_.xml
033300B_.xml
033500B_.xml
035000B_.xml
035100B_.xml
035200B_.xml
035600B_.xml
036100B_.xml
036200B_.xml
036500B_.xml
036700B_.xml
036800B_.xml
037000B_.xml
...
-}

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

prettyXMLFile :: FilePath -> FilePath -> IO ()
prettyXMLFile fin fout =
  (runX $ readDocument [ withValidate         False
                       , withSubstDTDEntities False
                       , withRemoveWS         True
                       ] fin
     >>> indentDoc
     >>> putXmlDocument True fout)
  >> return ()


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
{-
Primitives are inside "note" tags and there are several things that need to be
parsed at once: pitch or rest, duration, and maybe chord tags. Therefore, we
pass the primitive information as a list to each arrow
-}
-- A chord in MusicXml is a series of notes that where the 2nd,...,nth
-- note also contains the element <chord/>
arrParPrimitive :: SLA (Ctx,Rational) XmlTree (Ctx,[[Primitive]])
arrParPrimitive = (getState >>^ fst) &&& listA arrSeqPrimitive

-- primitives are just notes and rests, will probably need to handle
-- chords here as well
arrSeqPrimitive :: SLA (Ctx,Rational) XmlTree [Primitive]
arrSeqPrimitive
  =   listA (hasName "note" >>> getChildren) -- pass all "note" information to subarrows
  >>> listA (arrNote <+> arrRest)

arrNote :: SLA (Ctx,Rational) [XmlTree] Primitive
arrNote
  =   (arrPitch &&& arrDuration)
  >>^ (\((pc,oct),dur) -> Note pc oct Natural dur)


-- ^ will return a rest if there is a rest tage in the note data
arrRest :: SLA (Ctx,Rational) [XmlTree] Primitive
arrRest = arrDuration >>^ Rest

-- divisions per quarter note
-- ^ Takes a list of "note" trees and returns a duration
arrDuration :: SLA (Ctx,Rational) [XmlTree] Duration
arrDuration
  =   unlistA
  >>> (hasName "duration" /> getText)
  >>> (accessState $ \(_,divs) t ->
         case (read t :: Int) of
            int -> Duration (4 * (toRational int) / divs))

-------------
-- Pitches --
-------------

-- ^ Takes a list of "note" trees and returns a pitch and octave
arrPitch :: (ArrowXml a) => a [XmlTree] (Pitchclass,Int)
arrPitch
  =   unlistA
  >>> listA (hasName "pitch" >>> getChildren)
  >>> (arrPitchclass &&& arrOctave)

-- ^ Takes a list of "pitch" trees and returns the pitchclass
arrPitchclass :: (ArrowXml a) => a [XmlTree] Pitchclass
arrPitchclass
  =   unlistA
  >>> hasName "step"
  />  getText
  >>^ (\s -> case s of
               "A" -> A
               "B" -> B
               "C" -> C
               "D" -> D
               "E" -> E
               "F" -> F
               "G" -> G
               _   -> error $ "Unrecognized pitchclass: " ++ s)

-- ^ Takes a list of "pitch" trees and returns the octave
arrOctave :: (ArrowXml a) => a [XmlTree] Int
arrOctave = unlistA >>> hasName "octave" /> getText >>^ read
