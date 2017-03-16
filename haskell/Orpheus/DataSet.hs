{-# LANGUAGE DeriveGeneric #-}
module Orpheus.DataSet where

import GHC.Generics (Generic)
import Control.Monad
import Text.CSV
import System.FilePath.Posix
import Orpheus.Data.Music.Context
import Data.Hashable
import Data.MusicXML.Parser

--------------------------------------------------------------------------------
--                                DataSet Types                               --
--------------------------------------------------------------------------------

type DataSet = [(Score (KeySig,TimeSig), Composer)]

data Composer = Bach | Beethoven | Horetzky
  deriving (Show,Eq,Generic)

instance Hashable Composer

--------------------------------------------------------------------------------
--                                   IO                                       --
--------------------------------------------------------------------------------

getCSVs :: IO [(CSV,Composer)]
getCSVs
  = forM [Bach,Beethoven,Horetzky] $ \c -> do
      res <- parseCSVFromFile (dsDir </> composerFile c)
      case res of
        Left e  -> error  (show e)
        Right x -> return (x,c)

getDataSet :: IO DataSet
getDataSet = do
  csvs <- getCSVs
  let filesAndComposers = fmap (\(d,c) -> (csvToListOfFiles d,c)) csvs
      fAndC = foldr (\(fs,c) acc -> (fmap (\f -> (f,c)) fs) ++ acc)
                    []
                    filesAndComposers
  forM fAndC $ \(f,c) -> do
    scr <- parseMusicXMLFile (dsDir </> "xml" </> f)
    return (scr,c)

-----------
-- Paths --
-----------

dsDir :: FilePath
dsDir = "dataset"

composerFile :: Composer -> FilePath
composerFile Bach      = "bach.csv"
composerFile Beethoven = "beethoven.csv"
composerFile Horetzky  = "horetzky.csv"

-- this assumes that the filepaths are in the first column
csvToListOfFiles :: CSV -> [FilePath]
csvToListOfFiles c =
  let col = init . tail . getColumn 0 $ c
  in col

--------------------------------------------------------------------------------
--                                Partitioning                                --
--------------------------------------------------------------------------------
{-
Ideally, I would like to have the ability to partition data randomly. I would
like to have a 50/25/25 partition and a 70/30 partition
-}

trainValidTestPartition :: DataSet -> (DataSet,DataSet,DataSet)
trainValidTestPartition = undefined

trainTestPartition :: DataSet -> (DataSet,DataSet)
trainTestPartition = undefined


--------------------------------------------------------------------------------
--                                 Summaries                                  --
--------------------------------------------------------------------------------

scoreSummary :: Score (KeySig,TimeSig) -> IO ()
scoreSummary score =
  case score of
    Score parts -> do
      putStrLn . concat $ ["Number of parts: ",show . length $ parts]
      case head parts of
        Part (Voice xs) ->
          putStrLn . concat $ ["Number of measures: ",show . length $ xs]


datasetSummary :: DataSet -> IO ()
datasetSummary ds = do
  putStrLn "Dataset Summary:"
  putStrLn $ summaryC Bach
  putStrLn $ summaryC Beethoven
  putStrLn $ summaryC Horetzky
  putStrLn "\n"
  where summaryC c = concat [ show c
                            , " pieces #"
                            , show . length $ filter (\(_,c') -> c' == c) ds
                            ]

--------------------------------------------------------------------------------
--                               CSV Functions                                --
--------------------------------------------------------------------------------

getRow :: Int -> [[a]] -> [a]
getRow n = head . drop n

getColumn :: Int -> [[a]] -> [a]
getColumn n = fmap (head . drop n)
