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
--                                   DataSet                                  --
--------------------------------------------------------------------------------

type DataSet = [(Score (KeySig,TimeSig), Composer)]

data Composer = Bach | Beethoven | Horetzky
  deriving (Show,Eq,Generic)

instance Hashable Composer

--------------------------------------------------------------------------------
-- this is the path from the root orpheus project directory
dsDir :: FilePath
dsDir = "dataset"

composerFile :: Composer -> FilePath
composerFile Bach      = "bach.csv"
composerFile Beethoven = "beethoven.csv"
composerFile Horetzky  = "horetzky.csv"


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

-- this assumes that the filepaths are in the first column
csvToListOfFiles :: CSV -> [FilePath]
csvToListOfFiles c =
  let col = init . tail . getColumn 0 $ c
  in col

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
