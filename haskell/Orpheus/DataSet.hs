module Orpheus.DataSet where

import Text.CSV       

-- this is the path from the root orpheus project directory
datasetDirectory :: FilePath
datasetDirectory = "dataset"

csvList :: [FilePath]                 
csvList = fmap (\f -> datasetDirectory ++ "/" ++ f)
               ["bach.csv","beethoven.csv","horetzky.csv"]

getCSVs :: IO [CSV]
getCSVs = do
  eitherCsvsOrError <- mapM parseCSVFromFile csvList
  return $ fmap (\ece -> case ece of
                           Left e -> error $ show e
                           Right x -> x)
                eitherCsvsOrError

dataCategories :: [CSV] -> [([String],String)]
dataCategories = undefined               

datasetSummary :: IO ()
datasetSummary = do
  -- csvs <- getCSVs
  -- mapM print csvs
  return ()               
