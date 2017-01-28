--------------------------------------------------------------------------------
--                                                                  2017.01.28
-- |
-- Module      :  Orpheus.Main
-- Copyright   :  Copyright (c) 2017 Zach Sullivan
-- License     :  BSD3
-- Maintainer  :  zsulliva@cs.uoregon.edu
-- Stability   :  experimental
-- Portability :  GHC-only
-- |
--
--------------------------------------------------------------------------------

module Main where

import Orpheus.Data.Music.Diatonic
import Orpheus.Data.Pretty
import Text.PrettyPrint       

main :: IO ()
main = putStrLn . render . prettyTopLevel $ maryHadALittleLamb
