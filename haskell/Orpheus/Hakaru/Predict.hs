{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Orpheus.Hakaru.Predict where

import           Prelude hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)

prog = 
  let_ (lam $ \ categories1 ->
        lam $ \ features2 ->
        lam $ \ params3 ->
        lam $ \ x4 ->
        let_ (array categories1 $
                    \ c6 ->
                    case_ (params3 ! c6)
                          [branch (ppair PVar PVar)
                                  (\ pc7 pfs8 ->
                                   product (nat_ 0)
                                           features2
                                           (\ i9 ->
                                            case_ (x4 ! i9)
                                                  [branch ptrue (fromProb (pfs8 ! i9)),
                                                   branch pfalse
                                                          (nat2real (nat_ 1) +
                                                           negate (fromProb (pfs8
                                                                             ! i9)))]))]) $ \ pCat5 ->
        let_ (lam $ \ c111 ->
              lam $ \ c212 ->
              case_ (pCat5 ! c212 < pCat5 ! c111 || pCat5 ! c111 == pCat5 ! c212)
                    [branch ptrue (c111), branch pfalse (c212)]) $ \ pMax10 ->
        reduce pMax10
               (nat_ 0)
               (array categories1 $ \ i13 -> i13)) $ \ mvBernNBPredict0 ->
  mvBernNBPredict0
