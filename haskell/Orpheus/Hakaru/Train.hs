{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Orpheus.Hakaru.Train where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)

prog = 
  let_ (lam $ \ categories1 ->
        lam $ \ features2 ->
        lam $ \ train_set3 ->
        (array categories1 $
               \ c4 ->
               let_ (summate (nat_ 0)
                             (size train_set3)
                             (\ i6 ->
                              case_ (train_set3 ! i6)
                                    [branch (ppair PVar PVar)
                                            (\ fs7 ic8 ->
                                             case_ (ic8 == c4)
                                                   [branch pfalse (nat_ 0),
                                                    branch ptrue (nat_ 1)])])) $ \ ccount5 ->
               let_ (array features2 $
                           \ i10 ->
                           let_ (summate (nat_ 0)
                                         (size train_set3)
                                         (\ i12 ->
                                          case_ (train_set3 ! i12)
                                                [branch (ppair PVar PVar)
                                                        (\ fs13 ic14 ->
                                                         case_ (ic14 == c4)
                                                               [branch pfalse (nat_ 0),
                                                                branch ptrue
                                                                       (case_ (fs13 ! i12)
                                                                              [branch pfalse
                                                                                      (nat_ 0),
                                                                               branch ptrue
                                                                                      (nat_ 1)])])])) $ \ fcount11 ->
                           nat2prob (fcount11 + nat_ 1) *
                           recip (nat2prob (ccount5 + nat_ 2))) $ \ condps9 ->
               ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SProb) (SArray SProb)) (SPlus (SEt (SKonst SProb) (SEt (SKonst (SArray SProb)) SDone)) SVoid))
                    (pair (nat2prob ccount5 * recip (nat2prob categories1))
                          condps9))) $ \ mvBernNBTrain0 ->
  mvBernNBTrain0
