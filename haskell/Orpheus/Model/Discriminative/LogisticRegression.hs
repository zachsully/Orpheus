module Orpheus.Model.Discriminative.LogisticRegression where

import Orpheus.Model.Type

--------------------------------------------------------------------------------
--                       Logistic Regression Classifier                       --
--------------------------------------------------------------------------------

type Params = ([Double],Double)

logisticRegression :: Classifier x y Params
logisticRegression = Classifier { train   = trainLogisticRegression 10
                                , predict = testLogisticRegression }

trainLogisticRegression :: Double -> [(x,y)] -> Params
trainLogisticRegression _ _ = undefined

testLogisticRegression :: Params -> x -> y
testLogisticRegression = undefined
