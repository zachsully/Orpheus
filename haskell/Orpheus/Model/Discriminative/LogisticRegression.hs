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
trainLogisticRegression l2_weight ds = undefined

testLogisticRegression :: Params -> x -> y
testLogisticRegression = undefined                       
