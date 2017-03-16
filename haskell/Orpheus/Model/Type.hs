module Orpheus.Model.Type where

data Classifier x y model
  = Classifier { train   :: [(x,y)] -> model
               , predict :: model -> x -> y  }
