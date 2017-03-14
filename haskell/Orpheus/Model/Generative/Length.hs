{-# LANGUAGE DataKinds,
             FlexibleContexts #-}

module Orpheus.Model.Generative.Length where

import Orpheus.Data.Music.Diatonic

import Language.Hakaru.Syntax.Prelude
import Language.Hakaru.Types.DataKind
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Syntax.ABT

import Prelude hiding ((>>=),(==),(/),(+),fromRational)


{-

This uses a gamma/poisson model for the length of a piece

-}

mLength
  :: (ABT Term abt)
  => abt '[] 'HProb
  -> abt '[] 'HProb
  -> abt '[] ('HMeasure HPrimitive)
  -> abt '[] ('HMeasure ('HArray HPrimitive))
mLength shape scale mp =
  gamma shape scale >>= \g ->
    poisson g >>= \n ->
      plate n $ \_ ->
        mp


inferGammaPosterior
  :: (ABT Term abt)
  => abt '[] 'HProb
  -> abt '[] 'HProb
  -> abt '[] ('HArray ('HArray HPrimitive))
  -> abt '[] (hPair 'HProb 'HProb)
inferGammaPosterior shape scale dataset = undefined
  -- let  n = size(dataset)
  --      x = (summate i from 0 to (size dataset):
  --             size(dataset[i])) / n
  -- in pair (x + shape) (n + scale)
