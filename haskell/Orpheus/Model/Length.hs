module Orpheus.Model.Length where

{-

This uses a gamma/poisson model for the length of a piece

-}

mLength
  :: (ABT Term abt)
  => abt '[] 'HProb
  -> abt '[] 'HProb
  -> abt '[] ('HMeasure HPrim)
  -> abt '[] ('HMeasure ('HArray HPrim))
mLength alpha beta mp =
  gamma alpha beta >>= \g ->
    poisson g >>= \n ->
      plate n $ \_ ->
        mp


inferGammaLength
  :: (ABT Term abt)
  => abt '[] 'HProb
  -> abt '[] 'HProb
  -> abt '[] ('HArray ('HArray HPrim))
  -> abt '[] (hPair 'HProb 'HProb)
inferGammaLength alpha beta dataset =
  let  n = size(dataset)
       x = (summate i from 0 to (size dataset):
              size(dataset[i])) / n
  in pair (x + alpha) (n + beta)
