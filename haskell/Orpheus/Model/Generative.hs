{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Orpheus.Model.Generative where

-- import Language.Hakaru.Sample
import Language.Hakaru.Types.Sing
import Language.Hakaru.Syntax.Value
import Language.Hakaru.Syntax.ABT
import Language.Hakaru.Syntax.AST
import Language.Hakaru.Pretty.Concrete
import Language.Hakaru.Pretty.Haskell as HK
import qualified Text.PrettyPrint   as PP
import qualified System.Random.MWC  as MWC

--------------------------------------------------------------------------------
--                        Sampling From Generative Model                      --
--------------------------------------------------------------------------------
{-
  mDuration is defined recursively and never returns a sample. Should this just
  be modelled with a geometric distribution instead?

  BUG: geometric will run into an infinite loop when drawing samples

  What does this mean for mMusic which is recursive in several ways?
-}

illustrate :: Sing a -> MWC.GenIO -> Value a -> IO ()
illustrate (SMeasure s) g (VMeasure m) = do
    x <- m (VProb 1) g
    case x of
      Just (samp, _) -> illustrate s g samp
      Nothing        -> illustrate (SMeasure s) g (VMeasure m)
illustrate _ _ x = render x

render :: Value a -> IO ()
render = putStrLn . PP.renderStyle PP.style {PP.mode = PP.LeftMode} . prettyValue

prettyProg
  :: (ABT Term abt)
  => abt '[] a
  -> IO ()
prettyProg abt =
  putStrLn $ PP.renderStyle PP.style {PP.mode = PP.LeftMode}
    (PP.cat [ PP.text ("prog = ")
         , PP.nest 2 (HK.pretty abt)
         ])
