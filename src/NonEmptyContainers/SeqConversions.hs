{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module NonEmptyContainers.SeqConversions
  ( AsMonoSeq( seq' ), FromMonoSeq( fromList, fromSeq ), IsMonoSeq( seq )
  , ToMonoSeq( toSeq ), stripPrefix, tests )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.String          ( String )
import System.Exit          ( ExitCode )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import qualified  Data.Sequence  as  Seq

import Data.Sequence  ( Seq( (:<|) ) )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso' )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Monoid   ( ф )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.Tasty    ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.SeqNE  ( SeqNE, (⪬) )

--------------------------------------------------------------------------------

----------------------------------------
--            FromMonoSeq             --
----------------------------------------

{- | α that may be constructed from a (empty?) Sequence of `Element α` -}
class FromMonoSeq α where
  fromSeq ∷ Seq (Element α) → α
  fromList ∷ [Element α] → α
  fromList = fromSeq ∘ Seq.fromList

----------------------------------------
--             ToMonoSeq              --
----------------------------------------

{- | α that may be converted to a (empty?) Sequence of `Element α` -}
class ToMonoSeq α where
  toSeq ∷ α → Seq (Element α)

instance α ~ Element (SeqNE α) ⇒ ToMonoSeq (SeqNE α) where
  toSeq = SeqNE.toSeq

instance α ~ Element (Seq α) ⇒ ToMonoSeq (Seq α) where
  toSeq = id

----------------------------------------
--             IsMonoSeq              --
----------------------------------------

{- | α that are isomorphic to a (empty?) Sequence of `Element α` -}
class IsMonoSeq α where
  seq ∷ Iso' α (Seq (Element α))

----------------------------------------
--             AsMonoSeq              --
----------------------------------------

{- | α that may be representable by a non-empty Sequence of `Element α` -}
class AsMonoSeq α where
  seq' ∷ Prism' α (Seq (Element α))

instance α ~ Element (Seq α) ⇒ AsMonoSeq (Seq α) where
  seq' = prism' id Just

------------------------------------------------------------

{- | Strip a prefix from a `Seq`, possibly resulting in an empty `Seq`.  This
     isn't really a conversion, just a fn missing from the original `Seq`
     datatype.
 -}

stripPrefix ∷ Eq α ⇒ Seq α → Seq α  → Maybe (Seq α)
stripPrefix (SeqNE.toSeq → x :<| xs) (y :<| ys) | x ≡ y = stripPrefix xs ys
stripPrefix (SeqNE.toSeq → Seq.Empty) s = Just s
stripPrefix _ _ = Nothing

--------------------

stripPrefixTests ∷ TestTree
stripPrefixTests =
  let ones   = pure 1  ∷ Seq ℕ
      twos   = pure 2  ∷ Seq ℕ
      onetwo = 1 ⪬ [2] ∷ Seq ℕ
   in testGroup "stripPrefix"
                [ testCase "null"   $ Just (pure 1) ≟ stripPrefix ф ones
                , testCase "pfx"    $ Just (pure 2) ≟ stripPrefix ones onetwo
                , testCase "no pfx" $ Nothing       ≟ stripPrefix twos ones
                , testCase "equal"  $ Just ф        ≟ stripPrefix ones ones
                , testCase "longer" $ Nothing       ≟ stripPrefix onetwo ones
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "SeqConversions" [ stripPrefixTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
