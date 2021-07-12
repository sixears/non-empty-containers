module NonEmptyContainers.SeqConversions
  ( AsSeq( seq' ), FromSeq( fromList, fromSeq ), IsSeq( seq )
  , ToSeq( toSeq ), stripPrefix, tests )
where

-- base --------------------------------

import qualified  Data.Foldable

import Control.Applicative  ( pure )
import Data.Eq              ( Eq )
import Data.Function        ( ($), id )
import Data.List.NonEmpty   ( NonEmpty, toList )
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

import Control.Lens.Iso    ( Iso', iso )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )
import Data.NonNull          ( NonNull, toNullable )

-- more-unicode ------------------------

import Data.MoreUnicode.Monoid   ( ф )
import Data.MoreUnicode.Natural  ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.SeqNE  ( SeqNE, (⪬) )

--------------------------------------------------------------------------------

----------------------------------------
--              FromSeq               --
----------------------------------------

{- | α that may be constructed from a (empty?) Sequence of `Element α` -}
class FromSeq α where
  fromSeq ∷ Seq (Element α) → α
  fromList ∷ [Element α] → α
  fromList = fromSeq ∘ Seq.fromList

instance FromSeq (Seq α) where
  fromSeq = id
  fromList ∷ [α] → Seq α
  fromList = Seq.fromList

instance FromSeq [α] where
  fromSeq ∷ Seq α → [α]
  fromSeq = Data.Foldable.toList
  fromList = id

----------------------------------------
--             ToSeq              --
----------------------------------------

{- | α that may be converted to a (empty?) Sequence of `Element α` -}
class ToSeq α where
  toSeq ∷ α → Seq (Element α)

instance {- α ~ Element (Seq α) ⇒ -} ToSeq (Seq α) where
  toSeq = id

instance {- α ~ Element (SeqNE α) ⇒ -} ToSeq (SeqNE α) where
  toSeq = SeqNE.toSeq

instance ToSeq [α] where
  toSeq = Seq.fromList

instance ToSeq (NonEmpty α) where
  toSeq = toSeq ∘ toList

instance ToSeq (NonNull [α]) where
  toSeq = toSeq ∘ toNullable

----------------------------------------
--               IsSeq                --
----------------------------------------

{- | α that are isomorphic to a (empty?) Sequence of `Element α` -}
class IsSeq α where
  seq ∷ Iso' α (Seq (Element α))

instance IsSeq (Seq α) where
  seq = id

instance IsSeq [α] where
  seq = iso toSeq fromSeq

----------------------------------------
--               AsSeq                --
----------------------------------------

{- | α that may be representable by a Sequence of `Element α`; i.e., a
     `Seq (Element α)` is always convertable to an α, and an α /may be/
     convertable to a `Seq (Element α)`.  So there must be a `FromSeq`
     instance.
 -}
class FromSeq α ⇒ AsSeq α where
  toSeqMay ∷ α → Maybe (Seq (Element α))
  seq' ∷ Prism' α (Seq (Element α))
  seq' = prism' fromSeq toSeqMay

instance {- α ~ Element (Seq α) ⇒ -} AsSeq (Seq α) where
--  seq' = id -- prism' id Just
  toSeqMay = Just

instance AsSeq [α] where
  seq' = -- prism' (Seq α → [α]) ([α] → Maybe (Seq α))
         prism' fromSeq (Just ∘ toSeq)
  toSeqMay = Just ∘ toSeq

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
                [ testCase "null"   $ Just (pure 1) @=? stripPrefix ф ones
                , testCase "pfx"    $ Just (pure 2) @=? stripPrefix ones onetwo
                , testCase "no pfx" $ Nothing       @=? stripPrefix twos ones
                , testCase "equal"  $ Just ф        @=? stripPrefix ones ones
                , testCase "longer" $ Nothing       @=? stripPrefix onetwo ones
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
