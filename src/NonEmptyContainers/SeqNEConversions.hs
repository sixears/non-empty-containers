{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.SeqNEConversions
  ( AsMonoSeqNonEmpty( seqNE' ), FromMonoSeqNonEmpty( fromSeqNE )
  , IsMonoSeqNonEmpty( seqNE ), ToMonoSeqNonEmpty( toSeqNE, toSeq_ ) )
where

-- base --------------------------------

import qualified  Data.Foldable

import Data.Bifunctor      ( second )
import Data.Function       ( id )
import Data.Functor        ( fmap )
import Data.List.NonEmpty  ( NonEmpty, nonEmpty, toList )
import Data.Maybe          ( Maybe( Just ) )
import Data.Tuple          ( uncurry )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import Data.Sequence  ( Seq, fromList )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso', iso )
import Control.Lens.Prism  ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )
import Data.NonNull          ( NonNull, ncons, splitFirst, toNullable )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.SeqNE  as  SeqNE

import NonEmptyContainers.IsNonEmpty  ( fromNonEmpty, toNonEmpty )
import NonEmptyContainers.SeqNE       ( SeqNE( unSeqNE ), fromSeq )

--------------------------------------------------------------------------------

----------------------------------------
--        FromMonoSeqNonEmpty         --
----------------------------------------

{- | α that may be constructed from a non-empty Sequence of `Element α` -}
class FromMonoSeqNonEmpty α where
  fromSeqNE ∷ SeqNE (Element α) → α

instance {- α ~ Element (SeqNE α) ⇒ -} FromMonoSeqNonEmpty (SeqNE α) where
  fromSeqNE = id

instance {- α ~ Element (SeqNE α) ⇒ -} FromMonoSeqNonEmpty (Seq α) where
  fromSeqNE = SeqNE.toSeq

instance FromMonoSeqNonEmpty (NonNull [α]) where
  fromSeqNE ∷ SeqNE α → NonNull [α]
  fromSeqNE = uncurry ncons ∘ second Data.Foldable.toList ∘ splitFirst ∘ unSeqNE

instance FromMonoSeqNonEmpty (NonEmpty α) where
  fromSeqNE ∷ SeqNE α → NonEmpty α
  fromSeqNE = toNonEmpty

instance FromMonoSeqNonEmpty [α] where
  fromSeqNE ∷ SeqNE α → [α]
  fromSeqNE = toList ∘ toNonEmpty

----------------------------------------
--         ToMonoSeqNonEmpty          --
----------------------------------------

{- | α that may be converted to a non-empty Sequence of `Element α` -}
class ToMonoSeqNonEmpty α where
  toSeqNE ∷ α → SeqNE (Element α)
  {- | I'd really like to say "instance ToMonoSeqNonEmpty α ⇒ ToMonoSeq α",
       but this falls foul of the "constraint is no smaller than the instance
       head" issue.  So we provide this fn, in the expectation that instances of
       `ToMonoSeqNonEmpty` will also define instances as of `ToMonoSeq` with
       @toSeq = toSeq_@
   -}
  toSeq_ ∷ α → Seq (Element α)
  toSeq_ = SeqNE.toSeq ∘ toSeqNE

instance {- α ~ Element (SeqNE α) ⇒ -} ToMonoSeqNonEmpty (SeqNE α) where
  toSeqNE = id
  toSeq_  = SeqNE.toSeq

instance {- α ~ Element (SeqNE α) ⇒ -} ToMonoSeqNonEmpty (NonNull (Seq α)) where
  toSeqNE ∷ NonNull (Seq α) → SeqNE α
  toSeqNE = SeqNE.fromNonNullSeq
  toSeq_  = toNullable

instance ToMonoSeqNonEmpty (NonEmpty α) where
  toSeqNE ∷ NonEmpty α → SeqNE α
  toSeqNE = fromNonEmpty

instance α ~ Element (SeqNE α) ⇒ ToMonoSeqNonEmpty (NonNull [α]) where
  toSeqNE ∷ NonNull [α] → SeqNE α
  toSeqNE = toSeqNE ∘ uncurry ncons ∘ second fromList ∘ splitFirst
  toSeq_  ∷ NonNull [α] → Seq α
  toSeq_  = fromList ∘ uncurry (:) ∘ splitFirst


----------------------------------------
--         IsMonoSeqNonEmpty          --
----------------------------------------

{- | α that are isomorphic to a non-empty Sequence of `Element α` -}
class IsMonoSeqNonEmpty α where
  seqNE ∷ Iso' α (SeqNE (Element α))

instance IsMonoSeqNonEmpty (SeqNE α) where
  seqNE = id -- iso toSeqNE fromSeqNE

instance IsMonoSeqNonEmpty (NonNull [α]) where
  seqNE = iso toSeqNE fromSeqNE

instance IsMonoSeqNonEmpty (NonEmpty α) where
  seqNE = iso toSeqNE fromSeqNE

----------------------------------------
--         AsMonoSeqNonEmpty          --
----------------------------------------

{- | α that may be representable by a non-empty Sequence of `Element α`; i.e., a
     `SeqNE (Element α)` is always convertable to an α, and an α /may be/
     convertable to a `SeqNE (Element α)`.  So there must be a
     `FromMonoSeqNonEmpty` instance.
 -}
class FromMonoSeqNonEmpty α ⇒ AsMonoSeqNonEmpty α where
  toSeqNEMay ∷ α → Maybe (SeqNE (Element α))
  seqNE' ∷ Prism' α (SeqNE (Element α))
  seqNE' = prism' fromSeqNE toSeqNEMay

instance {- α ~ Element (SeqNE α) ⇒ -} AsMonoSeqNonEmpty (SeqNE α) where
  seqNE' = prism' id Just
  toSeqNEMay = Just

instance AsMonoSeqNonEmpty (Seq α) where
  seqNE' = -- prism' (SeqNE → Seq α) (Seq α → Maybe (SeqNE α))
           prism' SeqNE.toSeq fromSeq
  toSeqNEMay = fromSeq

instance AsMonoSeqNonEmpty (NonNull [α]) where
  seqNE' = -- prism' (SeqNE → NonNull [α]) (NonNull [α] → Maybe (SeqNE α))
           prism' fromSeqNE (Just ∘ toSeqNE)
  toSeqNEMay = Just ∘ toSeqNE

instance AsMonoSeqNonEmpty (NonEmpty α) where
  seqNE' = -- prism' (SeqNE → NonEmpty α) (NonEmpty α → Maybe (SeqNE α))
           prism' toNonEmpty (Just ∘ toSeqNE)
  toSeqNEMay = Just ∘ toSeqNE

instance AsMonoSeqNonEmpty [α] where
  seqNE' = -- prism' (SeqNE → [α]) ([α] → Maybe (SeqNE α))
           prism' Data.Foldable.toList (fmap fromNonEmpty ∘ nonEmpty)
  toSeqNEMay = fmap fromNonEmpty ∘ nonEmpty

-- that's all, folks! ----------------------------------------------------------
