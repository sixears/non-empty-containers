module NonEmptyContainers.NonEmptyHashSet
  ( NonEmptyHashSet, singleton, fromList, toList, toNEList )
where

import Base1  hiding  ( fromList, toList )

-- base --------------------------------

import qualified  Data.List.NonEmpty
import qualified  GHC.Exts

import Data.Foldable   ( Foldable )

-- mono-traversable --------------------

import Data.NonNull          ( NonNull, impureNonNull, toNullable )

-- text --------------------------------

import Data.Text  ( append, intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- unordered-containers ----------------

import qualified  Data.HashSet

-------------------------------------------------------------------------------

newtype NonEmptyHashSet a = NonEmptyHashSet (NonNull (Data.HashSet.HashSet a))
  deriving (Eq, Show)

toHashSet ∷ NonEmptyHashSet a → Data.HashSet.HashSet a
toHashSet (NonEmptyHashSet hs) = toNullable hs

toList ∷ NonEmptyHashSet a → [a]
toList = Data.HashSet.toList ∘ toHashSet

toNEList ∷ NonEmptyHashSet a → NonEmpty a
toNEList = Data.List.NonEmpty.fromList ∘ toList

fromList ∷ (Hashable a, Eq a) ⇒ NonEmpty a → NonEmptyHashSet a
fromList =
  NonEmptyHashSet ∘ impureNonNull ∘ Data.HashSet.fromList ∘ GHC.Exts.toList

instance (Hashable a, Eq a) ⇒ Semigroup (NonEmptyHashSet a) where
  NonEmptyHashSet a <> NonEmptyHashSet b = NonEmptyHashSet (a <> b)

instance Foldable NonEmptyHashSet where
  foldr f b = Data.HashSet.foldr f b ∘ toHashSet

instance Printable a ⇒ Printable (NonEmptyHashSet a) where
  print as =
    P.text $ "<" `append` intercalate "," (toText ⊳ toList as) `append` ">"

singleton ∷ Hashable a ⇒ a → NonEmptyHashSet a
singleton = NonEmptyHashSet ∘ impureNonNull ∘ Data.HashSet.singleton

-- that's all, folks! ---------------------------------------------------------
