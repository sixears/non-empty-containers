{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module NonEmptyContainers.IsNonEmpty
  ( FromMonoNonEmpty(..), IsMonoNonEmpty(..), ToMonoNonEmpty(..)
  , defaultNonEmpty )
where

-- base --------------------------------

import Data.Function       ( id )
import Data.List.NonEmpty  ( NonEmpty )

-- lens --------------------------------

import Control.Lens.Iso   ( Iso', iso )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class FromMonoNonEmpty α where
  fromNonEmpty ∷ NonEmpty (Element α) → α

instance FromMonoNonEmpty (NonEmpty α) where
  fromNonEmpty = id

------------------------------------------------------------

class ToMonoNonEmpty α where
  toNonEmpty ∷ α → NonEmpty (Element α)

instance ToMonoNonEmpty (NonEmpty α) where
  toNonEmpty = id

------------------------------------------------------------

class IsMonoNonEmpty α where
  nonEmpty ∷ Iso' α (NonEmpty (Element α))

{- | pre-made "instance" of `IsNonEmpty` for instances of
    `FromNonEmpty` & `ToNonEmpty` -}
defaultNonEmpty ∷ (FromMonoNonEmpty α, ToMonoNonEmpty α) ⇒
                  Iso' α (NonEmpty (Element α))
defaultNonEmpty = iso toNonEmpty fromNonEmpty

instance IsMonoNonEmpty (NonEmpty α) where
  nonEmpty = defaultNonEmpty

-- that's all, folks! ----------------------------------------------------------
