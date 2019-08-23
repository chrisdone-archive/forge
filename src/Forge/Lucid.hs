{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

-- | API for form fields using the Lucid HTML generation library for
-- the view.

module Forge.Lucid
  ( Lucid
  , LucidUnverified
  , Error(..)
  , Field(..)
  ) where

import           Data.Functor.Identity
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Forge.Internal.Types as Forge
import qualified Lucid
import           Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Data types for this interface

-- | The errors possible with a lucid form.
data Error
  = MissingInput Forge.Key
  | InvalidInputFormat Forge.Key Forge.Input
  deriving (Show, Eq)

-- | A standard Html5 field.
data Field a where
  TextField :: Field Text
  IntegerField :: Field Integer

--------------------------------------------------------------------------------
-- Instantiation of type families

-- | The index for this instantiation of Forge.
data Lucid

-- | Lucid forms by default are pure.
instance Forge.FormVerification Lucid where
  type NameStatus Lucid = 'Forge.FieldNamesOk

-- | Lucid forms by default are pure.
instance Forge.FormAction Lucid where
  type Action Lucid = Identity

-- | Renders straight to a Lucid renderer.
instance Forge.FormView Lucid where
  type View Lucid = Lucid.Html ()

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError Lucid where
  type Error Lucid = Error
  missingInputError = MissingInput
  invalidInputFormat = InvalidInputFormat

-- | Instantiation of the standard Html5 fields.
instance Forge.FormField Lucid where
  type Field Lucid = Field
  parseFieldInput key field input =
    case field of
      TextField ->
        case input of
          Forge.TextInput text -> pure text
          Forge.FileInput {} -> Left (Forge.invalidInputFormat @Lucid key input)
      IntegerField ->
        case input of
          Forge.FileInput {} -> Left (Forge.invalidInputFormat @Lucid key input)
          Forge.TextInput text ->
            case readMaybe (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat @Lucid key input)
  viewField key =
    \case
      TextField -> Lucid.input_ [Lucid.name_ (Forge.unKey key)]
      IntegerField ->
        Lucid.input_ [Lucid.name_ (Forge.unKey key), Lucid.type_ "number"]

-- | The index for this instantiation of Forge.
data LucidUnverified

-- | Lucid forms by default are pure.
instance Forge.FormVerification LucidUnverified where
  type NameStatus LucidUnverified = 'Forge.FieldNamesNeedVerification

-- | Lucid forms by default are pure.
instance Forge.FormAction LucidUnverified where
  type Action LucidUnverified = Identity

-- | Renders straight to a Lucid renderer.
instance Forge.FormView LucidUnverified where
  type View LucidUnverified = Lucid.Html ()

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError LucidUnverified where
  type Error LucidUnverified = Error
  missingInputError = MissingInput
  invalidInputFormat = InvalidInputFormat

-- | Instantiation of the standard Html5 fields.
instance Forge.FormField LucidUnverified where
  type Field LucidUnverified = Field
  parseFieldInput key field input =
    case field of
      TextField ->
        case input of
          Forge.TextInput text -> pure text
          Forge.FileInput {} -> Left (Forge.invalidInputFormat @LucidUnverified key input)
      IntegerField ->
        case input of
          Forge.FileInput {} -> Left (Forge.invalidInputFormat @LucidUnverified key input)
          Forge.TextInput text ->
            case readMaybe (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat @LucidUnverified key input)
  viewField key =
    \case
      TextField -> Lucid.input_ [Lucid.name_ (Forge.unKey key)]
      IntegerField ->
        Lucid.input_ [Lucid.name_ (Forge.unKey key), Lucid.type_ "number"]
