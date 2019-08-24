{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  ( Error(..)
  , Field(..)
  ) where

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
-- Instantiation of classes

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError Error where
  missingInputError = MissingInput
  invalidInputFormat = InvalidInputFormat

-- | Instantiation of the standard Html5 fields.
instance Forge.FormField (Lucid.Html ()) Field Error where
  parseFieldInput key field input =
    case field of
      TextField ->
        case input of
          Forge.TextInput text -> pure text
          Forge.FileInput {} -> Left (Forge.invalidInputFormat key input)
      IntegerField ->
        case input of
          Forge.FileInput {} -> Left (Forge.invalidInputFormat key input)
          Forge.TextInput text ->
            case readMaybe (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat key input)
  viewField key =
    \case
      TextField -> Lucid.input_ [Lucid.name_ (Forge.unKey key)]
      IntegerField ->
        Lucid.input_ [Lucid.name_ (Forge.unKey key), Lucid.type_ "number"]
