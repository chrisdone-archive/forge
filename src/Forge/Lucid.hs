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

import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.String
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
  | InvalidInputFormat Forge.Key (NonEmpty Forge.Input)
  | InvalidMultiselectKey Text
  deriving (Show, Eq)

-- | A standard Html5 field.
data Field a where
  TextField :: Field Text
  IntegerField :: Field Integer
  MultiselectField :: NonEmpty (a, Text) -> Field [a]

--------------------------------------------------------------------------------
-- Instantiation of classes

-- | Limited to the sum type 'Error' defined in this module.
instance Forge.FormError Error where
  missingInputError = MissingInput
  invalidInputFormat = InvalidInputFormat

-- | Instantiation of the standard Html5 fields.
instance (Forge.FormError error) =>
         Forge.FormField (Lucid.Html ()) Field error where
  parseFieldInput key field input =
    case field of
      TextField ->
        case input of
          Forge.TextInput text :| [] -> pure text
          _ -> Left (Forge.invalidInputFormat key input)
      IntegerField ->
        case input of
          Forge.TextInput text :| [] ->
            case readMaybe (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat key input)
          _ -> Left (Forge.invalidInputFormat key input)
      MultiselectField choices -> do
        keys <-
          mapM
            (\case
               (Forge.TextInput text) -> Right text
               _ -> Left (Forge.invalidInputFormat key input))
            input
        values <-
          mapM
            (\k ->
               case lookup k keyedChoices of
                 Nothing -> Left (Forge.invalidInputFormat key input)
                 Just ok -> pure ok)
            keys
        pure (toList values)
        where keyedChoices =
                map
                  (\(i, (value, title)) -> (uniqueKey i title, value))
                  (zip [0 :: Integer ..] (toList choices))
  viewField key minput =
    \case
      TextField ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key)] <>
           [Lucid.value_ value | Just (Forge.TextInput value :| []) <- [minput]])
      IntegerField ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key), Lucid.type_ "number"] <>
           [Lucid.value_ value | Just (Forge.TextInput value :| []) <- [minput]])
      MultiselectField choices ->
        Lucid.select_
          [Lucid.name_ (Forge.unKey key), Lucid.multiple_ "multiple"]
          (mapM_
             (\(i, (_, k)) ->
                Lucid.option_ [Lucid.value_ (uniqueKey i k)] (Lucid.toHtml k))
             (zip [0 :: Integer ..] (toList choices)))

-- | A key which is unique with respect to a list index and its display.
uniqueKey :: Integer -> Text -> Text
uniqueKey i title = fromString (show i) <> ":" <> title
