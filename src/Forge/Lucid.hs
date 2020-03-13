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

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Fixed
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Forge.Internal.Types as Forge
import qualified Lucid
import           Text.Email.Validate as Email
import           Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Data types for this interface

-- | The errors possible with a lucid form.
data Error
  = MissingInput Forge.Key
  | InvalidInputFormat Forge.Key (NonEmpty Forge.Input)
  deriving (Show, Eq)

-- | A standard Html5 field.
data Field a where
  TextField :: Maybe Text -> Field Text
  TextareaField :: Maybe Text -> Field Text
  IntegerField :: Maybe Integer -> Field Integer
  MultiselectField :: Eq a => Maybe [a] -> NonEmpty (a, Text) -> Field [a]
  DropdownField :: Eq a => Maybe a -> NonEmpty (a, Text) -> Field a
  EmailField :: Maybe EmailAddress -> Field EmailAddress
  FixedField :: HasResolution a => Maybe (Fixed a) -> Field (Fixed a)
  PhoneField :: Maybe Text -> Field Text

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
      TextField _ ->
        case input of
          Forge.TextInput text :| [] -> pure text
          _ -> Left (Forge.invalidInputFormat key input)
      TextareaField _ ->
        case input of
          Forge.TextInput textarea :| [] -> pure textarea
          _ -> Left (Forge.invalidInputFormat key input)
      PhoneField _ ->
        case input of
          Forge.TextInput phone :| [] -> pure phone
          _ -> Left (Forge.invalidInputFormat key input)
      IntegerField _ ->
        case input of
          Forge.TextInput text :| [] ->
            case readMaybe (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat key input)
          _ -> Left (Forge.invalidInputFormat key input)

      FixedField _ ->
        case input of
          Forge.TextInput text :| [] ->
            case readFixed (T.unpack text) of
              Just i -> pure i
              Nothing -> Left (Forge.invalidInputFormat key input)
          _ -> Left (Forge.invalidInputFormat key input)
      EmailField _ ->
        case input of
          Forge.TextInput text :| [] ->
            case Email.validate (T.encodeUtf8 text) of
              Right i -> pure i
              Left {} -> Left (Forge.invalidInputFormat key input)
          _ -> Left (Forge.invalidInputFormat key input)
      MultiselectField _ choices -> do
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
      DropdownField _ choices -> do
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
        case listToMaybe (toList values) of
          Nothing -> Left (Forge.missingInputError key)
          Just a -> pure a
        where keyedChoices =
                map
                  (\(i, (value, title)) -> (uniqueKey i title, value))
                  (zip [0 :: Integer ..] (toList choices))
  viewField key minput =
    \case
      TextField mdef ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key)] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [minput <|> fmap (pure . Forge.TextInput) mdef]
           ])
      TextareaField mdef ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key)] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [minput <|> fmap (pure . Forge.TextInput) mdef]
           ])
      PhoneField mdef ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key), Lucid.type_ "tel"] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [minput <|> fmap (pure . Forge.TextInput) mdef]
           ])
      EmailField mdef ->
        Lucid.input_
          ([Lucid.name_ (Forge.unKey key), Lucid.type_ "email"] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [ minput <|>
                 fmap
                   (pure . Forge.TextInput)
                   (fmap (T.decodeUtf8 . Email.toByteString) mdef)
               ]
           ])
      IntegerField mdef ->
        Lucid.input_
          ([ Lucid.name_ (Forge.unKey key)
           , Lucid.type_ "text"
           , Lucid.pattern_ "-?[0-9]*"
           ] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [minput <|> fmap (pure . Forge.TextInput . T.pack . show) mdef]
           ])
      FixedField mdef ->
        Lucid.input_
          ([ Lucid.name_ (Forge.unKey key)
           , Lucid.type_ "text"
           , Lucid.pattern_ "-?[0-9.]*"
           ] <>
           [ Lucid.value_ value
           | Just (Forge.TextInput value :| []) <-
               [minput <|> fmap (pure . Forge.TextInput . T.pack . show) mdef]
           ])
      MultiselectField mdef choices ->
        Lucid.select_
          ([Lucid.name_ (Forge.unKey key), Lucid.multiple_ "multiple"])
          (mapM_
             (\(i, (a, label)) ->
                Lucid.option_
                  ([Lucid.value_ (uniqueKey i label)] <>
                   case minput of
                     Just inputs
                       | elem
                          (uniqueKey i label)
                          (mapMaybe
                             (\case
                                Forge.TextInput s -> pure s
                                _ -> Nothing)
                             (toList inputs)) -> [Lucid.selected_ "selected"]
                     _ ->
                       case mdef of
                         Just defaults
                           | elem a defaults -> [Lucid.selected_ "selected"]
                         _ -> [])
                  (Lucid.toHtml label))
             (zip [0 :: Integer ..] (toList choices)))
      DropdownField mdef choices ->
        Lucid.select_
          [Lucid.name_ (Forge.unKey key)]
          (mapM_
             (\(i, (a, label)) ->
                Lucid.option_
                  ([Lucid.value_ (uniqueKey i label)] <>
                   case minput of
                     Just inputs
                       | elem
                          (uniqueKey i label)
                          (mapMaybe
                             (\case
                                Forge.TextInput s -> pure s
                                _ -> Nothing)
                             (toList inputs)) -> [Lucid.selected_ "selected"]
                     _ ->
                       case mdef of
                         Just default'
                           | a == default' -> [Lucid.selected_ "selected"]
                         _ -> [])
                  (Lucid.toHtml label))
             (zip [0 :: Integer ..] (toList choices)))

-- | A key which is unique with respect to a list index and its display.
uniqueKey :: Integer -> Text -> Text
uniqueKey i title = fromString (show i) <> ":" <> title

-- | Read a fixed precision.
readFixed :: HasResolution p => String -> Maybe (Fixed p)
readFixed s = do
  fixed <- readMaybe s
  let (_, rhs) = span (/= '.') s
  guard
    (null rhs ||
     (all isDigit (drop 1 rhs) && (10 ^ length (drop 1 rhs)) <= resolution fixed))
  pure fixed
