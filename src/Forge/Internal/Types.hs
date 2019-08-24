{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Web forms interface.

module Forge.Internal.Types
  ( -- * Main form type
    -- $form-type
    Form(..)
    -- * Classes
  , FormError(..)
  , FormVerification(..)
  , FormField(..)
  -- * Field names
  -- $field-names
  , FieldName(..)
  , FieldNameStatus(..)
  -- * Generation
  -- $generated
  , Generated(..)
  -- * Inputs to a form
  , Input(..)
  , Key(..)
  , Path(..)
  ) where

import Data.String
import Data.Text (Text)
import Data.Validation

--------------------------------------------------------------------------------
-- $form-type
--
-- The main form type used in this package.
--

-- | A form indexed by some type @index@ (a phantom type), returning a
-- value @a@.
--
-- You can use 'fmap' (aka '<$>') over the value @a@ and use '<*>' to
-- combine forms together.
--
-- For example:
--
-- @
-- (<>) '<$>' 'ValueForm' (pure "Hello, ") '<*>' 'ValueForm' (pure "World!")
-- @
--
-- Produces @ValueForm (pure ("Hello, World!"))@.
data Form action view field error a where
  -- | Produce a value in the action of your index.
  ValueForm
    :: action a
    -> Form action view field error a
  -- | Map over the value. This mirrors 'fmap'.
  MapValueForm
    :: (a -> b)
    -> Form action view field error a
    -> Form action view field error b
  -- | Applicative application of a function to a value. Notice that
  -- this mirrors '<*>' or 'ap'.
  ApValueForm
    :: Form action view field error (a -> b)
    -> Form action view field error a
    -> Form action view field error b
  -- | Embed a view in a form, such as a label or some text.
  ViewForm
    :: action view
    -> Form action view field error ()
  -- | A terminal node in the form tree that represents a field; a
  -- producer of values (aside from 'ValueForm'), from user input.
  FieldForm
    :: FieldName index
    -> action (field a)
    -> Form action view field error a
  -- | Parse a form's result.
  ParseForm
    :: (x -> action (Either (error) a))
    -> Form action view field error x
    -> Form action view field error a
  -- | Transform a form's view using the error from above.
  FloorForm
    :: (Maybe error -> action view -> (action view, Maybe (error)))
    -> Form action view field error a
    -> Form action view field error a
  -- | Transform a form's view using errors from below.
  CeilingForm
    :: ([error] -> action view -> (action view, [error]))
    -> Form action view field error a
    -> Form action view field error a

instance Functor (Form action view field error) where
  fmap = MapValueForm

-- | Used to combine forms together.
instance Applicative action => Applicative (Form action view field error) where
  (<*>) = ApValueForm
  pure = ValueForm . pure

--------------------------------------------------------------------------------
-- Field names

-- | Name for a field. Either dynamic (more typical), or statically
-- determined (such as a username/password) for browser-based
-- autocomplete purposes.
data FieldName index where
  DynamicFieldName :: FieldName index
  StaticFieldName
    :: (NameStatus index ~ 'FieldNamesNeedVerification) => Text -> FieldName index

--------------------------------------------------------------------------------
-- $type-families
--
-- Type families used by the form type.

-- | The status of verification of invariants in the form.
class FormVerification index where
  type NameStatus index :: FieldNameStatus

-- | Whether field names need to be checked statically for extra
-- invariants.
data FieldNameStatus
  = FieldNamesNeedVerification
    -- ^ The form has to be checked before it can be run. This is what
    -- happens when you use custom field names.
  | FieldNamesOk
    -- ^ No check has to be performed on the form. This is the typical case.

-- | The type of field used in the form.
class FormField view field error where
  parseFieldInput :: FormError error => Key -> field a -> Input -> Either error a
  viewField :: Key -> field a -> view

-- | The error type of the form.
class FormError error where
   missingInputError :: Key -> error
   invalidInputFormat :: Key -> Input -> error

--------------------------------------------------------------------------------
-- $generated
--
-- Forms generate views and they generate results. They are coupled
-- together. This type represents that coupling.

-- | The generated view and value of a form.
data Generated action view error a =
  Generated
    { generatedView :: !(action view)
      -- ^ The view for the page to display.
    , generatedValue :: !(Validation [error] a)
      -- ^ Either a successful result, or else a list of (possibly
      -- empty) errors.
    }

-- | Map over the result value.
deriving instance Functor (Generated action view field)
deriving instance Traversable (Generated action view field)
deriving instance Foldable (Generated action view field)

-- | Combine the results.
instance (Monoid view, Applicative action) => Applicative (Generated action view field) where
  pure x = Generated {generatedView = pure mempty, generatedValue = pure x}
  (<*>) x y =
    Generated
      { generatedView = (<>) <$> generatedView x <*> generatedView y
      , generatedValue = generatedValue x <*> generatedValue y
      }

--------------------------------------------------------------------------------
-- Input types

-- | A form field key.
newtype Key =
  Key
    { unKey :: Text
    }
  deriving (Show, Eq, Ord, IsString)

-- | A path to a field.
data Path
  = PathBegin !Path
  | InMapValue !Path
  | InApLeft !Path
  | InApRight !Path
  | InParse !Path
  | PathEnd
  deriving (Show, Eq, Ord)

-- | An input from a form.
data Input
  = TextInput !Text
  | FileInput !FilePath
  deriving (Show, Eq, Ord)
