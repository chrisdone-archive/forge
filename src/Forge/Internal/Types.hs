{-# LANGUAGE FunctionalDependencies #-}
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
  , FormIndex(..)
  , FormField(..)
  -- * Field names
  -- $field-names
  , FieldName(..)
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
data Form index (parse :: * -> *) view (field :: * -> *) error a where
  -- | Produce a pure value.
  ValueForm
    :: a
    -> Form index parse view field error a
  -- | Map over the value. This mirrors 'fmap'.
  MapValueForm
    :: (a -> b)
    -> Form index parse view field error a
    -> Form index parse view field error b
  -- | Map over the error type.
  MapErrorForm
    :: (FormError errorA, FormField view field errorA)
    => (errorA -> errorB)
    -> Form index parse view field errorA a
    -> Form index parse view field errorB a
  -- | Applicative application of a function to a value. Notice that
  -- this mirrors '<*>' or 'Control.Monad.ap'.
  ApValueForm
    :: Form index parse view field error (a -> b)
    -> Form index parse view field error a
    -> Form index parse view field error b
  -- | Embed a view in a form, such as a label or some text.
  ViewForm
    :: view
    -> Form index parse view field error ()
  -- | A terminal node in the form tree that represents a field; a
  -- producer of values (aside from 'ValueForm'), from user input.
  FieldForm
    :: FieldName index
    -> field a
    -> Form index parse view field error a
  -- | Parse a form's result.
  ParseForm
    :: (x -> parse (Either error a))
    -> Form index parse view field error x
    -> Form index parse view field error a
  -- | Transform a form's view using the error from above.
  FloorForm
    :: (Maybe error -> view -> (view, Maybe error))
    -> Form index parse view field error a
    -> Form index parse view field error a
  -- | Transform a form's view using errors from below.
  CeilingForm
    :: ([error] -> view -> (view, [error]))
    -> Form index parse view field error a
    -> Form index parse view field error a

-- | Map over the value of the form.
instance Functor (Form index parse view field error) where
  fmap = MapValueForm

-- | Used to combine forms together.
instance Applicative (Form index parse view field error) where
  (<*>) = ApValueForm
  pure = ValueForm

--------------------------------------------------------------------------------
-- Field names

-- | Name for a field.
--
-- Either dynamic (more typical), or statically
-- determined (such as a username/password) for browser-based
-- autocomplete purposes.
data FieldName index where
  DynamicFieldName :: FieldName index
  StaticFieldName :: Text -> FieldName 'Unverified

--------------------------------------------------------------------------------
-- $type-classes
--
-- Type classes used by the form type.

-- | Index of the form AST.
data FormIndex
  = Verified
  | Unverified

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
data Generated view error a =
  Generated
    { generatedView :: !view
      -- ^ The view for the page to display.
    , generatedValue :: !(Validation [error] a)
      -- ^ Either a successful result, or else a list of (possibly
      -- empty) errors.
    }

-- | Structural equality.
deriving instance (Eq view, Eq field, Eq a) => Eq (Generated view field a)
-- | Map over the result value.
deriving instance Functor (Generated view field)
-- | Traverse over the value.
deriving instance Traversable (Generated view field)
-- | Fold over the value.
deriving instance Foldable (Generated view field)

-- | Combine the results.
instance (Monoid view) => Applicative (Generated view field) where
  pure x = Generated {generatedView = mempty, generatedValue = pure x}
  (<*>) x y =
    Generated
      { generatedView = generatedView x <> generatedView y
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
  | InMapError !Path
  | InApLeft !Path
  | InApRight !Path
  | InParse !Path
  | InCeiling !Path
  | InFloor !Path
  | PathEnd
  deriving (Show, Eq, Ord)

-- | An input from a form.
data Input
  = TextInput !Text
  | FileInput !FilePath
  deriving (Show, Eq, Ord)
