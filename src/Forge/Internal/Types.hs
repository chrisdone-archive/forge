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
    -- * Type families used
    -- $type-families
  , FormField(..)
  , FormAction(..)
  , FormView(..)
  , FormError(..)
  -- * Lifting forms
  -- $lifting
  , FormLift(..)
  , Lift(..)
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

--------------------------------------------------------------------------------
-- $form-type
--
-- The main form type used in this package.
--
-- For your application, your form will have a bunch of types, but
-- they tend to never change. The field type, the view type, the monad
-- in which they are run, etc. Hence, we have a type @index@, and then
-- use various type families for instantiating types.
--
-- The @a@ type changes all the type within the same function
-- definition, so that is a regular type parameter.

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
data Form index a where
  -- | Produce a value in the action of your index.
  ValueForm :: Action index a -> Form index a
  -- | Map over the value. This mirrors 'fmap'.
  MapValueForm :: (a -> b) -> Form index a -> Form index b
  -- | Applicative application of a function to a value. Notice that
  -- this mirrors '<*>' or 'ap'.
  ApValueForm :: Form index (a -> b) -> Form index a -> Form index b
  -- | Embed a view in a form, such as a label or some text.
  ViewForm :: Action index (View index) -> Form index ()
  -- | A terminal node in the form tree that represents a field; a
  -- producer of values (aside from 'ValueForm'), from user input.
  FieldForm :: Action index (Field index a) -> Form index a

-- | A regular functor over the @a@ in @Form index a@.
instance Functor (Form i) where
  fmap = MapValueForm

-- | Used to combine forms together.
instance Applicative (Action i) => Applicative (Form i) where
  (<*>) = ApValueForm
  pure = ValueForm . pure

--------------------------------------------------------------------------------
-- $type-families
--
-- Type families used by the form type.

-- | The view generated by the form (typically HTML).
--
-- An example definition:
--
-- @
-- data App
-- instance FormView App where
--   type View App = Html
-- @
--
-- The 'Html' type is provided by e.g. the lucid package or
-- blaze-html.
class FormView index where
  type View index

-- | An action (typically monadic, but can just be 'Identity') which
-- is used in producing the form tree and also in validating the form.
--
-- An example definition:
--
-- @
-- import Data.Functor.Identity
-- data App
-- instance FormAction App where
--   type Action App = Identity
-- @
--
-- Using 'Identity' is a way to say that your form is pure and doesn't
-- need any side-effects.
class FormAction index where
   type Action index :: * -> *

-- | The type of field used in the form.
--
-- An example definition:
--
-- @
-- data App
-- instance FormField App where
--   type Field App = HtmlInput
-- @
--
-- Where the 'HtmlInput' type comes from the html-input package.
class FormField index where
  type Field index :: * -> *
  parseFieldInput :: Key -> Field index a -> Input -> Either (Error index) a
  viewField :: Key -> Field index a -> View index

-- | The error type of the form.
--
-- @
-- data App
-- data MyError = MissingField !Key
-- instance FormError App where
--   type Error App = MyError
--   missingFieldError = MissingField
-- @
class FormError index where
   type Error index
   missingInputError :: Key -> Error index
   invalidInputFormat :: Key -> Input -> Error index

--------------------------------------------------------------------------------
-- $lifting
--
-- When you want to change the index of a form to include it within
-- another, you can implement 'formLift' to convert from one to the
-- other.

-- | Lift one form into another.
class FormLift from to where
  formLift :: Lift from to

-- | A lift converts from one form to another.
data Lift from to =
  Lift
    { liftView :: View from -> View to
    , liftError :: Error from -> Error to
    , liftAction :: forall a. Action from a -> Action to a
    }

--------------------------------------------------------------------------------
-- $generated
--
-- Forms generate views and they generate results. They are coupled
-- together. This type represents that coupling.

-- | The generated view and value of a form.
data Generated index a =
  Generated
    { generatedView :: !(View index)
      -- ^ The view for the page to display.
    , generatedValue :: !(Either [Error index] a)
      -- ^ Either a successful result, or else a list of (possibly
      -- empty) errors.
    }

-- | Map over the result value.
deriving instance Functor (Generated index)
deriving instance Traversable (Generated index)
deriving instance Foldable (Generated index)

-- | Combine the results.
instance Applicative (Generated index)

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
  | PathEnd
  deriving (Show, Eq, Ord)

-- | An input from a form.
data Input
  = TextInput !Text
  | FileInput !FilePath
  deriving (Show, Eq, Ord)
