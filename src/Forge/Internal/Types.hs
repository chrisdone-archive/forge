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
  , Default
  , unsafeUndefault
  , noDefault
  , FieldRequired(..)
  , RequiredT(..)
  , FieldResult
  , maybeDefault
  , submittedToDefault
  , Submitted
  , Reflected
  , unsafeUnreflect
  , reflect
  ) where

import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))
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
    :: Reflected a -- ^ A pure value to produce.
    -> Form index parse view field error a
  -- | Map over the value. This mirrors 'fmap'.
  MapValueForm
    :: (a -> b) -- ^ Function to map over the value in form.
    -> Form index parse view field error a -- ^ Form to transform.
    -> Form index parse view field error b
  -- | Map over the error type.
  MapErrorForm
    :: (FormError errorA, FormField view field errorA)
    => (errorA -> errorB) -- ^ Map over the error.
    -> Form index parse view field errorA a -- ^ Original form.
    -> Form index parse view field errorB a -- ^ Form with new error type.
  -- | Applicative application of a function to a value. Notice that
  -- this mirrors '<*>' or 'Control.Monad.ap'.
  ApValueForm
    :: Form index parse view field error (a -> b) -- ^ Function-producing form.
    -> Form index parse view field error a -- ^ Argument form.
    -> Form index parse view field error b
  -- | Embed a view in a form, such as a label or some text.
  ViewForm
    :: Reflected view -- ^ A view (e.g. html) to embed.
    -> Form index parse view field error () -- ^ A form that just displays that view.
  -- | A terminal node in the form tree that represents a field; a
  -- producer of values (aside from 'ValueForm'), from user input.
  FieldForm
    :: FieldName index -- ^ Name of the field.
    -> FieldRequired r -- ^ Is the field required?
    -> Default a -- ^ Default value for the field.
    -> field a -- ^ The field.
    -> Form index parse view field error (FieldResult r a) -- ^ Form representing that field.
  -- | Parse a form's result.
  ParseForm
    :: Reflected (x -> parse (Either error a)) -- ^ Run a parser in @parse@ and produce @error@ or the value.
    -> Form index parse view field error x -- ^ Form whose result we will parse.
    -> Form index parse view field error a
  -- | Transform a form's view using the error from above.
  FloorForm
    :: Reflected (Maybe error -> view -> (view, Maybe error))
    -- ^ Transforms view below using error, if any, from above.
    -> Form index parse view field error a
    -- ^ Form whose errors we are not interested in.
    -> Form index parse view field error a
  -- | Transform a form's view using errors from below.
  CeilingForm
    :: Reflected ([error] -> view -> (view, [error]))
    -- ^ Transform the errors coming from below.
    -> Form index parse view field error a
    -- ^ Form that bubbles errors up.
    -> Form index parse view field error a
  -- | Many formlets.
  ManyForm
    :: Reflected (view -> [view] -> view)
    -- ^ The set's view, the items' views, and produce a final view.
    -> Form index parse view field error [Integer]
    -- ^ The set.
    -> (Default a -> Form index parse view field error a)
    -- ^ An individual item formlet.
    -> [a]
    -- ^ Defaults.
    -> Form index parse view field error [a]
    -- ^ The final form.

instance (a ~ (), IsString view) =>
         IsString (Form index parse view field error a) where
  fromString = ViewForm . fromString

-- | Map over the value of the form.
instance Functor (Form index parse view field error) where
  fmap = MapValueForm

-- | Used to combine forms together.
instance Applicative (Form index parse view field error) where
  (<*>) = ApValueForm
  pure = ValueForm . pure

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

instance (index ~ 'Unverified) => IsString (FieldName index) where
  fromString = StaticFieldName . fromString

--------------------------------------------------------------------------------
-- Submitted data

-- TODO: apply this type to validations/views within the Form type.

-- TODO: if that works out, then add a:
--  BindForm :: (Submitted (Maybe a) -> Form index parse view field error b)
--                      ^ note the Maybe a, Nothing=not-submitted
--           -> Form index parse view field error b

-- | Data which has been submitted by a form submission and may not be available yet.
newtype Submitted a = Submitted
  { unSubmitted :: Maybe a
  } deriving (Functor, Show, Applicative, Semigroup, Monoid)

submittedToDefault :: Submitted a -> Default a
submittedToDefault (Submitted a) = Default a

-- | Data which can come from the form itself.
newtype Reflected a = Reflected
  { unReflected :: Identity a
  } deriving (Functor, Show, Applicative, Semigroup, Monoid, IsString)

-- | For internal use.
unsafeUnreflect :: Reflected a -> a
unsafeUnreflect (Reflected (Identity a)) = a

-- | Reflect the submitted data to be available right now, if submitted.
reflect :: Submitted a -> Maybe (Reflected a)
reflect = fmap (Reflected . Identity) . unSubmitted

--------------------------------------------------------------------------------
-- Defaults

newtype Default a = Default { unsafeUndefault :: Maybe a}
  deriving (Functor, Show, Applicative, Semigroup, Monoid)

maybeDefault :: Maybe a -> Default a
maybeDefault = Default

noDefault :: Default a
noDefault = Default Nothing

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
  parseFieldInput ::
       FormError error
    => Key
    -> FieldRequired r
    -> NonEmpty Input
    -> field a
    -> Either error (FieldResult r a)
  viewField ::
       Key
    -> FieldRequired r
    -> Maybe a
    -> Maybe (NonEmpty Input)
    -> field a
    -> view

-- | The error type of the form.
class FormError error where
   missingInputError :: Key -> error
   invalidInputFormat :: Key -> NonEmpty Input -> error

data RequiredT = RequiredT | OptionalT

data FieldRequired a where
  RequiredField :: FieldRequired 'RequiredT
  OptionalField :: FieldRequired 'OptionalT
deriving instance Show (FieldRequired a)

type family FieldResult r a where
  FieldResult 'RequiredT a = a
  FieldResult 'OptionalT a = Maybe a

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
-- | Structural show.
deriving instance (Show view, Show field, Show a) => Show (Generated view field a)
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
  | InManySet !Path
  | InManyIndex !Integer !Path
  | PathEnd
  deriving (Show, Eq, Ord)

-- | An input from a form.
data Input
  = TextInput !Text
  | FileInput !FilePath
  deriving (Show, Eq, Ord)
