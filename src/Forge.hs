{-# LANGUAGE AllowAmbiguousTypes #-}
{-

FORMS important points:

1. Re-usability/composition.

2. View manipulation.

2. Error handling.

-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Forge where

import           Data.Bifunctor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Text.Read

data Generated index a =
  Generated
    { generatedView :: !(View index)
    , generatedValue :: !(Either [Error index] a)
    }
deriving instance Functor (Generated index)
instance Applicative (Generated index)

run ::
     (m ~ Action index, Functor m, Applicative m)
  => Form index a
  -> m (Generated index a)
run =
  \case
    PureValue m -> fmap pure m
    MapValue f form -> fmap (fmap f) (run form)
    ApValue f x -> (<*>) <$> run f <*> run x
    PureView m -> fmap pureView m
    -- MapError f form -> fmap (mapError f) (run form)
  where
    pureView view = Generated {generatedView = view, generatedValue = pure ()}
    mapView f gen = gen {generatedView = f (generatedView gen)}
    mapError f gen = gen {generatedValue = first (fmap f) (generatedValue gen)}

data Form index a where
  PureValue :: Action index a -> Form index a
  MapValue :: (a -> b) -> Form index a -> Form index b
  ApValue :: Form index (a -> b) -> Form index a -> Form index b
  -- Views
  PureView :: Action index (View index) -> Form index ()
  -- Fields
  PureField :: Action index (Field index a) -> Form index a
  -- Validation that bubbles errors up
  Validate
    :: (a -> Action index (Either (NonEmpty (Error index)) b))
    -> Form index a
    -> Form index b
  -- Validation that sends errors down
  WithErrors
    :: (Maybe (NonEmpty (Error index)) -> Form index a)
    -> Form index b
  -- Map over the view with the errors, possibly drawing from the
  -- errors, or allowing them to bubble up.
  MapViewErrors
    :: (View index -> [Error index] -> (View index, [Error index]))
    -> Form index a
    -> Form index a

instance Functor (Form i) where fmap = MapValue
instance (FormAction i, Monad (Action i)) => Applicative (Form i) where
  (<*>) = ApValue
  pure = PureValue . pure

class FormField index a where
  type Field index a

class FormAction index where
   type Action index :: * -> *

class FormView index where
   type View index
   renderField :: Field index a -> View index

class FormError index where
   type Error index

class FormLift from to where
  formLift :: Lift from to

-- | Use this for statically known names?
class FormNames index where
  type Names index

data Lift from to =
  Lift
    { liftView :: View from -> View to
    , liftError :: Error from -> Error to
    , liftAction :: forall a. Action from a -> Action to a
    }

liftWith ::
     forall from to mfrom mto a.
     (mfrom ~ Action from, Functor mfrom, mto ~ Action to, Functor mto)
  => Lift from to
  -> Form from a
  -> Form to a
liftWith Lift {..} = go
  where
    go =
      \case
        PureView m -> PureView (liftAction (fmap liftView m))

liftForm ::
     forall from to mfrom mto a.
     ( FormLift from to
     , mfrom ~ Action from
     , Functor mfrom
     , mto ~ Action to
     , Functor mto
     )
  => Form from a
  -> Form to a
liftForm = liftWith formLift

data HtmlT (m :: * -> *) a
data AppField a where TextField :: AppField String

renderAppField :: AppField a -> HtmlT m ()
renderAppField = undefined

data App
data MyError = GeneralError String
instance FormView App where
  type View App = HtmlT IO ()
  renderField = renderAppField
instance FormError App where type Error App = MyError
instance FormAction App where type Action App = IO
instance FormField App ty where type Field App ty = AppField ty

data WiderApp
data WiderError = MyError MyError
instance FormView WiderApp where
  type View WiderApp = HtmlT IO ()
  renderField = renderAppField
instance FormField WiderApp ty where type Field WiderApp ty = AppField ty
instance FormError WiderApp where type Error WiderApp = WiderError
instance FormAction WiderApp where type Action WiderApp = IO
instance FormLift App WiderApp where
  formLift = Lift {liftError = MyError, liftView = id, liftAction = id}

-- demo :: Form WiderApp Int
-- demo = MapView (\x -> x) $ liftForm demoinner

demoinner :: Form App Int
demoinner =
  Validate
    (pure . first (pure . GeneralError) . readEither)
    (PureField (pure TextField))
