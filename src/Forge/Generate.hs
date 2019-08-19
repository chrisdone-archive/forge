{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Generate a form.
--
-- Forms generate views and they generate results. They are coupled
-- together. This type represents that coupling.

module Forge.Generate
  ( generate
  , view
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Validation
import           Forge.Internal.Types hiding (Lift(..))

-- IDEA: rather than specifying field names in the form itself, do a
-- transform of the keys later? Custom names are inherently
-- non-compositional, but a rewrite for the purposes of browser
-- completion ain't bad. Or something else, as a separate pass, with
-- hints.

-- | Generate a form in the given action context.
generate ::
     forall index m a.
     ( m ~ Action index
     , Monad m
     , FormError index
     , Monoid (View index)
     , FormField index
     )
  => Map Key Input
  -- ^ The inputs to your form.
  -> Form index a
  -- ^ The description of your form.
  -> m (Generated index a)
  -- ^ The generated resut of the view and any value or errors.
generate inputs = go PathBegin
  where
    go :: forall x. (Path -> Path) -> Form index x -> m (Generated index x)
    go path =
      \case
        ValueForm m -> fmap pure m
        MapValueForm f form -> fmap (fmap f) (go (path . InMapValue) form)
        ApValueForm f x ->
          (<*>) <$> go (path . InApLeft) f <*> go (path . InApRight) x
        ViewForm m -> fmap pureView m
        FieldForm m -> do
          field <- m
          let key = pathToKey (path PathEnd)
              generatedView = viewField @index key field
          case M.lookup key inputs of
            Nothing ->
              pure
                (Generated
                   { generatedValue =
                       Failure (pure (missingInputError @index key))
                   , generatedView
                   })
            Just input ->
              case parseFieldInput @index key field input of
                Left errorIndexed ->
                  pure
                    (Generated
                       { generatedValue = Failure (pure errorIndexed)
                       , generatedView
                       })
                Right a ->
                  pure (Generated {generatedView, generatedValue = pure a})
        ParseForm f form -> do
          generated@Generated {generatedView} <- go (path . InParse) form
          case generatedValue generated of
            Success a -> do
              result <- f a
              case result of
                Left err ->
                  pure
                    (Generated {generatedView, generatedValue = Failure [err]})
                Right r ->
                  pure (Generated {generatedView, generatedValue = Success r})
            Failure errs ->
              pure (Generated {generatedView, generatedValue = Failure errs})
    pureView v = Generated {generatedView = v, generatedValue = pure ()}

-- | Convert a path to a rendered key.
pathToKey :: Path -> Key
pathToKey = Key . go
  where
    go =
      \case
        PathBegin path -> "/" <> go path
        InMapValue path -> "m/" <> go path
        InApLeft path -> "l/" <> go path
        InApRight path -> "r/" <> go path
        InParse path -> "p/" <> go path
        PathEnd -> ""

-- | View a form in the given action context.
view ::
     forall index f a.
     ( f ~ Action index
     , Monoid (View index)
     , Functor f
     , Applicative f
     , FormField index
     )
  => Form index a
  -- ^ The description of your form.
  -> f (View index)
  -- ^ The view of the form, no validations.
view = go PathBegin
  where
    go :: forall x. (Path -> Path) -> Form index x -> f (View index)
    go path =
      \case
        ValueForm _ -> pure mempty
        MapValueForm _ form -> go (path . InMapValue) form
        ApValueForm f x ->
          (<>) <$> go (path . InApLeft) f <*> go (path . InApRight) x
        ViewForm m -> m
        FieldForm m -> fmap (viewField @index (pathToKey (path PathEnd))) m
        ParseForm _ form -> go (path . InParse) form
