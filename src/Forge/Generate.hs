{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
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

import           Data.Bifunctor
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Validation
import           Forge.Internal.Types
import           Forge.Verify

-- | Generate a form in the given parse context.
generate ::
     forall index parse view field error a.
     (Monad parse, Monoid view, FormField view field error, FormError error)
  => Map Key Input
  -- ^ The inputs to your form.
  -> VerifiedForm index parse view field error a
  -- ^ The description of your form.
  -> parse (Generated view error a)
  -- ^ The generated resut of the view and any value or errors.
generate inputs = go PathBegin . unVerifiedForm
  where
    go ::
         forall x.
         (Path -> Path)
      -> Form index parse view field error x
      -> parse (Generated view error x)
    go path =
      \case
        ValueForm m -> pure (pure m)
        MapValueForm f form -> fmap (fmap f) (go (path . InMapValue) form)
        ApValueForm f x ->
          (<*>) <$> go (path . InApLeft) f <*> go (path . InApRight) x
        ViewForm m -> pure (pureView m)
        FieldForm name m -> do
          field <- pure m
          let key =
                case name of
                  DynamicFieldName -> pathToKey (path PathEnd)
                  StaticFieldName text -> Key text
              generatedView = viewField @view @field @error key field
          case M.lookup key inputs of
            Nothing ->
              pure
                (Generated
                   { generatedValue = Failure (pure (missingInputError key))
                   , generatedView
                   })
            Just input ->
              case parseFieldInput @view @field @error key field input of
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
                    (Generated
                       { generatedView =
                           viewWithError (Just err) (path . InParse) form
                       , generatedValue = Failure [err]
                       })
                Right r ->
                  pure (Generated {generatedView, generatedValue = Success r})
            Failure errs -> do
              pure (Generated {generatedView, generatedValue = Failure errs})
        FloorForm _ form -> go (path . InFloor) form
        CeilingForm f form -> do
          generated@Generated {generatedView} <- go (path . InCeiling) form
          let (generatedView', errs') =
                f
                  (validation id (const []) (generatedValue generated))
                  generatedView
          pure
            (Generated
               { generatedView = generatedView'
               , generatedValue = first (const errs') (generatedValue generated)
               })
    pureView :: view -> Generated view error ()
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
        InCeiling path -> "c/" <> go path
        InFloor path -> "f/" <> go path
        PathEnd -> ""

-- | View a form in the given parse context.
view ::
     forall index parse view field error a. (Monoid view, FormField view field error)
  => VerifiedForm index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
view = viewWithError Nothing PathBegin . unVerifiedForm

-- | View the form with the given error from above.
viewWithError ::
     forall index parse view field error a.
     (Monoid view, FormField view field error)
  => Maybe error
  -- ^ Errors generated by an above validation.
  -> (Path -> Path)
  -- ^ Starting path.
  -> Form index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
viewWithError = go
  where
    go ::
         forall x.
         Maybe error
      -> (Path -> Path)
      -> Form index parse view field error x
      -> view
    go errs path =
      \case
        ValueForm _ -> mempty
        MapValueForm _ form -> go errs (path . InMapValue) form
        CeilingForm _ form -> go errs (path . InCeiling) form
        ApValueForm f x ->
          go errs (path . InApLeft) f <> go errs (path . InApRight) x
        ViewForm m -> m
        FieldForm name m -> viewField @view @field @error key m
          where key =
                  case name of
                    DynamicFieldName -> pathToKey (path PathEnd)
                    StaticFieldName text -> Key text
        ParseForm _ form -> go errs (path . InParse) form
        FloorForm f form ->
          let (view', errs') = f errs (go errs' (path . InFloor) form)
           in view'
