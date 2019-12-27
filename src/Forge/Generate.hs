{-# LANGUAGE TupleSections #-}
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
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.String
import           Data.Validation
import           Forge.Internal.Types
import           Forge.Verify

-- | Generate a form in the given parse context.
generate ::
     forall index parse view field error a.
     (Monad parse, Monoid view, FormField view field error, FormError error)
  => Map Key (NonEmpty Input)
  -- ^ The inputs to your form.
  -> VerifiedForm index parse view field error a
  -- ^ The description of your form.
  -> parse (Generated view error a)
  -- ^ The generated resut of the view and any value or errors.
generate inputs = go PathBegin . unVerifiedForm
  where
    go ::
         forall x err. (FormError err, FormField view field err)
      => (Path -> Path)
      -> Form index parse view field err x
      -> parse (Generated view err x)
    go path =
      \case
        ValueForm m -> pure (pure m)
        MapValueForm f form -> fmap (fmap f) (go (path . InMapValue) form)
        MapErrorForm f form ->
          fmap
            (\gen ->
               Generated
                 { generatedView = generatedView gen
                 , generatedValue = first (fmap f) (generatedValue gen)
                 })
            (go (path . InMapError) form)
        ApValueForm f x ->
          (<*>) <$> go (path . InApLeft) f <*> go (path . InApRight) x
        ViewForm m -> pure (pureView m)
        FieldForm name m -> do
          field <- pure m
          let key =
                case name of
                  DynamicFieldName -> pathToKey (path PathEnd)
                  StaticFieldName text -> Key text
              minput = M.lookup key inputs
              generatedView = viewField @view @field @err key minput field
          case minput of
            Nothing ->
              pure
                (Generated
                   { generatedValue = Failure (pure (missingInputError key))
                   , generatedView
                   })
            Just input ->
              case parseFieldInput @view @field @err key field input of
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
                           viewWithError inputs (Just err) (path . InParse) form
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
        ManyForm viewTransformer setForm itemForm defaults -> do
          setGenerated <- go (path . InManySet) setForm
          case setGenerated of
            Generated {generatedValue = Success set, generatedView = setView} -> do
              generateds <-
                fmap
                  M.fromList
                  (traverse
                     (\idx ->
                        fmap
                          (idx, )
                          (go
                             (path . InManyIndex idx)
                             (itemForm (M.lookup idx defaults))))
                     (Set.toList set))
              let totalGenerated = sequenceA generateds
              pure
                Generated
                  { generatedValue = generatedValue totalGenerated
                  , generatedView =
                      viewTransformer setView (map generatedView (M.elems generateds))
                  }
            Generated {generatedValue = Failure err} ->
              pure
                setGenerated
                  { generatedValue = Failure err
                  , generatedView =
                      viewTransformer (generatedView setGenerated) []
                  }
    pureView :: forall e. view -> Generated view e ()
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
        InMapError path -> "e/" <> go path
        InManySet path -> "s/" <> go path
        InManyIndex idx path -> "i/" <> fromString (show idx) <> "/" <> go path
        PathEnd -> ""

-- | View a form in the given parse context.
view ::
     forall index parse view field error a. (Monoid view, FormField view field error)
  => VerifiedForm index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
view = viewWithError mempty Nothing PathBegin . unVerifiedForm

-- | View the form with the given error from above.
viewWithError ::
     forall index parse view field error a.
     (Monoid view, FormField view field error)
  => Map Key (NonEmpty Input)
  -- ^ The inputs to your form.
  -> Maybe error
  -- ^ Errors generated by an above validation.
  -> (Path -> Path)
  -- ^ Starting path.
  -> Form index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
viewWithError inputs = go
  where
    go ::
         forall x err.
         Maybe err
      -> (Path -> Path)
      -> Form index parse view field err x
      -> view
    go errs path =
      \case
        ValueForm _ -> mempty
        -- When we hit a map over the error, that means what is below
        -- cannot by the type system even access what's
        -- above. Therefore this forms a lower boundary.
        ManyForm viewTransformer setForm itemForm defaults ->
          viewTransformer
            (go errs (path . InManySet) setForm)
            (map
               (\(i, a) -> go errs (path . InManyIndex i) (itemForm (pure a)))
               (M.toList defaults))
        MapErrorForm _ form -> go Nothing (path . InMapError) form
        MapValueForm _ form -> go errs (path . InMapValue) form
        CeilingForm f form -> fst (f [] (go errs (path . InCeiling) form))
        ApValueForm f x ->
          go errs (path . InApLeft) f <> go errs (path . InApRight) x
        ViewForm m -> m
        FieldForm name m ->
          viewField @view @field @error key (M.lookup key inputs) m
          where key =
                  case name of
                    DynamicFieldName -> pathToKey (path PathEnd)
                    StaticFieldName text -> Key text
        ParseForm _ form -> go errs (path . InParse) form
        FloorForm f form ->
          let (view', errs') = f errs (go errs' (path . InFloor) form)
           in view'
