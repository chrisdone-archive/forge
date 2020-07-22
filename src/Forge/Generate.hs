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
generate inputs = go Nothing PathBegin . unVerifiedForm
  where
    go ::
         forall x err. (FormError err, FormField view field err)
      => Maybe err
      -> (Path -> Path)
      -> Form index parse view field err x
      -> parse (Generated view err x)
    go merrorFromAbove path =
      \case
        BindForm final m f -> do
          mresult@Generated {generatedValue = value} <-
            go merrorFromAbove (path . InBindLhs) m
          case value of
            Failure {} -> do
              fresult <- go merrorFromAbove (path . InBindRhs) (f notSubmitted)
              pure (final <$> mresult <*> fresult)
            Success v -> do
              fresult <- go merrorFromAbove (path . InBindRhs) (f (pure v))
              pure (final <$> mresult <*> fresult)
        ValueForm m -> pure (pure (unsafeUnreflect m))
        MapValueForm f form ->
          fmap (fmap f) (go merrorFromAbove (path . InMapValue) form)
        MapErrorForm f form ->
          fmap
            (\gen ->
               Generated
                 { generatedView = generatedView gen
                 , generatedValue = first (fmap f) (generatedValue gen)
                 })
            (go Nothing (path . InMapError) form)
        ApValueForm f x ->
          (<*>) <$> go merrorFromAbove (path . InApLeft) f <*>
          go merrorFromAbove (path . InApRight) x
        ViewForm m -> pure (pureView (unsafeUnreflect m))
        FieldForm name required def m -> do
          field <- pure m
          let key =
                case name of
                  DynamicFieldName -> pathToKey (path PathEnd)
                  StaticFieldName text -> Key text
              minput = M.lookup key inputs
              generatedView =
                viewField
                  @view
                  @field
                  @err
                  key
                  required
                  (unsafeUndefault def)
                  minput
                  field
          case minput of
            Nothing ->
              pure
                (Generated
                   { generatedValue = Failure (pure (missingInputError key))
                   , generatedView
                   })
            Just input ->
              case parseFieldInput @view @field @err key required input field of
                Left errorIndexed ->
                  pure
                    (Generated
                       { generatedValue = Failure (pure errorIndexed)
                       , generatedView
                       })
                Right a ->
                  pure (Generated {generatedView, generatedValue = pure a})
        ParseForm parser form -> do
          generated@Generated {generatedView} <-
            go merrorFromAbove (path . InParse) form
          case generatedValue generated of
            Success a -> do
              result <- (unsafeUnreflect parser) a
              case result of
                Left err -> do
                  Generated {generatedView = generatedView'} <-
                    go (pure err) (path . InParse) form
                  pure
                    (Generated
                       { generatedView = generatedView'
                       , generatedValue = Failure [err]
                       })
                Right r ->
                  pure (Generated {generatedView, generatedValue = Success r})
            Failure errs -> do
              pure (Generated {generatedView, generatedValue = Failure errs})
        FloorForm errorTransform viewTransform form -> do
          let merrorFromAbove' =
                (unsafeUnreflect errorTransform) merrorFromAbove
          generated@Generated {generatedView} <-
            go merrorFromAbove' (path . InFloor) form
          let generatedView' =
                (unsafeUnreflect viewTransform) merrorFromAbove generatedView
          pure generated {generatedView = generatedView'}
        CeilingForm f form -> do
          generated@Generated {generatedView} <-
            go merrorFromAbove (path . InCeiling) form
          let (generatedView', errs') =
                (unsafeUnreflect f)
                  (validation id (const []) (generatedValue generated))
                  generatedView
          pure
            (Generated
               { generatedView = generatedView'
               , generatedValue = first (const errs') (generatedValue generated)
               })
        ManyForm viewTransformer setForm itemForm _defaults -> do
          setGenerated <- go merrorFromAbove (path . InManySet) setForm
          case setGenerated of
            Generated {generatedValue = Success set, generatedView = setView} -> do
              generateds <-
                traverse
                  (\idx ->
                     go
                       merrorFromAbove
                       (path . InManyIndex idx)
                       (itemForm noDefault))
                  set
              let totalGenerated = sequenceA generateds
              pure
                Generated
                  { generatedValue = generatedValue totalGenerated
                  , generatedView =
                      (unsafeUnreflect viewTransformer)
                        setView
                        (map generatedView generateds)
                  }
            Generated {generatedValue = Failure err} ->
              pure
                setGenerated
                  { generatedValue = Failure err
                  , generatedView =
                      (unsafeUnreflect viewTransformer)
                        (generatedView setGenerated)
                        []
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
        InBindLhs path -> "blhs/" <> go path
        InBindRhs path -> "brhs/" <> go path
        PathEnd -> ""

-- | View a form in the given parse context.
view ::
     forall index parse view field error a. (Monoid view, FormField view field error)
  => VerifiedForm index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
view = viewInternal mempty PathBegin . unVerifiedForm

-- | View the form with the given error from above.
viewInternal ::
     forall index parse view field error a.
     (Monoid view, FormField view field error)
  => Map Key (NonEmpty Input)
  -- ^ The inputs to your form.
  -> (Path -> Path)
  -- ^ Starting path.
  -> Form index parse view field error a
  -- ^ The description of your form.
  -> view
  -- ^ The view of the form, no validations.
viewInternal inputs = go
  where
    go ::
         forall x err.
         (Path -> Path)
      -> Form index parse view field err x
      -> view
    go path =
      \case
        BindForm _ m f ->
          go (path . InBindLhs) m <> go (path . InBindRhs) (f notSubmitted)
        ValueForm _ -> mempty
        -- When we hit a map over the error, that means what is below
        -- cannot by the type system even access what's
        -- above. Therefore this forms a lower boundary.
        ManyForm viewTransformer setForm itemForm defaults ->
          (unsafeUnreflect viewTransformer)
            (go (path . InManySet) setForm)
            (map
               (\(i, a) -> go (path . InManyIndex i) (itemForm (pure a)))
               (zip [1 ..] defaults))
        MapErrorForm _ form -> go (path . InMapError) form
        MapValueForm _ form -> go (path . InMapValue) form
        CeilingForm f form ->
          fst ((unsafeUnreflect f) [] (go (path . InCeiling) form))
        ApValueForm f x -> go (path . InApLeft) f <> go (path . InApRight) x
        ViewForm m -> unsafeUnreflect m
        FieldForm name required def m ->
          viewField
            @view
            @field
            @error
            key
            required
            (unsafeUndefault def)
            (M.lookup key inputs)
            m
          where key =
                  case name of
                    DynamicFieldName -> pathToKey (path PathEnd)
                    StaticFieldName text -> Key text
        ParseForm _ form -> go (path . InParse) form
        FloorForm _ _ form -> go (path . InFloor) form
