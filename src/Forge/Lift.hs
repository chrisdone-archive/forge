{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | Lifting between forms.

module Forge.Lift
  ( liftForm
  , liftWith
  ) where

import Data.Bifunctor
import Forge.Internal.Types

-- | Lift with the lifter from the 'FormLift' class.
liftForm ::
     forall from to mfrom mto a.
     ( FormLift from to
     , mfrom ~ Action from
     , Functor mfrom
     , mto ~ Action to
     , Functor mto
     )
  => Form from a
     -- ^ Child form type.
  -> Form to a
     -- ^ Parent form type, which the child is embedded in.
liftForm = liftWith formLift

-- | Lift with a custom 'Lift'.
liftWith ::
     forall from to mfrom mto a.
     (mfrom ~ Action from, Functor mfrom, mto ~ Action to, Functor mto)
  => Lift from to
  -- ^ Custom lifter.
  -> Form from a
  -- ^ Child form type.
  -> Form to a
  -- ^ Parent form type, which the child is embedded in.
liftWith lift@Lift {..} = go
  where
    go =
      \case
        ViewForm m -> ViewForm (liftAction (fmap liftView m))
        ValueForm m -> ValueForm (liftAction m)
        MapValueForm f m -> MapValueForm f (liftWith lift m)
        ApValueForm f x -> ApValueForm (liftWith lift f) (liftWith lift x)
        FieldForm m -> FieldForm (liftAction (fmap liftField m))
        ParseForm f m ->
          ParseForm (liftAction . fmap (first liftError) . f) (liftWith lift m)
