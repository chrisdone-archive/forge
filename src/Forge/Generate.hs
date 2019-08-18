{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Generate a form.
--
-- Forms generate views and they generate results. They are coupled
-- together. This type represents that coupling.

module Forge.Generate
  ( generate
  , Generated
  ) where

import Data.Bifunctor
import Forge.Internal.Types

-- | Generate a form in the given action context.
generate ::
     (m ~ Action index, Functor m, Applicative m)
  => Form index a
  -- ^ The description of your form.
  -> m (Generated index a)
  -- ^ The generated resut of the view and any value or errors.
generate =
  \case
    ValueForm m -> fmap pure m
    MapValueForm f form -> fmap (fmap f) (generate form)
    ApValueForm f x -> (<*>) <$> generate f <*> generate x
    ViewForm m -> fmap pureView m
  where
    pureView view = Generated {generatedView = view, generatedValue = pure ()}
