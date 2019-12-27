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

-- | Easy functions for verifying invariants of a form. See
-- 'VerificationResult' for the kind of checks that are run.

module Forge.Verify
  ( -- * Static Verification
    -- $static-verification
    verified
  , verify
    -- * Dynamic verification
    -- $dynamic-verification
  , maybeVerify
  , VerifiedForm
   -- * Unwrapping verification
  , unVerifiedForm
  -- * Running the verifier (for test suites)
  , runVerification
  , VerificationResult(..)
  ) where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Forge.Internal.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Lift ()
import           Language.Haskell.TH.Syntax hiding (lift)

-- $static-verification
-- Verify invariants about the form at compile-time. Choose this method preferably.

-- $dynamic-verification
-- This can be helpful if your form is generated from runtime inputs.

-- | Verification result of a form.
data VerificationResult
  = VerifiedResult
  | DuplicateField !Text
  deriving (Eq, Show)

-- | A form whose invariants have been verified statically.
newtype VerifiedForm index parse view field error a =
  VerifiedForm
    { unVerifiedForm :: Form index parse view field error a
      -- ^ Unwrap a form from the proof that it was verified.
    }

-- | A form whose invariants are verified in the type system can be
-- immediately converted to a verified form.
verified ::
     Form 'Verified parse view field error a
     -- ^ A form which is already verified carries the proof.
  -> VerifiedForm 'Verified parse view field error a
  -- ^ A wrapped version of the form.
verified = VerifiedForm

-- | Smart constructor: verify the form at runtime.
maybeVerify ::
     Form index parse view field error a
     -- ^ A form which may be verifiable.
  -> Maybe (VerifiedForm index parse view field error a)
  -- ^ Maybe the verified form, or else nothing.
maybeVerify frm =
  case runVerification frm of
    VerifiedResult -> Just (VerifiedForm frm)
    _ -> Nothing

-- | Verify a form that needs verification at compile-time. Example:
--
-- @
-- '$$'($$(verify [|| myform ||]))
-- @
verify ::
     Q (TExp (Form index parse view field error a))
     -- ^ A typed quoted expression representing the form, e.g.
     --
     -- @
     -- [|| myform ||]
     -- @
  -> Q (TExp (Q (TExp (VerifiedForm index parse view field error a))))
  -- ^ Two layers of expression generators. Unpack it via
  --
  -- @
  -- '$$'($$(..))
  -- @
verify q = do
  TExp expr <- q
  [|| case runVerification $$(q) of
        VerifiedResult -> TExp <$> (appE (conE name) (pure expr))
        DuplicateField field -> error ("Duplicate field: " <> T.unpack field)
   ||]
  where name = 'VerifiedForm

-- | Run verification on the form.
runVerification :: Form index parse view field error a -> VerificationResult
runVerification =
  either DuplicateField (const VerifiedResult) . flip evalStateT Set.empty . go
  where
    go ::
         Form index parse view field error x
      -> StateT (Set Text) (Either Text) ()
    go =
      \case
        ManyForm _ f1 f2 defs ->
          go f1 *> void (traverse go (map (f2 . pure) (M.elems defs)))
        ValueForm {} -> pure ()
        MapValueForm _ f -> go f
        MapErrorForm _ f -> go f
        FloorForm _ f -> go f
        CeilingForm _ f -> go f
        ApValueForm f x -> go f >> go x
        ViewForm {} -> pure ()
        ParseForm _ f -> go f
        FieldForm name _ -> do
          seen <- get
          case name of
            StaticFieldName text ->
              if Set.member text seen
                then lift (Left text)
                else modify (Set.insert text)
            _ -> pure ()
