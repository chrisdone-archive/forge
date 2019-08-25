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

-- | Verifying invariants of a form.

module Forge.Verify
  ( verified
  , VerifiedForm
  , unVerifiedForm
  , verify
  , runVerification
  , maybeVerify
  ) where

import           Control.Monad.State.Strict
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Forge.Internal.Types
import           Language.Haskell.TH
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Lift ()
import           Language.Haskell.TH.Syntax hiding (lift)

-- | Verification result of a form.
data VerificationResult
  = VerifiedResult
  | DuplicateField !Text
  deriving (Eq, Show)

-- | A form whose invariants have been verified statically.
newtype VerifiedForm index parse view field error a =
  VerifiedForm
    { unVerifiedForm :: Form index parse view field error a
    }

-- | A form whose invariants are verified in the type system can be
-- immediately converted to a verified form.
verified ::
     Form 'Verified parse view field error a
  -> VerifiedForm 'Verified parse view field error a
verified = VerifiedForm

-- | Smart constructor.
maybeVerify ::
     Form index parse view field error a
  -> Maybe (VerifiedForm index parse view field error a)
maybeVerify frm =
  case runVerification frm of
    VerifiedResult -> Just (VerifiedForm frm)
    _ -> Nothing

-- | Verify a form that needs verification.
verify ::
     Q (TExp (Form index parse view field error a))
  -> Q (TExp (Q (TExp (VerifiedForm index parse view field error a))))
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
        ValueForm {} -> pure ()
        MapValueForm _ f -> go f
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
