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
  , FormVerification(..)
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
  = Verified
  | DuplicateField !Text
  deriving (Eq, Show)

-- | A form whose invariants have been verified statically.
newtype VerifiedForm index a =
  VerifiedForm
    { unVerifiedForm :: Form index a
    }

-- | A form whose invariants are verified in the type system can be
-- immediately converted to a verified form.
verified ::
     (NameStatus index ~ 'FieldNamesOk) => Form index a -> VerifiedForm index a
verified = VerifiedForm

-- | Smart constructor.
maybeVerify :: Form index a -> Maybe (VerifiedForm index a)
maybeVerify frm =
  case runVerification frm of
    Verified -> Just (VerifiedForm frm)
    _ -> Nothing

-- | Verify a form that needs verification.
verify :: Q (TExp (Form index a)) -> Q (TExp (Q (TExp (VerifiedForm index a))))
verify q = do
  TExp expr <- q
  [|| case runVerification $$(q) of
        Verified -> TExp <$> (appE (conE name) (pure expr))
        DuplicateField field -> error ("Duplicate field: " <> T.unpack field)
   ||]
  where name = 'VerifiedForm

-- | Run verification on the form.
runVerification :: Form index a -> VerificationResult
runVerification =
  either DuplicateField (const Verified) . flip evalStateT Set.empty . go
  where
    go :: Form index x -> StateT (Set Text) (Either Text) ()
    go =
      \case
        ValueForm {} -> pure ()
        MapValueForm _ f -> go f
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
