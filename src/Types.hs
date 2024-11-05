{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types        as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

class HasAdmins a where
    getAdmins :: a -> [T.WalletPaymentPKH]

class HasAdminToken a where
    getAdminToken_CS :: a -> LedgerApiV2.CurrencySymbol

class ShowDatum datum where
    showCborAsDatumType :: BuiltinData -> Maybe P.String

--------------------------------------------------------------------------------2
