{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Protocol.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types           as T
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_ProtocolDatum_With_NormalChanges #-}
mkUpdated_ProtocolDatum_With_NormalChanges :: T.ProtocolDatumType -> 
    [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> T.ProtocolDatumType
mkUpdated_ProtocolDatum_With_NormalChanges !protocolDatum_In !admins !tokenAdminPolicy_CS  =
    protocolDatum_In {
        T.pdAdmins = admins,
        T.pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_ProtocolDatum_With_NewMinADA #-}
mkUpdated_ProtocolDatum_With_NewMinADA :: T.ProtocolDatumType -> Integer -> T.ProtocolDatumType
mkUpdated_ProtocolDatum_With_NewMinADA !protocolDatum_In !newMinADA =
    protocolDatum_In {
        T.pdMinADA = newMinADA
    }

--------------------------------------------------------------------------------2
