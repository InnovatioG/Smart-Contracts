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

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types        as T
import qualified Protocol.Types       as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Protocol_Datum_With_NormalChanges #-}
mkUpdated_Protocol_Datum_With_NormalChanges :: T.ProtocolDatumType ->
    [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_NormalChanges !protocolDatum_In !admins !tokenAdminPolicy_CS  =
    protocolDatum_In {
        T.pdAdmins = admins,
        T.pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
    }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Protocol_Datum_With_MinADAChanged #-}
mkUpdated_Protocol_Datum_With_MinADAChanged :: T.ProtocolDatumType -> Integer -> T.ProtocolDatumType
mkUpdated_Protocol_Datum_With_MinADAChanged !protocolDatum_In !newMinADA =
    protocolDatum_In {
        T.pdMinADA = newMinADA
    }

--------------------------------------------------------------------------------2
