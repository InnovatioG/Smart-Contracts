{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Constants where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types        as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

validTxTimeRange :: LedgerApiV2.POSIXTime
validTxTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos

--------------------------------------------------------------------------------2

protocolID_TN :: T.TN
protocolID_TN = LedgerApiV2.TokenName "ProtocolID"

tokenEmergencyAdmin_TN :: T.TN
tokenEmergencyAdmin_TN = LedgerApiV2.TokenName "EmergencyAdmin"

tokenAdmin_TN :: T.TN
tokenAdmin_TN = LedgerApiV2.TokenName "Admin"

campaignID_TN :: T.TN
campaignID_TN = LedgerApiV2.TokenName "CampaignID"

campaignFundsID_TN_basename :: BuiltinByteString
campaignFundsID_TN_basename = "CampaignFundsID"

--------------------------------------------------------------------------------2

-- protocolVersion :: Integer
-- protocolVersion = 1

-- campaignVersion :: Integer
-- campaignVersion = 1

--------------------------------------------------------------------------------2

-- | Helper function to create a version number with a dependency
mkVersionWithDependency :: [Integer] -> Integer -> Integer
mkVersionWithDependency xs ownVersion
  = foldr (\x acc -> acc * 100 + x) ownVersion xs

--------------------------------------------------------------------------------2
