-- :NOTE USE THIS {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- :NOTE USE THIS {-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{- |
Module      : TestUtils.Types
Description : Types for the Test Utils.
-}
module TestUtils.Types where

--------------------------------------------------------------------------------
-- App Monad
--------------------------------------------------------------------------------

import qualified Codec.Serialise                 as Serialise
import qualified Control.Monad.IO.Class          as MonadIO
import qualified Control.Monad.Reader            as MReader
import qualified Data.Aeson                      as DataAeson
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base16          as B16
import qualified Data.ByteString.Char8           as BSC
import qualified Data.ByteString.Lazy            as BSL
import qualified Flat
import qualified GHC.Generics                    as GHCGenerics
import qualified GHC.Stack                       as GHC (CallStack, HasCallStack, callStack, prettyCallStack)
import           Prelude
import qualified System.Directory                as SystemDirectory
import qualified System.FilePath.Posix           as SystemFilePathPosix

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                      as LedgerAda
import qualified Ledger.Address                  as LedgerAddress
import qualified Ledger.Crypto                   as LedgerCrypo
import qualified Plutonomy
import qualified Plutus.Script.Utils.V2.Scripts  as ScriptsUtilsV2
import qualified Plutus.V1.Ledger.Interval       as LedgerInterval
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins               as TxBuiltins
import qualified PlutusTx.Code                   as PlutusTxCode
import           PlutusTx.Prelude                (BuiltinData, divide)
import qualified Wallet.Emulator.Types           as WalletEmulatorTypes (XPrv)

-- Project imports

import qualified Helpers.Deploy           as DeployHelpers
import qualified Helpers.OffChain         as OffChainHelpers
import qualified Helpers.OnChain          as OnChainHelpers
import qualified Helpers.Types                   as T
import qualified Campaign.Funds.OnChain as CampaignFunds
import qualified Campaign.Funds.Types   as CampaignFundsT
import qualified Campaign.Helpers       as CampaignHelpers
import qualified Campaign.OnChain       as Campaign
import qualified Campaign.Types         as CampaignT
import qualified Constants              as T
import qualified Protocol.OnChain       as Protocol
import qualified Protocol.Types         as ProtocolT
import qualified Script.Types           as ScriptT
import qualified Types                  as T

--------------------------------------------------------------------------------
-- Scripts And Test Params
--------------------------------------------------------------------------------

data TestCompiledCodeScripts
    = TestCompiledCodeScripts
          { tccsProtocolPolicyID_Pre       :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsProtocolValidator_Pre      :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsScriptPolicyID_Pre         :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsScriptValidator_Pre        :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsCampaignPolicy_Pre         :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsCampaignValidator_Pre      :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsCampaignFundsPolicyID_Pre  :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          , tccsCampaignFundsValidator_Pre :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
          }

data TestParams
    = TestParams
          { tpProtocolPolicyID             :: LedgerApiV2.MintingPolicy
          , tpProtocolPolicyID_CS          :: LedgerApiV2.CurrencySymbol
          , tpProtocolValidator            :: LedgerApiV2.Validator
          , tpProtocolValidator_Hash       :: LedgerApiV2.ValidatorHash
          , tpScriptPolicyID               :: LedgerApiV2.MintingPolicy
          , tpScriptPolicyID_CS            :: LedgerApiV2.CurrencySymbol
          , tpScriptValidator              :: LedgerApiV2.Validator
          , tpScriptValidator_Hash         :: LedgerApiV2.ValidatorHash
          , tpCampaignPolicy               :: LedgerApiV2.MintingPolicy
          , tpCampaignPolicy_CS            :: LedgerApiV2.CurrencySymbol
          , tpCampaignValidator            :: LedgerApiV2.Validator
          , tpCampaignValidator_Hash       :: LedgerApiV2.ValidatorHash
          , tpCampaignFundsPolicyID        :: LedgerApiV2.MintingPolicy
          , tpCampaignFundsPolicyID_CS     :: LedgerApiV2.CurrencySymbol
          , tpCampaignFundsValidator       :: LedgerApiV2.Validator
          , tpCampaignFundsValidator_Hash  :: LedgerApiV2.ValidatorHash
          , tpProtocolPolicyID_TxOutRef    :: Ledger.TxOutRef
          , tpCampaignPolicy_TxOutRef      :: Ledger.TxOutRef
          , tpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          , tpTokenAdminPolicy_CS          :: LedgerApiV2.CurrencySymbol
          , tpProtocolAdmins               :: [T.WalletPaymentPKH]
          , tpCampaignAdmins               :: [T.WalletPaymentPKH]
          , tpMint_CampaignFT              :: Bool
          , tpCampaignFT_CS                :: T.CS
          , tpCampaignFT_TN                :: T.TN
          , tpCampaignFT_PriceADA          :: Integer
          , tpRequestedMaxADA              :: Integer
          , tpRequestedMinADA              :: Integer
          , tpFundedADA                :: Integer
          , tpCollectedADA             :: Integer
          , tpBeginAt                      :: LedgerApiV2.POSIXTime
          , tpDeadline                     :: LedgerApiV2.POSIXTime
          , tpStatus                       :: CampaignT.CapaignStatus
          , tpMilestones                   :: [CampaignT.CampaignMilestones]
          , tpTransactionDate              :: LedgerApiV2.POSIXTime
          , tpDepositDate                  :: LedgerApiV2.POSIXTime
          , tpWithdrawDate                 :: LedgerApiV2.POSIXTime
          , tpCollectCommissionsDate       :: LedgerApiV2.POSIXTime
          }
    deriving (Eq, Show)
