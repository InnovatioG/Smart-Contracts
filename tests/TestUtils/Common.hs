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
Module      : TestUtils.Common
Description : Common Mock Data and Auxiliary Functions for testing.
-}
module TestUtils.Common where

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
import qualified Test.QuickCheck                 as QC
import qualified Test.Tasty.HUnit                as Tasty
import qualified Debug.Trace                     as DebugTrace

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
import qualified Deploy                 as T
import qualified Protocol.OnChain       as Protocol
import qualified Protocol.Types         as ProtocolT
import qualified Script.Types           as ScriptT
import qualified Types                  as T
import           TestUtils.Types


--------------------------------------------------------------------------------
-- Constants for test parameters
--------------------------------------------------------------------------------

_CHANGE_VALUE :: Bool
_CHANGE_VALUE = True
_NO_CHANGE_VALUE :: Bool
_NO_CHANGE_VALUE = False
_VALID_VALUE_MINADA_CHANGE :: Bool
_VALID_VALUE_MINADA_CHANGE = True
_INVALID_VALUE_MINADA_CHANGE :: Bool
_INVALID_VALUE_MINADA_CHANGE = False
_CHANGE_DATUM :: Bool
_CHANGE_DATUM = True
_NO_CHANGE_DATUM :: Bool
_NO_CHANGE_DATUM = False
_VALID_DATUM_CHANGE :: Bool
_VALID_DATUM_CHANGE = False
_INVALID_DATUM_CHANGE :: Bool
_INVALID_DATUM_CHANGE = True
_VALID_SIGNATURE :: Bool
_VALID_SIGNATURE = True
_INVALID_SIGNATURE :: Bool
_INVALID_SIGNATURE = False
_USE_ADMIN_TOKEN :: Bool
_USE_ADMIN_TOKEN = True
_DONT_USE_ADMIN_TOKEN :: Bool
_DONT_USE_ADMIN_TOKEN = False
_USE_EMERGENCY_TOKEN :: Bool
_USE_EMERGENCY_TOKEN = True
_DONT_USE_EMERGENCY_TOKEN :: Bool
_DONT_USE_EMERGENCY_TOKEN = False
_VALID_TOKEN_INDEX :: Bool
_VALID_TOKEN_INDEX = True
_INVALID_TOKEN_INDEX :: Bool
_INVALID_TOKEN_INDEX = False

--------------------------------------------------------------------------------

generateTestParams :: T.ProtocolAndCampaignFactoryDeployParams -> IO TestParams
generateTestParams factoryDeployparams = do
    putStrLn "---------------"
    putStrLn "Generate Test Params - INIT"

    let
        !testCompiledCodeScripts =
            TestCompiledCodeScripts
                { tccsProtocolPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapProtocolPolicyID_Pre_CborHex factoryDeployparams
                , tccsProtocolValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapProtocolValidator_Pre_CborHex factoryDeployparams
                , tccsScriptPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapScriptPolicyID_Pre_CborHex factoryDeployparams
                , tccsScriptValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapScriptValidator_Pre_CborHex factoryDeployparams
                , tccsCampaignPolicy_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapCampaignPolicy_Pre_CborHex factoryDeployparams
                , tccsCampaignValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapCampaignValidator_Pre_CborHex factoryDeployparams
                , tccsCampaignFundsPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapCampaignFundsPolicyID_Pre_CborHex factoryDeployparams
                , tccsCampaignFundsValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapCampaignFundsValidator_Pre_CborHex factoryDeployparams
                }

    putStrLn "---------------"

    let
        !testParams =
            do
                let
                    ------------
                    protocolPolicyID_TxOutRef =
                        LedgerApiV2.TxOutRef
                            { LedgerApiV2.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000007"
                            , LedgerApiV2.txOutRefIdx = 1
                            }
                    campaignPolicy_TxOutRef =
                        LedgerApiV2.TxOutRef
                            { LedgerApiV2.txOutRefId = "10b4bb4711b96b90672a3db089e7ca78ec3377bd1a7eaccc82d29a115a3d316a"
                            , LedgerApiV2.txOutRefIdx = 1
                            }
                    ------------
                    protocolAdmins = ["0000000000000000000000000000000000000000000000000000000000000004"]
                    fundAdmins = ["0000000000000000000000000000000000000000000000000000000000000005"]
                    ------------
                    tokenEmergencyAdminPolicy_CS = "0000000000000000000000000000000000000000000000000000000000000003"
                    tokenAdminPolicy_CS = "0000000000000000000000000000000000000000000000000000000000000002"
                    ------------
                    mint_CampaignFT = False
                    -- campaignFT_CS
                    campaignFT_TN = "FT"
                    campaignFT_PriceADA = 1_000_000
                    requestedMaxADA = 10_000_000
                    requestedMinADA = 5_000_000
                    fundedADA = 8_000_000
                    collectedADA = 0
                    ------------
                    beginDate = 1 :: LedgerApiV2.POSIXTime
                    deadlineDate = LedgerApiV2.POSIXTime ((10 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                    ------------
                    status = CampaignT.CsCreated
                    milestones = CampaignT.CampaignMilestones {
                            CampaignT.cmEstimatedDeliveryDate = LedgerApiV2.POSIXTime ((5 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                            , CampaignT.cmPerncentage           = 100
                            , CampaignT.cmStatus                =CampaignT.MsCreated
                            }
                    ------------
                    transactionDate = LedgerApiV2.POSIXTime ((2 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                    depositDate = LedgerApiV2.POSIXTime ((3 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                    withdrawDate = LedgerApiV2.POSIXTime ((4 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                    collectCommissionsDate = LedgerApiV2.POSIXTime ((6 * 30 * 24 * 60 * 60 * 1000) :: Integer)
                    ------------
                let
                    protocolPolicyID =
                        do
                            let
                                code = tccsProtocolPolicyID_Pre testCompiledCodeScripts
                                -- params = exampleProtocolPolicyParams
                                param1 = TxBuiltins.mkB $ LedgerApiV2.getTxId $ LedgerApiV2.txOutRefId protocolPolicyID_TxOutRef
                                param2 = TxBuiltins.mkI $ LedgerApiV2.txOutRefIdx protocolPolicyID_TxOutRef
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkMintingPolicyScript appliedCode
                let
                    protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID

                let
                    protocolValidator =
                        do
                            let
                                code = tccsProtocolValidator_Pre testCompiledCodeScripts
                                -- params = exampleProtocolValidatorParams
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkValidatorScript appliedCode
                let
                    protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator

                let
                    scriptPolicyID =
                        do
                            let
                                code = tccsScriptPolicyID_Pre testCompiledCodeScripts
                                -- params = exampleScriptPolicyParams
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                            LedgerApiV2.mkMintingPolicyScript appliedCode
                let
                    scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID

                let
                    scriptValidator =
                        do
                            let
                                code = tccsScriptValidator_Pre testCompiledCodeScripts
                                -- params = exampleScriptValidatorParams
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol scriptPolicyID_CS
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkValidatorScript appliedCode
                let
                    scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator

                let
                    campaignValidator =
                        do
                            let
                                code = tccsCampaignValidator_Pre testCompiledCodeScripts
                                -- params = exampleFundValidator_Params
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkValidatorScript appliedCode
                let
                    campaignValidator_Hash = OffChainHelpers.hashValidator campaignValidator

                let
                    campaignPolicy =
                        do
                            let
                                code = tccsCampaignPolicy_Pre testCompiledCodeScripts
                                --   protocolPolicyID_CS fundPolicy_TxHash fundPolicy_TxOutputIndex campaignValidator_Hash
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                param2 = TxBuiltins.mkB $ LedgerApiV2.getTxId $ LedgerApiV2.txOutRefId campaignPolicy_TxOutRef
                                param3 = TxBuiltins.mkI $ LedgerApiV2.txOutRefIdx campaignPolicy_TxOutRef
                                (LedgerApiV2.ValidatorHash hash) = campaignValidator_Hash
                                param4 = TxBuiltins.mkB hash
                                appliedCode =
                                    code
                                        `PlutusTx.applyCode` PlutusTx.liftCode param1
                                        `PlutusTx.applyCode` PlutusTx.liftCode param2
                                        `PlutusTx.applyCode` PlutusTx.liftCode param3
                                        `PlutusTx.applyCode` PlutusTx.liftCode param4
                            LedgerApiV2.mkMintingPolicyScript appliedCode

                let
                    campaignPolicy_CS = OffChainHelpers.getCurSymbolOfPolicy campaignPolicy

                let
                    campaignFundsValidator =
                        do
                            let
                                code = tccsCampaignFundsValidator_Pre testCompiledCodeScripts
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                                param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkValidatorScript appliedCode
                let
                    campaignFundsValidator_Hash = OffChainHelpers.hashValidator campaignFundsValidator

                let
                    campaignFundsPolicyID =
                        do
                            let
                                code = tccsCampaignFundsPolicyID_Pre testCompiledCodeScripts
                                -- params = exampleCampaignFundsPolParams
                                param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol campaignPolicy_CS
                                (LedgerApiV2.ValidatorHash hash) = campaignFundsValidator_Hash
                                param2 = TxBuiltins.mkB hash
                                appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1  `PlutusTx.applyCode` PlutusTx.liftCode param2
                            LedgerApiV2.mkMintingPolicyScript appliedCode

                let
                    campaignFundsPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy campaignFundsPolicyID

                TestParams
                    { tpProtocolPolicyID = protocolPolicyID
                    , tpProtocolPolicyID_CS = protocolPolicyID_CS
                    , tpProtocolValidator = protocolValidator
                    , tpProtocolValidator_Hash = protocolValidator_Hash
                    , tpScriptPolicyID = scriptPolicyID
                    , tpScriptPolicyID_CS = scriptPolicyID_CS
                    , tpScriptValidator = scriptValidator
                    , tpScriptValidator_Hash = scriptValidator_Hash
                    , tpCampaignPolicy = campaignPolicy
                    , tpCampaignPolicy_CS = campaignPolicy_CS
                    , tpCampaignValidator = campaignValidator
                    , tpCampaignValidator_Hash = campaignValidator_Hash
                    , tpCampaignFundsPolicyID = campaignFundsPolicyID
                    , tpCampaignFundsPolicyID_CS = campaignFundsPolicyID_CS
                    , tpCampaignFundsValidator = campaignFundsValidator
                    , tpCampaignFundsValidator_Hash = campaignFundsValidator_Hash
                    , tpProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
                    , tpCampaignPolicy_TxOutRef = campaignPolicy_TxOutRef
                    , tpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    , tpTokenAdminPolicy_CS = tokenAdminPolicy_CS
                    , tpProtocolAdmins = protocolAdmins
                    , tpCampaignAdmins = fundAdmins
                    , tpMint_CampaignFT      = mint_CampaignFT
                    , tpCampaignFT_CS        = campaignPolicy_CS
                    , tpCampaignFT_TN        = campaignFT_TN
                    , tpCampaignFT_PriceADA  = campaignFT_PriceADA
                    , tpRequestedMaxADA      = requestedMaxADA
                    , tpRequestedMinADA      = requestedMinADA
                    , tpCollectedADA= collectedADA
                    , tpFundedADA= fundedADA
                    , tpBeginAt              = beginDate
                    , tpDeadline             = deadlineDate
                    , tpStatus               = status
                    , tpMilestones           = [milestones]
                    , tpTransactionDate = transactionDate
                    , tpDepositDate = depositDate
                    , tpWithdrawDate = withdrawDate
                    , tpCollectCommissionsDate = collectCommissionsDate
                    }

    putStrLn "Generate Test Params - OK"
    putStrLn "---------------"

    return testParams

--------------------------------------------------------------------------------
-- Auxiliary Functions
--------------------------------------------------------------------------------

mkCampaignFundsID_TN :: Integer -> LedgerApiV2.TokenName
mkCampaignFundsID_TN index =
    LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS index


--------------------------------------------------------------------------------

findValidator :: TestParams -> LedgerApiV2.Address -> Maybe LedgerApiV2.Validator
findValidator tp address
    | address == OffChainHelpers.addressValidator (tpProtocolValidator_Hash tp) = Just (tpProtocolValidator tp)
    | address == OffChainHelpers.addressValidator (tpScriptValidator_Hash tp) = Just (tpScriptValidator tp)
    | address == OffChainHelpers.addressValidator (tpCampaignValidator_Hash tp) = Just (tpCampaignValidator tp)
    | address == OffChainHelpers.addressValidator (tpCampaignFundsValidator_Hash tp) = Just (tpCampaignFundsValidator tp)
    | otherwise = Nothing

findMintingPolicy :: TestParams -> LedgerApiV2.CurrencySymbol -> Maybe LedgerApiV2.MintingPolicy
findMintingPolicy tp cs
    | cs == tpProtocolPolicyID_CS tp = Just (tpProtocolPolicyID tp)
    | cs == tpScriptPolicyID_CS tp = Just (tpScriptPolicyID tp)
    | cs == tpCampaignPolicy_CS tp = Just (tpCampaignPolicy tp)
    | cs == tpCampaignFundsPolicyID_CS tp = Just (tpCampaignFundsPolicyID tp)
    | otherwise = Nothing

--------------------------------------------------------------------------------