{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : TestUtils.Helpers
Description : Common Mock Data and Auxiliary Functions for testing.
-}
module TestUtils.HelpersINNOVATIO where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                        as P
import qualified Deploy              
import qualified System.Directory               as SystemDirectory

-- IOG imports
import qualified Ledger
import qualified Ledger.Crypto                  as LedgerCrypo
import qualified Plutus.V2.Ledger.Api           as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins              as TxBuiltins
import           PlutusTx.Prelude               as Ptx hiding (($), (*), (+), (++), (-), (.), (<>), (==))

-- Project imports

import qualified Helpers.Deploy          as DeployHelpers
import qualified Helpers.OffChain       as OffChainHelpers
import qualified Helpers.OnChain        as OnChainHelpers
import qualified Constants             as T
import qualified Campaign.Funds.Types    as CampaignFundsT
import qualified Campaign.Types            as CampaignT
import qualified Protocol.Types        as ProtocolT
import qualified Script.Types          as ScriptT
import qualified Types                 as T
import           TestUtils.Constants
import           TestUtils.Helpers
import           TestUtils.TestContext.Evaluate
import           TestUtils.TypesINNOVATIO
import TestUtils.Contracts.InitialData

--------------------------------------------------------------------------------

getTestParams :: String -> IO TestParams
getTestParams filePath = do
    maybeDeployParams <- loadDeployParams filePath
    !params <- case maybeDeployParams of
            Just params -> generateTestParams params
            Nothing     -> P.error "Failed to load deploy parameters"
    return params

--------------------------------------------------------------------------------

getTestCompiledCodeScripts :: String -> IO TestCompiledCodeScripts
getTestCompiledCodeScripts filePath = do
    maybeDeployParams <- loadDeployParams filePath
    case maybeDeployParams of
            Just params -> generateScripts params
            Nothing     -> P.error "Failed to load deploy parameters"

--------------------------------------------------------------------------------

loadDeployParams :: FilePath -> IO (Maybe Deploy.ProtocolAndCampaignFactoryDeployParams)
loadDeployParams filePath = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists
        then do
            ------------------------------
            P.putStrLn $ "Reading Deploy File: " <> filePath
            ------------------------------
            OffChainHelpers.readDecodedFromFile filePath
        else do
            deployParams <- Deploy.deploy_ProtocolFactory_And_CampaingFactory "export" "test" False
            return $ Just deployParams

--------------------------------------------------------------------------------

generateTestParams :: Deploy.ProtocolAndCampaignFactoryDeployParams -> IO TestParams
generateTestParams deployAllParams = do
    putStrLn "Generating Test Params..."
    testCompiledCodeScripts <- generateScripts deployAllParams
    let
        !testParams =
            do
                let
                    ------------
                    protocolPolicyID_TxOutRef =
                        LedgerApiV2.TxOutRef
                            { LedgerApiV2.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
                            , LedgerApiV2.txOutRefIdx = 0
                            }
                    campaignPolicy_TxOutRef =
                        LedgerApiV2.TxOutRef
                            { LedgerApiV2.txOutRefId = "10b4bb4711b96b90672a3db089e7ca78ec3377bd1a7eaccc82d29a115a3d316a"
                            , LedgerApiV2.txOutRefIdx = 1
                            }
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
                                -- params = exampleCampaignValidator_Params
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
                                --   protocolPolicyID_CS campaignPolicy_TxHash campaignPolicy_TxOutputIndex campaignValidator_Hash
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
                    , tpCampaignAdmins = campaignAdmins
                    , tpCampaignToken_CS        = campaignPolicy_CS
                    , tpCampaignToken_TN        = campaignToken_TN
                    , tpCampaignToken_PriceADA  = campaignToken_PriceADA
                    , tpRequestedMaxADA      = requestedMaxADA
                    , tpRequestedMinADA      = requestedMinADA
                    , tpBeginAt              = beginDate
                    , tpDeadline             = deadlineDate
                    , tpMilestones           = [milestones]
                    , tpTransactionDate = transactionDate
                    }

    putStrLn "Generate Test Params - OK"
    putStrLn "---------------"

    return testParams

--------------------------------------------------------------------------------

generateScripts :: Deploy.ProtocolAndCampaignFactoryDeployParams -> IO TestCompiledCodeScripts
generateScripts deployAllParams = do
    putStrLn "Generating Scripts..."
    let
        !testCompiledCodeScripts =
            TestCompiledCodeScripts
                { tccsProtocolPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpProtocolPolicyID_Pre_CborHex deployAllParams
                , tccsProtocolValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpProtocolValidator_Pre_CborHex deployAllParams
                , tccsScriptPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpScriptPolicyID_Pre_CborHex deployAllParams
                , tccsScriptValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpScriptValidator_Pre_CborHex deployAllParams
                , tccsCampaignPolicy_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpCampaignPolicy_Pre_CborHex deployAllParams
                , tccsCampaignValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpCampaignValidator_Pre_CborHex deployAllParams
                , tccsCampaignFundsPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpCampaignFundsPolicyID_Pre_CborHex deployAllParams
                , tccsCampaignFundsValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ Deploy.fdpCampaignFundsValidator_Pre_CborHex deployAllParams
                }
    return testCompiledCodeScripts

--------------------------------------------------------------------------------

testContextWrapper :: TestParams ->  LedgerApiV2.ScriptContext -> IO [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))]
testContextWrapper tp ctx  =
    let results = testContext (findValidator tp) (findMintingPolicy tp) (findValidatorRedeemerNameMaybe tp) (findMintingPolicyRedeemerNameMaybe tp) ctx False
    in results

testContextWrapperTrace :: TestParams ->  LedgerApiV2.ScriptContext -> IO [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))]
testContextWrapperTrace tp ctx =
    let results = testContext (findValidator tp) (findMintingPolicy tp) (findValidatorRedeemerNameMaybe tp) (findMintingPolicyRedeemerNameMaybe tp) ctx True
    in results

--------------------------------------------------------------------------------

findValidator :: TestParams -> LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.Validator
findValidator tp scriptHash
    | scriptHash == OffChainHelpers.hashScriptValidator (tpProtocolValidator tp) = Just (tpProtocolValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpScriptValidator tp) = Just (tpScriptValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpCampaignValidator tp) = Just (tpCampaignValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpCampaignFundsValidator tp) = Just (tpCampaignFundsValidator tp)
    | otherwise = Nothing

findMintingPolicy :: TestParams -> LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.MintingPolicy
findMintingPolicy tp scriptHash
    | scriptHash == OffChainHelpers.hashScriptMinting (tpProtocolPolicyID tp) = Just (tpProtocolPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpScriptPolicyID tp) = Just (tpScriptPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpCampaignPolicy tp) = Just (tpCampaignPolicy tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpCampaignFundsPolicyID tp) = Just (tpCampaignFundsPolicyID tp)
    | otherwise = Nothing

----------------------------------------------------------------------------------------

findValidatorRedeemerNameMaybe :: TestParams -> Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe ValidatorTestRedeemer
findValidatorRedeemerNameMaybe tp scriptHashMaybe redeemer =
    case scriptHashMaybe of
            Just scriptHash -> findValidatorRedeemerName tp scriptHash redeemer
            Nothing         -> Nothing

findValidatorRedeemerName :: TestParams -> LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe ValidatorTestRedeemer
findValidatorRedeemerName tp scriptHash (LedgerApiV2.Redeemer redeemer)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpProtocolValidator tp) =
        getValidatorTestRedeemer (case ProtocolT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @ProtocolT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Protocol_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpScriptValidator tp) =
        getValidatorTestRedeemer (case ScriptT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @ScriptT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Script_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpCampaignValidator tp) =
        getValidatorTestRedeemer (case CampaignT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @CampaignT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Campaign_" ++  x
                    Nothing -> Nothing)
     | scriptHash == OffChainHelpers.hashScriptValidator (tpCampaignFundsValidator tp) =
        getValidatorTestRedeemer (case CampaignFundsT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @CampaignFundsT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "CampaignFunds_" ++ x
                    Nothing -> Nothing)
    | otherwise = Nothing

----------------------------------------------------------------------------------------

findMintingPolicyRedeemerNameMaybe :: TestParams -> Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe PolicyTestRedeemer
findMintingPolicyRedeemerNameMaybe tp scriptHashMaybe redeemer =
    case scriptHashMaybe of
            Just scriptHash ->findMintingPolicyRedeemerName tp scriptHash redeemer
            Nothing         -> Nothing

findMintingPolicyRedeemerName :: TestParams -> LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe PolicyTestRedeemer
findMintingPolicyRedeemerName tp scriptHash (LedgerApiV2.Redeemer redeemer)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpProtocolPolicyID tp) =
        Nothing
    | scriptHash == OffChainHelpers.hashScriptMinting (tpScriptPolicyID tp) =
        getPolicyTestRedeemer (case ScriptT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @ScriptT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "Script_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpCampaignPolicy tp) =
        getPolicyTestRedeemer (case CampaignT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @CampaignT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "Campaign_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpCampaignFundsPolicyID tp) =
        getPolicyTestRedeemer (case CampaignFundsT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @CampaignFundsT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "CampaignFunds_" ++ x
                    Nothing -> Nothing)
    | otherwise = Nothing


----------------------------------------------------------------------------------------

