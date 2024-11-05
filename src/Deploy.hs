{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication" -}
--------------------------------------------------------------------------------2

module Deploy where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2
import qualified Control.Monad.IO.Class  as MonadIOClass (MonadIO (..))
import qualified Data.Aeson              as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema     as DataOpenApiSchema (ToSchema)
import qualified Data.Time               as DataTime (defaultTimeLocale, formatTime, getCurrentTime)
import qualified GHC.Generics            as GHCGenerics (Generic)
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins.Class as TxBuiltinsClass
import           PlutusTx.Prelude        hiding (unless)
import qualified Prelude                 as P
import qualified Schema
import qualified System.Directory        as SystemDirectory
import qualified System.FilePath.Posix   as SystemFilePathPosix
--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Campaign.Funds.OnChain  as CampaignFundsOnChain
import qualified Campaign.Funds.Types    as CampaignFundsT
import qualified Campaign.OnChain        as CampaignOnChain
import qualified Campaign.Types          as CampaignT
import qualified Constants               as T
import qualified Helpers.CLI             as CLIHelpers
import qualified Helpers.Deploy          as DeployHelpers
import qualified Helpers.OffChain        as OffChainHelpers
import qualified Protocol.OnChain        as ProtocolOnChain
import qualified Protocol.Types          as ProtocolT
import qualified Script.OnChain          as ScriptOnChain
import qualified Script.Types            as ScriptT

--------------------------------------------------------------------------------2
-- Types
--------------------------------------------------------------------------------2

data ProtocolDeployParams
    = ProtocolDeployParams
          { pdpProtocolVersion                       :: Integer
          , pdpTokenEmergencyAdmin_CS                :: LedgerApiV2.CurrencySymbol
          , pdpProtocolPolicyID_Params               :: ProtocolT.PolicyParams
          , pdpProtocolPolicyID_CborHex              :: P.String
          , pdpProtocolPolicyID_CS                   :: LedgerApiV2.CurrencySymbol
          , pdpProtocolValidator_Params              :: ProtocolT.ValidatorParams
          , pdpProtocolValidator_Hash                :: LedgerApiV2.ValidatorHash
          , pdpProtocolValidator_CborHex             :: P.String
          , pdpProtocolValidator_AddressTestnet      :: P.String
          , pdpProtocolValidator_AddressMainnet      :: P.String
          , pdpScriptPolicyID_Params                 :: ScriptT.PolicyParams
          , pdpScriptPolicyID_CborHex                :: P.String
          , pdpScriptPolicyID_CS                     :: LedgerApiV2.CurrencySymbol
          , pdpScriptValidator_Params                :: ScriptT.ValidatorParams
          , pdpScriptValidator_Hash                  :: LedgerApiV2.ValidatorHash
          , pdpScriptValidator_CborHex               :: P.String
          , pdpScriptValidator_AddressTestnet        :: P.String
          , pdpScriptValidator_AddressMainnet        :: P.String
          , pdpCampaignValidator_Params              :: CampaignT.ValidatorParams
          , pdpCampaignValidator_Hash                :: LedgerApiV2.ValidatorHash
          , pdpCampaignValidator_CborHex             :: P.String
          , pdpCampaignValidator_AddressTestnet      :: P.String
          , pdpCampaignValidator_AddressMainnet      :: P.String
          , pdpCampaignFundsValidator_Params         :: CampaignFundsT.ValidatorParams
          , pdpCampaignFundsValidator_Hash           :: LedgerApiV2.ValidatorHash
          , pdpCampaignFundsValidator_CborHex        :: P.String
          , pdpCampaignFundsValidator_AddressTestnet :: P.String
          , pdpCampaignFundsValidator_AddressMainnet :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data CampaignFactoryDeployParams
    = CampaignFactoryDeployParams
          { ffdpCampaignVersion                   :: Integer
          , ffdpCampaignPolicy_Pre_CborHex        :: P.String
          , ffdpCampaignFundsPolicyID_Pre_CborHex :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)


data ProtocolAndCampaignFactoryDeployParams
    = ProtocolAndCampaignFactoryDeployParams
          { dapProtocolVersion                    :: Integer
          , dapCampaignVersion                    :: Integer
          , dapProtocolPolicyID_Pre_CborHex       :: P.String
          , dapProtocolValidator_Pre_CborHex      :: P.String
          , dapScriptPolicyID_Pre_CborHex         :: P.String
          , dapScriptValidator_Pre_CborHex        :: P.String
          , dapCampaignPolicy_Pre_CborHex         :: P.String
          , dapCampaignValidator_Pre_CborHex      :: P.String
          , dapCampaignFundsPolicyID_Pre_CborHex  :: P.String
          , dapCampaignFundsValidator_Pre_CborHex :: P.String
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)


--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

-- Para obtener deploy file de un protocolo especifico con una fabrica de Campaign incluida
deploy_Protocol_And_CampaignFactory_With_StringParams :: P.String -> P.String -> P.IO ()
deploy_Protocol_And_CampaignFactory_With_StringParams protocolPolicyID_TxOutRefStr tokenEmergencyAdminPolicy_CS_Str = do
    ------------------------------
    let path = "export/protocol-v" ++ P.show T.protocolVersion
    ------------------------------
    -- Get the current time
    currentTime <- MonadIOClass.liftIO DataTime.getCurrentTime
    -- Format the time. "%Y-%m-%d-%H-%M" corresponds to "yyyy-mm-dd-hh-mm"
    let defaultName = DataTime.formatTime DataTime.defaultTimeLocale "%Y-%m-%d-%H-%M" currentTime
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn $ "Protocol Name (default=" ++ defaultName ++ "):"
    !protocolName <- MonadIOClass.liftIO $ CLIHelpers.getStrWithDefault defaultName
    ------------------------------
    let
        !protocolPolicyID_TxOutRef = OffChainHelpers.unsafeReadTxOutRef protocolPolicyID_TxOutRefStr
        !tokenEmergencyAdminPolicy_CS = LedgerApiV2.CurrencySymbol $ TxBuiltinsClass.toBuiltin  $ OffChainHelpers.stringFromHexString tokenEmergencyAdminPolicy_CS_Str
    ------------------------------
    deploy_Protocol path protocolName protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS
    -- deploy_CampaignFactory (path SystemFilePathPosix.</> protocolName) "campaign-factory"

-- Para obtener deploy files de un protocolo especifico
deploy_Protocol :: P.FilePath -> P.String -> LedgerApiV2.TxOutRef ->LedgerApiV2.CurrencySymbol -> P.IO ()
deploy_Protocol path protocolName protocolPolicyID_TxOutRef tokenEmergencyAdminPolicy_CS = do
    ------------------------------
    SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> protocolName)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> protocolName)
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Protocol Deploy File..."
    ------------------------------
    P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> protocolName
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol PolicyID' Script..."
    MonadIOClass.liftIO $ P.putStrLn $ "Protocol PolicyID TxOutRef: " ++ P.show protocolPolicyID_TxOutRef
    ------------------------------
    let protocolPolicyParams =
            ProtocolT.PolicyParams
                {
                    ProtocolT.ppProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
                }
        protocolPolicyID = ProtocolOnChain.policyID protocolPolicyParams
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID
    MonadIOClass.liftIO $ P.putStrLn $ "Protocol PolicyParams: " ++ P.show protocolPolicyParams
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ProtocolPolicyID" protocolPolicyID protocolPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Protocol Validator' Script..."
    let protocolValidatorParams =
            ProtocolT.ValidatorParams
                    {
                        ProtocolT.vpProtocolPolicyID_CS = protocolPolicyID_CS,
                        ProtocolT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                    }
        protocolValidator = ProtocolOnChain.validator protocolValidatorParams
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator
        protocolValidator_Address = OffChainHelpers.addressValidator protocolValidator_Hash
    MonadIOClass.liftIO $ P.putStrLn $ "Protocol ValidatorParams: " ++ P.show protocolValidatorParams
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ProtocolValidator" protocolValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script PolicyID' Script..."
    let scriptPolicyParams =
            ScriptT.PolicyParams
                    {
                        ScriptT.ppProtocolPolicyID_CS = protocolPolicyID_CS
                    }
        scriptPolicyID = ScriptOnChain.policyID scriptPolicyParams
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployMintingPolicy (path SystemFilePathPosix.</> protocolName) "ScriptPolicyID" scriptPolicyID scriptPolicyID_CS
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Script Validator' Script..."
    let scriptValidatorParams =
            ScriptT.ValidatorParams
                    {
                        ScriptT.vpScriptPolicyID_CS = scriptPolicyID_CS,
                        ScriptT.vpProtocolPolicyID_CS = protocolPolicyID_CS
                    }
        scriptValidator = ScriptOnChain.validator scriptValidatorParams
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator
        scriptValidator_Address = OffChainHelpers.addressValidator scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "ScriptValidator" scriptValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'Campaign Validator' Script..."
    let campaignValidatorParams =
            CampaignT.ValidatorParams
                {
                    CampaignT.vpProtocolPolicyID_CS = protocolPolicyID_CS,
                    CampaignT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        campaignValidator = CampaignOnChain.validator campaignValidatorParams
        campaignValidator_Hash = OffChainHelpers.hashValidator campaignValidator
        campaignValidator_Address = OffChainHelpers.addressValidator campaignValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "CampaignValidator" campaignValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "CampaignValidator" campaignValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "CampaignValidator" campaignValidator_Address
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating 'CampaignFunds Validator ' Script..."
    let campaignFundsValidatorParams =
            CampaignFundsT.ValidatorParams
                {
                    CampaignFundsT.vpProtocolPolicyID_CS = protocolPolicyID_CS,
                    CampaignFundsT.vpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
                }
        campaignFundsValidator = CampaignFundsOnChain.validator campaignFundsValidatorParams
        campaignFundsValidator_Hash = OffChainHelpers.hashValidator campaignFundsValidator
        campaignFundsValidator_Address = OffChainHelpers.addressValidator campaignFundsValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidator (path SystemFilePathPosix.</> protocolName) "CampaignFundsValidator" campaignFundsValidator
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorHash (path SystemFilePathPosix.</> protocolName) "CampaignFundsValidator" campaignFundsValidator_Hash
    _ <- MonadIOClass.liftIO $ DeployHelpers.deployValidatorAddress (path SystemFilePathPosix.</> protocolName) "CampaignFundsValidator" campaignFundsValidator_Address
    ------------------------------
    protocolPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolPolicyID.plutus")
    protocolValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator.plutus")
    protocolValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator-Testnet.addr")
    protocolValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolValidator-Mainnet.addr")
    ------------------------------
    scriptPolicyID_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptPolicyID.plutus")
    scriptValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator.plutus")
    scriptValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator-Testnet.addr")
    scriptValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ScriptValidator-Mainnet.addr")
    ------------------------------
    campaignValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignValidator.plutus")
    campaignValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignValidator-Testnet.addr")
    campaignValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignValidator-Mainnet.addr")
    ------------------------------
    campaignFundsValidator_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignFundsValidator.plutus")
    campaignFundsValidator_Address_Testnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignFundsValidator-Testnet.addr")
    campaignFundsValidator_Address_Mainnet <- OffChainHelpers.readFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "CampaignFundsValidator-Mainnet.addr")
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Creating Protocol Deploy File..."
    ------------------------------
    let protocolDeployParams =
            ProtocolDeployParams {
                pdpProtocolVersion = T.protocolVersion,
                pdpTokenEmergencyAdmin_CS = tokenEmergencyAdminPolicy_CS,
                pdpProtocolPolicyID_Params = protocolPolicyParams,
                pdpProtocolPolicyID_CborHex = OffChainHelpers.lazyByteStringToString protocolPolicyID_CborHex,
                pdpProtocolPolicyID_CS = protocolPolicyID_CS,
                pdpProtocolValidator_Params = protocolValidatorParams,
                pdpProtocolValidator_Hash = protocolValidator_Hash,
                pdpProtocolValidator_CborHex = OffChainHelpers.lazyByteStringToString protocolValidator_CborHex,
                pdpProtocolValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString protocolValidator_Address_Testnet,
                pdpProtocolValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString protocolValidator_Address_Mainnet,
                pdpScriptPolicyID_Params          = scriptPolicyParams,
                pdpScriptPolicyID_CborHex         = OffChainHelpers.lazyByteStringToString scriptPolicyID_CborHex,
                pdpScriptPolicyID_CS              = scriptPolicyID_CS,
                pdpScriptValidator_Params         = scriptValidatorParams,
                pdpScriptValidator_Hash           = scriptValidator_Hash,
                pdpScriptValidator_CborHex        = OffChainHelpers.lazyByteStringToString scriptValidator_CborHex,
                pdpScriptValidator_AddressTestnet = OffChainHelpers.lazyByteStringToString scriptValidator_Address_Testnet,
                pdpScriptValidator_AddressMainnet = OffChainHelpers.lazyByteStringToString scriptValidator_Address_Mainnet,
                pdpCampaignValidator_Params          = campaignValidatorParams,
                pdpCampaignValidator_Hash            = campaignValidator_Hash,
                pdpCampaignValidator_CborHex         = OffChainHelpers.lazyByteStringToString campaignValidator_CborHex,
                pdpCampaignValidator_AddressTestnet  = OffChainHelpers.lazyByteStringToString campaignValidator_Address_Testnet,
                pdpCampaignValidator_AddressMainnet  = OffChainHelpers.lazyByteStringToString campaignValidator_Address_Mainnet ,
                pdpCampaignFundsValidator_Params          = campaignFundsValidatorParams,
                pdpCampaignFundsValidator_Hash            = campaignFundsValidator_Hash,
                pdpCampaignFundsValidator_CborHex         = OffChainHelpers.lazyByteStringToString campaignFundsValidator_CborHex,
                pdpCampaignFundsValidator_AddressTestnet  = OffChainHelpers.lazyByteStringToString campaignFundsValidator_Address_Testnet,
                pdpCampaignFundsValidator_AddressMainnet  = OffChainHelpers.lazyByteStringToString campaignFundsValidator_Address_Mainnet
            }
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolDeploy.json") protocolDeployParams
    ------------------------------
    P.putStrLn $ "Saved Protocol Deploy in: " ++ P.show (path SystemFilePathPosix.</> protocolName SystemFilePathPosix.</> "ProtocolDeploy.json")
    P.putStrLn "--------------------------------"
    ------------------------------

deploy_CampaignFactory :: P.FilePath -> P.String ->  P.IO ()
deploy_CampaignFactory path campaignFactoryName =
    do
        SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> campaignFactoryName)
        SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> campaignFactoryName)
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating Campaign Factory Deploy File..."
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> campaignFactoryName
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'Campaign Policy' Pre-Script..."
        let campaignPolicy =  Plutonomy.optimizeUPLC  CampaignOnChain.policyCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignPolicy_PRE.plutus") campaignPolicy
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignPolicy_PRE.serialized") campaignPolicy
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "Generating 'CampaignFunds PolicyID' Pre-Script..."
        let campaignFundsPolicyID =  Plutonomy.optimizeUPLC  CampaignFundsOnChain.policyIDCode
        DeployHelpers.writeCompiledCodeToJsonFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignFundsPolicyID_PRE.plutus") campaignFundsPolicyID
        -- DeployHelpers.writeCompiledCodeToBinaryFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignFundsPolicyID_PRE.serialized") campaignFundsPolicyID
        ------------------------------
        MonadIOClass.liftIO $ P.putStrLn "--------------------------------"
        MonadIOClass.liftIO $ P.putStrLn "Creating Campaign Factory Deploy File..."
        ------------------------------
        campaignPolicy_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignPolicy_PRE.plutus")
        campaignFundsPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignFundsPolicyID_PRE.plutus")
        ------------------------------
        let campaignFactoryDeployParams =
                CampaignFactoryDeployParams {
                    ffdpCampaignVersion                 = T.campaignVersion,
                    ffdpCampaignPolicy_Pre_CborHex           = OffChainHelpers.lazyByteStringToString  campaignPolicy_Pre_CborHex,
                    ffdpCampaignFundsPolicyID_Pre_CborHex    = OffChainHelpers.lazyByteStringToString  campaignFundsPolicyID_Pre_CborHex
                }
        OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignFactoryDeploy.json") campaignFactoryDeployParams
        ------------------------------
        P.putStrLn $ "Saved Campaign Factory Deploy in: " ++ P.show (path SystemFilePathPosix.</> campaignFactoryName SystemFilePathPosix.</> "CampaignFactoryDeploy.json")
        P.putStrLn "--------------------------------"
        ------------------------------

------------------------------------------------------------------------------2

deploy_PRE_script ::  P.FilePath -> P.String -> Bool -> PlutusTx.CompiledCode a -> P.IO ()
deploy_PRE_script filePath name swOverWrite code = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists && not swOverWrite
        then do
            MonadIOClass.liftIO $ P.putStrLn $ "Reading '" ++ name ++ "' Pre-Script..."
        else do
            MonadIOClass.liftIO $ P.putStrLn $ "Generating '" ++ name ++ "' Pre-Script..."
            -- let !optimizedCode = Plutonomy.optimizeUPLC code
            let optimizedCode = code
            -- DeployHelpers.writeCompiledCodeToBinaryFile filePath optimizedCode
            DeployHelpers.writeCompiledCodeToJsonFile filePath optimizedCode

------------------------------------------------------------------------------2

deploy_ProtocolFactory_And_CampaingFactory ::  P.FilePath -> P.String -> Bool -> P.IO ProtocolAndCampaignFactoryDeployParams
deploy_ProtocolFactory_And_CampaingFactory path name swOverWrite = do
    ------------------------------
    -- SystemDirectory.removePathForcibly (path SystemFilePathPosix.</> name)
    SystemDirectory.createDirectoryIfMissing True (path SystemFilePathPosix.</> name)
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn "Generating Deploy Files..."
    ------------------------------
    MonadIOClass.liftIO $ P.putStrLn $ "Path: " ++ path SystemFilePathPosix.</> name
    ------------------------------
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus") "ProtocolPolicyID" swOverWrite ProtocolOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus") "ProtocolValidator" swOverWrite ProtocolOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus") "ScriptPolicyID" swOverWrite ScriptOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptValidator_PRE.plutus") "ScriptValidator" swOverWrite ScriptOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignPolicy_PRE.plutus") "CampaignPolicy" swOverWrite CampaignOnChain.policyCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignValidator_PRE.plutus") "CampaignValidator" swOverWrite CampaignOnChain.validatorCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignFundsPolicyID_PRE.plutus") "CampaignFundsPolicyID" swOverWrite CampaignFundsOnChain.policyIDCode
    deploy_PRE_script (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignFundsValidator_PRE.plutus") "CampaignFundsValidator" swOverWrite CampaignFundsOnChain.validatorCode
    ------------------------------
    MonadIOClass.liftIO $P.putStrLn "--------------------------------"
    ------------------------------
    protocolPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolPolicyID_PRE.plutus")
    protocolValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ProtocolValidator_PRE.plutus")
    scriptPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptPolicyID_PRE.plutus")
    scriptValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "ScriptValidator_PRE.plutus")
    campaignPolicy_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignPolicy_PRE.plutus")
    campaignValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignValidator_PRE.plutus")
    campaignFundsPolicyID_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignFundsPolicyID_PRE.plutus")
    campaignFundsValidator_Pre_CborHex <- OffChainHelpers.readFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "CampaignFundsValidator_PRE.plutus")
    ------------------------------
    let deployAllPreParams =
                ProtocolAndCampaignFactoryDeployParams {
                    dapProtocolVersion  = T.protocolVersion,
                    dapCampaignVersion                 = T.campaignVersion,
                    dapProtocolPolicyID_Pre_CborHex    = OffChainHelpers.lazyByteStringToString protocolPolicyID_Pre_CborHex,
                    dapProtocolValidator_Pre_CborHex   = OffChainHelpers.lazyByteStringToString protocolValidator_Pre_CborHex,
                    dapScriptPolicyID_Pre_CborHex      = OffChainHelpers.lazyByteStringToString scriptPolicyID_Pre_CborHex,
                    dapScriptValidator_Pre_CborHex     = OffChainHelpers.lazyByteStringToString scriptValidator_Pre_CborHex,
                    dapCampaignPolicy_Pre_CborHex           = OffChainHelpers.lazyByteStringToString  campaignPolicy_Pre_CborHex,
                    dapCampaignValidator_Pre_CborHex = OffChainHelpers.lazyByteStringToString  campaignValidator_Pre_CborHex,
                    dapCampaignFundsPolicyID_Pre_CborHex    = OffChainHelpers.lazyByteStringToString  campaignFundsPolicyID_Pre_CborHex,
                    dapCampaignFundsValidator_Pre_CborHex   = OffChainHelpers.lazyByteStringToString  campaignFundsValidator_Pre_CborHex
                }
    ------------------------------
    OffChainHelpers.writeEncodedToFile (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "deploy.json") deployAllPreParams
    ------------------------------
    P.putStrLn $ "Saved Deploy File in: " ++ P.show (path SystemFilePathPosix.</> name SystemFilePathPosix.</> "deploy.json")
    P.putStrLn "--------------------------------"
    ------------------------------
    return deployAllPreParams

------------------------------------------------------------------------------2

loadFactoryDeployParams :: SystemFilePathPosix.FilePath -> Bool -> P.IO (Maybe ProtocolAndCampaignFactoryDeployParams)
loadFactoryDeployParams filePath swOverWrite = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists && not swOverWrite
        then do
            OffChainHelpers.readDecodedFromFile filePath
        else do
            deployParams <- deploy_ProtocolFactory_And_CampaingFactory "export" "test" False
            return $ Just deployParams

------------------------------------------------------------------------------2
