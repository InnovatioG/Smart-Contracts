{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
module TestUtils.TypesINNOVATIO where

--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.IO.Class  as MonadIO
import qualified Control.Monad.Reader    as MReader
import qualified Data.List               as DataList
import           Prelude                 as P
import qualified Text.Read               as TextRead

-- IOG imports
import qualified Ledger
import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx
import qualified Wallet.Emulator.Types   as WalletEmulatorTypes (XPrv)

-- Project imports
import qualified Helpers.Types           as T
import qualified Protocol.Types as ProtocolT
import           TestUtils.Types
import qualified Campaign.Types as CampaignT

--------------------------------------------------------------------------------
data TestCompiledCodeScripts
    = TestCompiledCodeScripts
          { tccsProtocolPolicyID_Pre       :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsProtocolValidator_Pre      :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsScriptPolicyID_Pre         :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsScriptValidator_Pre        :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsCampaignPolicy_Pre         :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsCampaignValidator_Pre      :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsCampaignFundsPolicyID_Pre  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsCampaignFundsValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          }

instance Show TestCompiledCodeScripts where
  show _ = "<TestCompiledCodeScripts>"


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
          , tpCampaignToken_CS                :: T.CS
          , tpCampaignToken_TN                :: T.TN
          , tpCampaignToken_PriceADA          :: Integer
          , tpRequestedMaxADA              :: Integer
          , tpRequestedMinADA              :: Integer
          , tpBeginAt                      :: LedgerApiV2.POSIXTime
          , tpDeadline                     :: LedgerApiV2.POSIXTime
          , tpMilestones                   :: [CampaignT.CampaignMilestones]
          , tpTransactionDate              :: LedgerApiV2.POSIXTime
          }
    deriving (Eq, Show)

instance Eq WalletEmulatorTypes.XPrv where
  (==) :: WalletEmulatorTypes.XPrv -> WalletEmulatorTypes.XPrv -> Bool
  _ == _ = True

instance Show WalletEmulatorTypes.XPrv where
  show :: WalletEmulatorTypes.XPrv -> String
  show _ = "<XPrv>"

-----------------------------------------------------------------
data TestEntity = Protocol_TestEntity | ProtocolTxoutRefNeeded_TestEntity | Campaign_TestEntity | CampaignFunds_TestEntity deriving (P.Show)

instance Pretty TestEntity where
  pretty entity = stripSuffix "_TestEntity" (P.show entity)

data TestToken = ProtocolID_TestToken | CampaignID_TestToken | CampaignToken_TestToken | CampaignFundsID_TestToken  deriving (P.Show)

instance Pretty TestToken where
  pretty token = stripSuffix "_TestToken" (P.show token)

data PolicyTestRedeemer = 
  Campaign_MintID_TestRedeemer | Campaign_BurnID_TestRedeemer | Campaign_MintCampaignToken_TestRedeemer | Campaign_BurnCampaignToken | 
    Script_MintID_TestRedeemer | Script_BurnID_TestRedeemer | Protocol_MintID_TestRedeemer | CampaignFunds_MintID_TestRedeemer | CampaignFunds_BurnID_TestRedeemer deriving
        ( P.Eq
        , P.Read
        , P.Show
        )

instance Pretty PolicyTestRedeemer where
  pretty redeemer = stripSuffix "_TestRedeemer" (P.show redeemer)

data ValidatorTestRedeemer = 
  Protocol_DatumUpdate_TestRedeemer | Protocol_UpdateMinADA_TestRedeemer | Protocol_Emergency_TestRedeemer |  
  Campaign_DatumUpdate_TestRedeemer  |
  Campaign_UpdateMinADA_TestRedeemer  |
  Campaign_FundsAdd_TestRedeemer  |
  Campaign_FundsMerge_TestRedeemer  |
  Campaign_FundsDelete_TestRedeemer  |
  Campaign_FundsCollect_TestRedeemer  |
  Campaign_InitializeCampaign_TestRedeemer  |
  Campaign_ReachedCampaign_TestRedeemer  |
  Campaign_NotReachedCampaign_TestRedeemer  |
  Campaign_MilestoneAprobe_TestRedeemer  |
  Campaign_MilestoneReprobe_TestRedeemer  |
  Campaign_Emergency_TestRedeemer  |
  Campaign_Delete_TestRedeemer  |
  CampaignFunds_UpdateMinADA_TestRedeemer |
  CampaignFunds_Deposit_TestRedeemer |
  CampaignFunds_Withdraw_TestRedeemer |
  CampaignFunds_Sell_TestRedeemer |
  CampaignFunds_GetBack_TestRedeemer |
  CampaignFunds_Collect_TestRedeemer |
  CampaignFunds_Merge_TestRedeemer |
  CampaignFunds_Delete_TestRedeemer |
  CampaignFunds_BalanceAssets_TestRedeemer |
  CampaignFunds_Emergency_TestRedeemer  | 
  Script_Delete_TestRedeemer | Generic_TestRedeemer deriving
        ( P.Eq
        , P.Read
        , P.Show
        )

instance Pretty ValidatorTestRedeemer where
  pretty redeemer = stripSuffix "_TestRedeemer" (P.show redeemer)

-- Helper function to convert string into the appropriate ValidatorTestRedeemer
getValidatorTestRedeemer :: Maybe String -> Maybe ValidatorTestRedeemer
getValidatorTestRedeemer (Just str) =
  TextRead.readMaybe (str ++ "_TestRedeemer")
getValidatorTestRedeemer _ = Nothing

getPolicyTestRedeemer :: Maybe String -> Maybe PolicyTestRedeemer
getPolicyTestRedeemer (Just str) = TextRead.readMaybe (str ++ "_TestRedeemer")
getPolicyTestRedeemer _          = Nothing

-----------------------------------------------------------------

data TestTransactions = 
  Protocol_Create_Tx | Protocol_DatumUpdate_Tx | Protocol_UpdateMinADA_Tx | Protocol_Emergency_Tx | 
  Campaign_Create_Tx | 
  Campaign_DatumUpdate_Tx  |
  Campaign_UpdateMinADA_Tx  |
  Campaign_FundsAdd_Tx  |
  Campaign_FundsMerge_Tx  |
  Campaign_FundsDelete_Tx  |
  Campaign_FundsCollect_Tx  |
  Campaign_InitializeCampaign_Tx  |
  Campaign_ReachedCampaign_Tx  |
  Campaign_NotReachedCampaign_Tx  |
  Campaign_MilestoneAprobe_Tx  |
  Campaign_MilestoneReprobe_Tx  |
  Campaign_Emergency_Tx  |
  Campaign_Delete_Tx  |
  CampaignFunds_UpdateMinADA_Tx |
  CampaignFunds_Deposit_Tx |
  CampaignFunds_Withdraw_Tx |
  CampaignFunds_Sell_Tx |
  CampaignFunds_GetBack_Tx |
  CampaignFunds_BalanceAssets_Tx |
  CampaignFunds_Emergency_Tx 
  deriving
        ( P.Show
        )



-----------------------------------------------------------------
data RedeemerLog
    = RedeemerLogValidator (Maybe ValidatorTestRedeemer)
    | RedeemerLogPolicy (Maybe PolicyTestRedeemer)
    deriving (P.Eq)

instance P.Show RedeemerLog where
  show = getRedeemerNameFromLog

getRedeemerNameFromLog :: RedeemerLog -> String
getRedeemerNameFromLog (RedeemerLogValidator (Just r)) = pretty r
getRedeemerNameFromLog (RedeemerLogPolicy (Just r))    = pretty r
-- getRedeemerNameFromLog (RedeemerLogValidator Nothing) = "Nothing"
-- getRedeemerNameFromLog (RedeemerLogPolicy Nothing) = "Nothing"
-- getRedeemerNameFromLog s = error $ "Redeemer not found "
getRedeemerNameFromLog _                               = error "Redeemer not found "

-- Function to extract the base name from a redeemer name
extractBaseName :: String -> String
extractBaseName redeemerName =
  case DataList.break (== '_') redeemerName of
    (baseName, _) -> baseName

getRedeemerScriptNameFromLog :: RedeemerLog -> String
getRedeemerScriptNameFromLog (RedeemerLogValidator (Just r)) =
  let baseName = extractBaseName (pretty r)
   in baseName ++ "_Validator"
getRedeemerScriptNameFromLog (RedeemerLogPolicy (Just r)) =
  let baseName = extractBaseName (pretty r)
   in baseName ++ "_Policy"
getRedeemerScriptNameFromLog s = error $ "Redeemer not found " ++ show s
-----------------------------------------------------------------
