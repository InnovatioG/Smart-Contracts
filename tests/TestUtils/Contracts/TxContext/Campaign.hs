--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.Campaign
Description :
-}
module TestUtils.Contracts.TxContext.Campaign where

-- Non-IOG imports

-- IOG imports
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Helpers.OnChain          as OnChainHelpers
import qualified Helpers.Types                   as T
import qualified Constants              as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesINNOVATIO
import qualified Campaign.Types as CampaignT
import qualified Campaign.Helpers as CampaignHelpers
import TestUtils.Contracts.TxContext.CampaignFunds

--------------------------------------------------------------------------------
-- Campaign Contract
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Campaign Create
--------------------------------------------------------------------------------

campaign_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_Create_TxContext tp =
    mkContext
        |> setInputWithTxOufRef (campaign_spend_UTxO_And_TxOutRef_MockData tp)
        |> setInputsRef [protocol_UTxO_MockData tp]
        |> setOutputs [campaign_UTxO_MockData tp]
        |> setMintAndAddRedeemers
            [( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 1
            , CampaignT.mkMintIDRedeemer)]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Delete
--------------------------------------------------------------------------------

campaign_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_Delete_TxContext tp =  
    mkContext
        |> setInputsRef [uTxOForValidatorAsReference tp (tpCampaignValidator tp), uTxOForMintingAsReference tp (tpCampaignPolicy tp)]
        |> setInputsRef [protocol_UTxO_MockData tp]
        |> setInputsAndAddRedeemers [(campaign_UTxO_MockData tp, CampaignT.mkDeleteRedeemer)]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton
                    (tpCampaignPolicy_CS tp) T.campaignID_TN 
                    (negate 1)
                , CampaignT.mkBurnIDRedeemer
                )
            ]
        |> setSignatories (tpProtocolAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Datum Update
--------------------------------------------------------------------------------

campaign_DatumUpdate_TxContext :: TestParams -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> LedgerApiV2.ScriptContext
campaign_DatumUpdate_TxContext tp admins tokenAdminPolicy_CS' =
    let
        input_Campaign_UTxO = campaign_UTxO_MockData tp
        input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO
        -----------------
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NormalChanges
                input_Campaign_Datum
                admins tokenAdminPolicy_CS'
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    CampaignT.mkDatum output_Campaign_Datum
            }
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkDatumUpdateRedeemer)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpCampaignAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign UpdateMinADA
--------------------------------------------------------------------------------

campaign_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
campaign_UpdateMinADA_TxContext tp newMinADA =
    let
        input_Campaign_UTxO = campaign_UTxO_MockData tp
        input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO
        input_Value = LedgerApiV2.txOutValue input_Campaign_UTxO
        -----------------
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_MinADAChanged
                input_Campaign_Datum
                newMinADA
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    CampaignT.mkDatum output_Campaign_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpCampaignAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Funds Add
--------------------------------------------------------------------------------

campaign_FundsAdd_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_FundsAdd_TxContext = campaignFunds_Create_TxContext

--------------------------------------------------------------------------------
-- Campaign Funds Delete
--------------------------------------------------------------------------------

campaign_FundsDelete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_FundsDelete_TxContext = campaignFunds_Delete_TxContext

--------------------------------------------------------------------------------
-- Campaign Funds Merge
--------------------------------------------------------------------------------

campaign_FundsMerge_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_FundsMerge_TxContext = campaignFunds_Merge_TxContext 

--------------------------------------------------------------------------------
-- Campaign Funds Collect
--------------------------------------------------------------------------------

campaign_FundsCollect_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_FundsCollect_TxContext = campaignFunds_Collect_TxContext 

--------------------------------------------------------------------------------
-- Campaign Initialize
--------------------------------------------------------------------------------

campaign_InitializeCampaign_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_InitializeCampaign_TxContext tp =
    let
        input_Campaign_UTxO = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO

        input_CampaignFunds_UTxO = campaignFunds_UTxO_With_Deposits_MockData tp 0 deposit_MockData

        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus
            input_Campaign_Datum
            CampaignT.CsInitialized
            
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, input_CampaignFunds_UTxO]
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkInitializeCampaignRedeemer)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Reached
--------------------------------------------------------------------------------

campaign_ReachedCampaign_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_ReachedCampaign_TxContext tp =
    let
        input_Campaign_UTxO' = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        
        input_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus
            (CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO')
            CampaignT.CsInitialized

        input_Campaign_UTxO = input_Campaign_UTxO'
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum input_Campaign_Datum }

        fundedAmount = CampaignT.cdRequestedMinADA input_Campaign_Datum

        input_CampaignFunds_UTxO = campaignFunds_UTxO_With_Deposits_ADA_And_Sold_MockData tp 0 deposit_MockData (tpRequestedMaxADA tp) fundedAmount 
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatusReached
            input_Campaign_Datum
            fundedAmount
            
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, input_CampaignFunds_UTxO]
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkReachedCampaignRedeemer)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpDeadline tp + 1000000000))

--------------------------------------------------------------------------------
-- Campaign Not Reached
--------------------------------------------------------------------------------

campaign_NotReachedCampaign_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_NotReachedCampaign_TxContext tp =
    let
        input_Campaign_UTxO' = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        
        input_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus
            (CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO')
            CampaignT.CsInitialized

        input_Campaign_UTxO = input_Campaign_UTxO'
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum input_Campaign_Datum }

        fundedAmount = CampaignT.cdRequestedMinADA input_Campaign_Datum - 1

        input_CampaignFunds_UTxO = campaignFunds_UTxO_With_Deposits_ADA_And_Sold_MockData tp 0 deposit_MockData (tpRequestedMaxADA tp) fundedAmount 
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatusReached
            input_Campaign_Datum
            fundedAmount
            
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp, input_CampaignFunds_UTxO]
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkReachedCampaignRedeemer)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpDeadline tp + 1000000000))

--------------------------------------------------------------------------------
-- Campaign Milestone Approve
--------------------------------------------------------------------------------

campaign_MilestoneApprove_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
campaign_MilestoneApprove_TxContext tp milestoneIndex =
    let
        input_Campaign_UTxO' = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        
        input_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus
            (CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO')
            CampaignT.CsReached

        input_Campaign_UTxO = input_Campaign_UTxO'
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum input_Campaign_Datum }
        
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_MilestoneApproved
            input_Campaign_Datum
            milestoneIndex
            
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkMilestoneApproveRedeemer milestoneIndex)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Milestone Reprove
--------------------------------------------------------------------------------

campaign_MilestoneReprobe_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
campaign_MilestoneReprobe_TxContext tp milestoneIndex =
    let
        input_Campaign_UTxO' = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        
        input_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus
            (CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO')
            CampaignT.CsReached

        input_Campaign_UTxO = input_Campaign_UTxO'
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum input_Campaign_Datum }
        
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_MilestoneFailed
            input_Campaign_Datum
            milestoneIndex
            
        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_Campaign_UTxO, CampaignT.mkMilestoneFailRedeemer milestoneIndex)]
            |> setOutputs [output_Campaign_UTxO]
            |> setSignatories (tpProtocolAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- Campaign Emergency
--------------------------------------------------------------------------------

campaign_Emergency_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaign_Emergency_TxContext tp =
    mkContext
        |> setInputsAndAddRedeemers [(campaign_UTxO_MockData tp, CampaignT.mkEmergencyRedeemer)]
        |> setOutputs []  -- Emergency allows taking all value
        |> setSignatories (tpProtocolAdmins tp)  -- Must be signed by protocol admin
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
