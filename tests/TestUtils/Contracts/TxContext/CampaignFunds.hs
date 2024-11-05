--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

{- |
Module      : TestUtils.Contracts.TxContext.CampaignFunds
Description :
-}
module TestUtils.Contracts.TxContext.CampaignFunds where

-- Non-IOG imports
import           Prelude                         as P hiding (negate, (<>))

-- IOG imports
import qualified Plutus.V2.Ledger.Api            as LedgerApiV2
import           PlutusTx.Prelude                (head, negate, (<>))
import qualified PlutusTx.Prelude                as Ptx
import qualified PlutusTx.Ratio                  as TxRatio

-- Project imports
import qualified Campaign.Funds.Types            as CampaignFundsT
import qualified Campaign.Helpers                as CampaignHelpers
import qualified Campaign.Types                  as CampaignT
import qualified Helpers.OnChain                 as OnChainHelpers
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesINNOVATIO
import qualified Types                           as T

--------------------------------------------------------------------------------
-- CampaignFunds Contract
--------------------------------------------------------------------------------

-- CampaignFunds_UpdateMinADA_Tx |
--   CampaignFunds_Deposit_Tx |
--   CampaignFunds_Withdraw_Tx |
--   CampaignFunds_Sell_Tx |
--   CampaignFunds_GetBack_Tx |
--   CampaignFunds_BalanceAssets_Tx |
--   CampaignFunds_Emergency_Tx

campaignFunds_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaignFunds_Create_TxContext tp =
    mkContext
        |> setInputsRef [ uTxOForValidatorAsReference tp (tpCampaignValidator tp), uTxOForMintingAsReference tp (tpCampaignFundsPolicyID tp)]
        |> setInputsAndAddRedeemers [(campaign_UTxO_MockData tp, CampaignT.mkCampaignFundsAddRedeemer)]
        |> setOutputs [campaign_UTxO_With_Added_CampaignFunds_MockData tp, campaignFunds_UTxO_With_NoDeposits_MockData tp 0]
        |> setMintAndAddRedeemers
                [(  LedgerApiV2.singleton
                            (tpCampaignFundsPolicyID_CS tp)
                            (CampaignHelpers.mkCampaignFundsID_TN 0)
                            1
                        , CampaignFundsT.mkMintIDRedeemer)]
        |> setSignatories (tpCampaignAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

campaignFunds_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaignFunds_Delete_TxContext tp =
    mkContext
        |> setInputsRef [ uTxOForValidatorAsReference tp (tpCampaignValidator tp), uTxOForValidatorAsReference tp (tpCampaignFundsValidator tp), uTxOForMintingAsReference tp (tpCampaignFundsPolicyID tp)]
        |> setInputsAndAddRedeemers [(campaign_UTxO_With_Added_CampaignFunds_MockData tp, CampaignT.mkCampaignFundsDeleteRedeemer 1), (campaignFunds_UTxO_With_NoDeposits_MockData tp 0, CampaignFundsT.mkDeleteRedeemer)]
        |> setOutputs [campaign_UTxO_With_Deleted_CampaignFunds_MockData tp]
        |> setMintAndAddRedeemers
                [(  LedgerApiV2.singleton
                            (tpCampaignFundsPolicyID_CS tp)
                            (CampaignHelpers.mkCampaignFundsID_TN 0)
                            (-1)
                        , CampaignFundsT.mkBurnIDRedeemer)]
        |> setSignatories (tpCampaignAdmins tp)
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

campaignFunds_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
campaignFunds_UpdateMinADA_TxContext tp newMinADA =
    let
        input_CampaignFunds_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp 0
        input_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds_UTxO
        input_CampaignFunds_Value = LedgerApiV2.txOutValue input_CampaignFunds_UTxO
        -----------------
        output_CampaignFunds_Datum = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_MinADAChanged
                input_CampaignFunds_Datum
                newMinADA
        output_CampaignFunds_UTxO = input_CampaignFunds_UTxO
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    CampaignFundsT.mkDatum output_CampaignFunds_Datum
            , LedgerApiV2.txOutValue =
                changeValue_Amount
                    input_CampaignFunds_Value
                    OnChainHelpers.adaAssetClass
                    newMinADA
            }
        -----------------
    in
        mkContext
            |> setInputsRef [campaign_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_CampaignFunds_UTxO, CampaignFundsT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_CampaignFunds_UTxO]
            |> setSignatories (tpCampaignAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- CampaignFunds Merge
--------------------------------------------------------------------------------

campaignFunds_Merge_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaignFunds_Merge_TxContext tp =
    let
        input_Campaign_UTxO = (campaign_UTxO_MockData tp)  { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $ CampaignT.mkDatum $ (campaign_DatumType_MockData tp) {
                        CampaignT.cdFundsIndex = 2,
                        CampaignT.cdFundsCount = 2
                    }}
        input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO

        -- Two Campaign Funds to merge
        input_CampaignFunds1_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp 0
        input_CampaignFunds2_UTxO = campaignFunds_UTxO_With_Deposits_MockData tp 1 (tpRequestedMinADA tp)

        -- Output after merge
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_CampaignFundsDeleted
            input_Campaign_Datum
            1  -- Reducing funds count by 1 after merge

        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }

        -- Merged output for CampaignFunds
        output_CampaignFunds_UTxO = input_CampaignFunds1_UTxO -- Use first UTxO as base for merge
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [ (input_Campaign_UTxO, CampaignT.mkCampaignFundsMergeRedeemer 2) -- 2 funds being merged
                , (input_CampaignFunds1_UTxO, CampaignFundsT.mkMergeRedeemer)
                , (input_CampaignFunds2_UTxO, CampaignFundsT.mkMergeRedeemer)
                ]
            |> setOutputs [output_Campaign_UTxO, output_CampaignFunds_UTxO]
            |> setSignatories (tpCampaignAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
-- CampaignFunds Collect
--------------------------------------------------------------------------------

campaignFunds_Collect_TxContext :: TestParams -> LedgerApiV2.ScriptContext
campaignFunds_Collect_TxContext tp =
    let
        collectAmount = 1000000000 -- Amount to collect

        input_Campaign_UTxO = campaign_UTxO_With_Added_CampaignFunds_MockData tp
        input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO

        input_CampaignFunds_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp 0
        input_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds_UTxO

        -- Update Campaign datum with collected amount
        output_Campaign_Datum = CampaignHelpers.mkUpdated_Campaign_Datum_With_CampaignFundsCollected
            input_Campaign_Datum
            collectAmount

        output_Campaign_UTxO = input_Campaign_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum output_Campaign_Datum }

         -- Update CampaignFund datum with collected amount
        output_CampaignFund_Datum = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Collect
            input_CampaignFunds_Datum
            collectAmount

        output_CampaignFunds_UTxO = input_CampaignFunds_UTxO
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum output_CampaignFund_Datum }
    in
        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers
                [ (input_Campaign_UTxO, CampaignT.mkCampaignFundsCollectRedeemer collectAmount)
                , (input_CampaignFunds_UTxO, CampaignFundsT.mkCollectRedeemer collectAmount)
                ]
            |> setOutputs [output_Campaign_UTxO, output_CampaignFunds_UTxO]
            |> setSignatories (tpCampaignAdmins tp)
            |> setValidyRange (createValidRange (tpTransactionDate tp))


-- campaignFunds_Deposit_TxContext_Wrapper :: TestParams -> TxContextParametrizable
-- campaignFunds_Deposit_TxContext_Wrapper tp txParams =
--     let
--         depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
--         depositAmount = getTxParam "depositAmount" txParams :: Integer
--     in campaignFunds_Deposit_TxContext tp depositDate depositAmount

-- campaignFunds_Deposit_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
-- campaignFunds_Deposit_TxContext tp depositDate depositAmount =
--     let
--         --------------------
--         input_Campaign_UTxO = campaign_UTxO_MockData tp
--         input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO
--         --------------------
--         input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
--         input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
--         input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
--         --------------------
--         input_CampaignFunds_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp
--         input_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds_UTxO
--         input_CampaignFunds_Value = LedgerApiV2.txOutValue input_CampaignFunds_UTxO
--         --------------------
--         (userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6) = calculateDepositCommissionsUsingMonths_Parametrizable tp input_Campaign_Datum depositDate depositAmount
--         --------------------
--         investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositAmount
--         --------------------
--         output_CampaignFunds_Datum = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Deposit
--                 input_CampaignFunds_Datum
--                 depositAmount userFT commissionsFT commissions_FT_Release_PerMonth_1e6
--         output_CampaignFunds_UTxO = input_CampaignFunds_UTxO
--             { LedgerApiV2.txOutDatum =
--                 LedgerApiV2.OutputDatum $
--                     CampaignFundsT.mkDatum output_CampaignFunds_Datum
--             , LedgerApiV2.txOutValue = input_CampaignFunds_Value <> investUnit_Value <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpCampaignFT_TN tp) commissionsFT
--             }
--         -----------------
--     in
--         mkContext
--             |> setInputsRef [input_Campaign_UTxO, input_InvestUnit_UTxO,
--                 uTxOForValidatorAsReference tp (tpCampaignFundsValidator tp), uTxOForMintingAsReference tp (tpCampaignPolicy tp)]
--             |> setInputsAndAddRedeemers [(input_CampaignFunds_UTxO, CampaignFundsT.mkDepositRedeemer depositDate depositAmount)]
--             |> setOutputs [output_CampaignFunds_UTxO]
--             |> setMintAndAddRedeemers[ (LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpCampaignFT_TN tp) depositAmount, CampaignT.mkMintFTRedeemer)]
--             |> setValidyRange (createValidRange depositDate)

-- --------------------------------------------------------------------------------

-- campaignFunds_Withdraw_TxContext_Wrapper :: TestParams -> TxContextParametrizable
-- campaignFunds_Withdraw_TxContext_Wrapper tp txParams =
--     let
--         depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
--         depositAmount = getTxParam "depositAmount" txParams :: Integer
--         withdrawDate = getTxParam "withdrawDate" txParams :: LedgerApiV2.POSIXTime
--         withdrawAmount = getTxParam "withdrawAmount" txParams :: Integer
--     in campaignFunds_Withdraw_TxContext tp depositDate depositAmount withdrawDate withdrawAmount

-- campaignFunds_Withdraw_TxContext :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.ScriptContext
-- campaignFunds_Withdraw_TxContext tp depositDate depositAmount withdrawDate withdrawAmount =
--     let
--         --------------------
--         input_Campaign_UTxO = campaign_UTxO_MockData tp
--         input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO
--         --------------------
--         input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
--         input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
--         input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
--         --------------------
--         base_CampaignFunds_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp
--         base_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO base_CampaignFunds_UTxO
--         --------------------
--         input_CampaignFunds_UTxO = campaignFunds_UTxO_With_Deposits_MockData_Parametrizable tp input_Campaign_Datum base_CampaignFunds_Datum input_InvestUnit 0 depositAmount depositDate
--         input_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds_UTxO
--         input_CampaignFunds_Value = LedgerApiV2.txOutValue input_CampaignFunds_UTxO
--         --------------------
--         investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues input_InvestUnit)
--         (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp input_Campaign_Datum withdrawDate withdrawAmount investUnit_Granularity
--         --------------------
--         investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit withdrawPlusCommissionsGetBack
--         --------------------
--         output_CampaignFunds_Datum = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Withdraw
--                 input_CampaignFunds_Datum
--                 withdrawAmount commissionsForUserFTToGetBack commissions_FT_Release_PerMonth_1e6
--         output_CampaignFunds_UTxO = input_CampaignFunds_UTxO
--             { LedgerApiV2.txOutDatum =
--                 LedgerApiV2.OutputDatum $
--                     CampaignFundsT.mkDatum output_CampaignFunds_Datum
--             , LedgerApiV2.txOutValue = input_CampaignFunds_Value <> negate investUnit_Value <> negate (LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpCampaignFT_TN tp) commissionsForUserFTToGetBack)
--             }
--         -----------------
--     in
--         mkContext
--             |> setInputsRef [input_Campaign_UTxO, input_InvestUnit_UTxO,
--                               uTxOForValidatorAsReference tp (tpCampaignFundsValidator tp), uTxOForMintingAsReference tp (tpCampaignPolicy tp)]
--             |> setInputsAndAddRedeemers [(input_CampaignFunds_UTxO, CampaignFundsT.mkWithdrawRedeemer withdrawDate withdrawAmount withdrawPlusCommissionsGetBack)]
--             |> setOutputs [output_CampaignFunds_UTxO]
--             |> setMintAndAddRedeemers
--                 [( LedgerApiV2.singleton
--                     (tpCampaignPolicy_CS tp)
--                     (tpCampaignFT_TN tp)
--                     (-withdrawPlusCommissionsGetBack)
--                 , CampaignT.mkBurnFTRedeemer)]
--             |> setValidyRange (createValidRange withdrawDate)

-- --------------------------------------------------------------------------------

-- campaignFunds_Collect_Protocol_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
-- campaignFunds_Collect_Protocol_Commission_TxContext tp =
--      mkContext
--         |> setInputsRef [protocol_UTxO_MockData tp, campaign_UTxO_MockData tp]
--         |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
--         |> setOutputs [campaignFunds_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData]
--         |> setSignatories (tpProtocolAdmins tp)
--         |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

-- --------------------------------------------------------------------------------

-- campaignFunds_Collect_Managers_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
-- campaignFunds_Collect_Managers_Commission_TxContext tp =
--  mkContext
--         |> setInputsRef [protocol_UTxO_MockData tp, campaign_UTxO_MockData tp]
--         |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
--         |> setOutputs [campaignFunds_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData]
--         |> setSignatories (tpCampaignAdmins tp)
--         |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

-- --------------------------------------------------------------------------------

-- campaignFunds_Collect_Delegators_Commission_TxContext :: TestParams -> LedgerApiV2.ScriptContext
-- campaignFunds_Collect_Delegators_Commission_TxContext tp =
--     mkContext
--         |> setInputsRef [protocol_UTxO_MockData tp, campaign_UTxO_MockData tp]
--         |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData)]
--         |> setOutputs [campaignFunds_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData]
--         |> setSignatories (tpDelegatorsAdmins tp)
--         |> setValidyRange (createValidRange (tpCollectCommissionsDate tp))

-- --------------------------------------------------------------------------------

-- campaignFunds_ReIndexing_TxContext :: TestParams -> LedgerApiV2.ScriptContext
-- campaignFunds_ReIndexing_TxContext  = investUnit_ReIndexing_TxContext

-- --------------------------------------------------------------------------------

-- campaignFunds_BalanceAssets_TxContext :: TestParams -> [Integer] -> [Integer] -> [Integer] -> Bool -> [Integer] -> LedgerApiV2.ScriptContext
-- campaignFunds_BalanceAssets_TxContext tp depositsInit depositsAlterations commissionsFTAlterationsForRedeemer useCustomRelease customReleases =
--     let
--         --------------------
--         swTrace = False
--         --------------------
--         depositsInit_1 = P.head depositsInit
--         depositsInit_2 = P.head (tail depositsInit)
--         --------------------
--         depositsAlter_1 = P.head depositsAlterations
--         depositsAlter_2 = P.head (tail depositsAlterations)
--         --------------------
--         input_Campaign_UTxO = campaign_UTxO_MockData tp
--         input_Campaign_Datum = CampaignT.getCampaign_DatumType_From_UTxO input_Campaign_UTxO
--         --------------------
--         input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
--         input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
--         --------------------
--         input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
--         --------------------
--         base_CampaignFunds_UTxO = campaignFunds_UTxO_With_NoDeposits_MockData tp
--         base_CampaignFunds_Datum = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO base_CampaignFunds_UTxO
--         --------------------
--         input_CampaignFunds1_UTxO = campaignFunds_UTxO_With_Deposits_MockData_Parametrizable tp input_Campaign_Datum base_CampaignFunds_Datum input_InvestUnit 0 depositsInit_1 (tpDepositDate tp)
--         input_CampaignFunds1_Value = LedgerApiV2.txOutValue input_CampaignFunds1_UTxO
--         -----------------
--         input_CampaignFunds2_UTxO = campaignFunds_UTxO_With_Deposits_MockData_Parametrizable tp input_Campaign_Datum base_CampaignFunds_Datum input_InvestUnit 1 depositsInit_2 (tpDepositDate tp)
--         input_CampaignFunds2_Value = LedgerApiV2.txOutValue input_CampaignFunds2_UTxO
--         -----------------
--         tokens_InvestUnit_ValueToAlter1 = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositsAlter_1
--         tokens_InvestUnit_ValueToAlter2 = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit depositsAlter_2
--         -----------------
--         output_CampaignFunds1_Value = input_CampaignFunds1_Value <> tokens_InvestUnit_ValueToAlter1
--         output_CampaignFunds2_Value = input_CampaignFunds2_Value <> tokens_InvestUnit_ValueToAlter2
--         -----------------
--         output_CampaignFunds1_UTxO =
--             if null commissionsFTAlterationsForRedeemer then
--                 input_CampaignFunds1_UTxO {
--                         LedgerApiV2.txOutValue = output_CampaignFunds1_Value
--                     }
--             else
--                 let
--                     ------------------
--                     datumIn1 = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds1_UTxO
--                     commissionChange1 = P.head commissionsFTAlterationsForRedeemer
--                     ------------------
--                     !oldCommissions1 = CampaignFundsT.hdSubtotal_FT_Commissions datumIn1
--                     !oldRelease1 = CampaignFundsT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn1
--                     ------------------
--                     (newCommissions1, newRelease1)
--                       | useCustomRelease =
--                           let
--                             !newCommissions = oldCommissions1 + commissionChange1
--                             !newRelease = P.head customReleases -- Tomar el primer valor de customReleases
--                           in
--                             (newCommissions, newRelease)
--                       | oldCommissions1 == 0 =
--                           let
--                             !newCommissions = oldCommissions1 + commissionChange1
--                             !newRelease = 0
--                           in
--                             (newCommissions, newRelease)
--                       | otherwise =
--                           let
--                             !newCommissions = oldCommissions1 + commissionChange1
--                             !changeRatio1 = TxRatio.unsafeRatio commissionChange1 oldCommissions1
--                             !changeRelease1 = TxRatio.truncate (changeRatio1 Ptx.* TxRatio.fromInteger oldRelease1)
--                             !newRelease = oldRelease1 + changeRelease1
--                           in
--                             (newCommissions, newRelease)
--                 in
--                     input_CampaignFunds1_UTxO {
--                         LedgerApiV2.txOutDatum =
--                                 LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum datumIn1
--                                 {
--                                     CampaignFundsT.hdSubtotal_FT_Commissions = newCommissions1
--                                     , CampaignFundsT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 = newRelease1
--                                 }
--                         , LedgerApiV2.txOutValue = output_CampaignFunds1_Value <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpCampaignFT_TN tp) commissionChange1
--                     }
--         -----------------
--         output_CampaignFunds2_UTxO =
--             if length commissionsFTAlterationsForRedeemer < 2 then
--                 input_CampaignFunds2_UTxO{
--                         LedgerApiV2.txOutValue = output_CampaignFunds2_Value
--                     }
--             else
--                 let
--                     ------------------
--                     datumIn1 = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds1_UTxO
--                     commissionChange1 = P.head commissionsFTAlterationsForRedeemer
--                     ------------------
--                     datumIn2 = CampaignFundsT.getCampaignFunds_DatumType_From_UTxO input_CampaignFunds2_UTxO
--                     commissionChange2 = P.head (P.tail commissionsFTAlterationsForRedeemer)
--                     ------------------
--                     !oldCommissions1 = CampaignFundsT.hdSubtotal_FT_Commissions datumIn1
--                     !oldRelease1 = CampaignFundsT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn1
--                     ------------------
--                     !oldCommissions2 = CampaignFundsT.hdSubtotal_FT_Commissions datumIn2
--                     !oldRelease2 = CampaignFundsT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 datumIn2
--                     ------------------
--                     (newCommissions2, newRelease2)
--                       | useCustomRelease =
--                           let
--                             !newCommissions = oldCommissions2 + commissionChange2
--                             !newRelease = P.head (P.tail customReleases) -- Tomar el segundo valor de customReleases
--                           in
--                             (newCommissions, newRelease)
--                       | oldCommissions1 == 0 =
--                           let
--                             !newCommissions = oldCommissions2 + commissionChange2
--                             !newRelease = 0
--                           in
--                             (newCommissions, newRelease)
--                       | otherwise =
--                           let
--                             !newCommissions = oldCommissions2 + commissionChange2
--                             !changeRatio1 = TxRatio.unsafeRatio commissionChange1 oldCommissions1
--                             !changeRelease1 = TxRatio.truncate (changeRatio1 Ptx.* TxRatio.fromInteger oldRelease1)
--                             !newRelease = oldRelease2 - changeRelease1
--                           in
--                             (newCommissions, newRelease)
--                 in
--                     input_CampaignFunds2_UTxO {
--                         LedgerApiV2.txOutDatum =
--                                 LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum datumIn2
--                                 {
--                                     CampaignFundsT.hdSubtotal_FT_Commissions = newCommissions2
--                                     , CampaignFundsT.hdSubtotal_FT_Commissions_Release_PerMonth_1e6 = newRelease2
--                                 }
--                         , LedgerApiV2.txOutValue = output_CampaignFunds2_Value <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpCampaignFT_TN tp) commissionChange2
--                     }
--         -----------------
--     in do
--         mkContext
--             |> setInputsRef [campaign_UTxO_With_Added_CampaignFunds_MockData tp,
--                         uTxOForValidatorAsReference tp (tpCampaignFundsValidator tp)]
--             |> setInputsAndAddRedeemers [(input_CampaignFunds1_UTxO, CampaignFundsT.mkBalanceAssetsRedeemer commissionsFTAlterationsForRedeemer), (input_CampaignFunds2_UTxO, CampaignFundsT.mkBalanceAssetsRedeemer commissionsFTAlterationsForRedeemer)]
--             |> setOutputs [output_CampaignFunds1_UTxO, output_CampaignFunds2_UTxO]
--             |> setSignatories (tpCampaignAdmins tp)
--             |> setValidyRange (createValidRange (tpTransactionDate tp))

-- --------------------------------------------------------------------------------
