--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Campaign.Funds.Validator
Description : Validation logic and tests related to the CampaignFunds module.

This module defines the validation logic for the CampaignFunds contract.

It includes multiple test cases to ensure the integrity and correctness of the
validation script.
-}
module Contracts.Campaign.Funds.Validator where

--------------------------------------------------------------------------------4

-- Non-IOG imports
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import Prelude as P

-- IOG imports

-- Project imports
import TestUtils.Contracts.InitialData
import TestUtils.Contracts.TxContext.CampaignFunds
import TestUtils.HelpersINNOVATIO
import TestUtils.TestContext.Asserts
import TestUtils.TypesINNOVATIO

--------------------------------------------------------------------------------

campaignFunds_Validator_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Validator_Tests tp =
    Tasty.testGroup
        "CampaignFunds Validator Tests"
        [ campaignFunds_Validator_Redeemer_UpdateMinADA_Tests tp
        -- , campaignFunds_Validator_Redeemer_Deposit_Tests tp
        -- , campaignFunds_Validator_Redeemer_Withdraw_Tests tp
        -- , campaignFunds_Validator_Redeemer_Collect_Protocol_Commission_Tests tp
        -- , campaignFunds_Validator_Redeemer_Collect_Managers_Commission_Tests tp
        -- , campaignFunds_Validator_Redeemer_Collect_Delegators_Commission_Tests tp
        -- , campaignFunds_Validator_Redeemer_ReIndexing_Tests tp
        -- , campaignFunds_Validator_Redeemer_BalanceAssets_Tests tp
        , campaignFunds_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

campaignFunds_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show CampaignFunds_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = campaignFunds_UpdateMinADA_TxContext tp toAlter_minAda
            in
                [ Tasty.testCase "Changing min ADA correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                ]

--------------------------------------------------------------------------------

-- campaignFunds_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_Deposit_Tests tp =
--     let
--         ------------------------
--         txName = show Campaign_Deposit_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Deposit_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
--             in
--                 [ Tasty.testCase "Depositing correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Depositing without minting FT must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setMintAndAddRedeemers mempty
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isMintingFT"]
--                 , Tasty.testCase "Depositing without increasing FT minted subtotal must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Minted =
--                                     CampaignFundsT.hdSubtotal_FT_Minted (campaignFunds_DatumType_With_NoDeposits_MockData tp)
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_With_Deposit"]
--                 , Tasty.testCase "Depositing without paying invest units must fail" $ do
--                     let
--                         (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (tpDepositDate tp) deposit_MockData
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                 { LedgerApiV2.txOutValue =
--                                     LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
--                                         <> LedgerApiV2.singleton (tpCampaignFundsPolicyID_CS tp) (mkCampaignFundsID_TN 0) 1
--                                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) commissionsFT_MockData
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_With_Tokens_And_FT"]
--                 , Tasty.testCase "Depositing date outside valid range must fail" $ do
--                     let
--                         -- valid range for tx is created with tpDepositDate as date, and then subs and adds hald of valid time
--                         -- (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) + 1) (date' + LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTimeRange `divide` 2) -1)
--                         -- so if we set in redeemer the date (tpDepositDate tp+T.validTimeRange ) it must fail
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_NoDeposits_MockData tp, CampaignFundsT.mkDepositRedeemer (tpDepositDate tp + T.validTimeRange) deposit_MockData)]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isDateInRange"]
--                 , Tasty.testCase "Depositing with invalid range must fail" $ do
--                     let
--                         ctx' = ctx |> setValidyRange (createInValidRange (tpDepositDate tp))
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isValidRange"]
--                 ]

-- --------------------------------------------------------------------------------

-- campaignFunds_Validator_Redeemer_Withdraw_Tests :: GHC.HasCallStack => TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_Withdraw_Tests tp =
--     let
--         ------------------------
--         txName = show Campaign_Withdraw_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Withdraw_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) withdraw_MockData
--             in
--                 [ Tasty.testCase "Withdrawing correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Withdrawing without burning FT must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setMintAndAddRedeemers mempty
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isBurningFT"]
--                 , Tasty.testCase "Withdrawing without updating FT minted subtotal must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Withdraw_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Minted =
--                                     CampaignFundsT.hdSubtotal_FT_Minted (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Withdraw_MockData tp)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_With_Withdraw"]
--                 , Tasty.testCase "Withdrawing without user recovering commissions must fail" $ do
--                     let
--                         !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
--                         (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Withdraw_MockData tp)
--                                 { LedgerApiV2.txOutValue =
--                                     LedgerApiV2.txOutValue (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                         -- <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
--                                         <> LedgerApiV2.singleton investUnit_Initial_Token_CS investUnit_Initial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnit_Initial_Token_Amount) `divide` 100))
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_Without_Tokens_And_FT_for_Commissions"]
--                 ]

-- --------------------------------------------------------------------------------

-- campaignFunds_Validator_Redeemer_Collect_Protocol_Commission_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_Collect_Protocol_Commission_Tests tp =
--     let
--         ------------------------
--         txName = show CampaignFunds_Collect_Protocol_Commission_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Collect_Protocol_Commission_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_Collect_Protocol_Commission_TxContext tp
--             in
--                 [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Commissions =
--                                     CampaignFundsT.hdSubtotal_FT_Commissions (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_With_Collect_Protocol_Commission"]
--                 , Tasty.testCase "Collecting commissions without updating CampaignFunds value must fail" $ do
--                     let
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutValue =
--                                     LedgerApiV2.txOutValue (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_Without_FT_for_Commissions"]
--                 , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
--                     let
--                         available = CampaignHelpers.getCommissionsAvailable (tpDeadline tp) (campaignFunds_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Protocol tp) (CampaignFundsT.hdSubtotal_FT_Commissions_Collected_Protocol (campaignFunds_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
--                         withdraw_Commissions_MockData' = available + 1
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
--                                 |> setOutputs [campaignFunds_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData']
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
--                 , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setSignatories []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
--                 , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setValidyRange (Ledger.interval 0 300_000)
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isDateInRange"]
--                 ]

-- campaignFunds_Validator_Redeemer_Collect_Managers_Commission_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_Collect_Managers_Commission_Tests tp =
--     let
--         ------------------------
--         txName = show CampaignFunds_Collect_Managers_Commission_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Collect_Managers_Commission_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_Collect_Managers_Commission_TxContext tp
--             in
--                 [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Commissions =
--                                     CampaignFundsT.hdSubtotal_FT_Commissions (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_With_Collect_Managers_Commission"]
--                 , Tasty.testCase "Collecting commissions without updating CampaignFunds value must fail" $ do
--                     let
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutValue =
--                                     LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
--                                         <> LedgerApiV2.singleton T.exampleCS T.exampleTN 300
--                                         <> LedgerApiV2.singleton (tpCampaignFundsPolicyID_CS tp) (mkCampaignFundsID_TN 0) 1
--                                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) 200
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_Without_FT_for_Commissions"]
--                 , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
--                     let
--                         available = CampaignHelpers.getCommissionsAvailable (tpDeadline tp) (campaignFunds_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Managers tp) (CampaignFundsT.hdSubtotal_FT_Commissions_Collected_Managers (campaignFunds_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
--                         withdraw_Commissions_MockData' = available + 1
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
--                                 |> setOutputs [campaignFunds_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData']
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
--                 , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setSignatories []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
--                 , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setValidyRange (Ledger.interval 0 300_000)
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isDateInRange"]
--                 ]

-- campaignFunds_Validator_Redeemer_Collect_Delegators_Commission_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_Collect_Delegators_Commission_Tests tp =
--     let
--         ------------------------
--         txName = show CampaignFunds_Collect_Delegators_Commission_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Collect_Delegators_Commission_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_Collect_Delegators_Commission_TxContext tp
--             in
--                 [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Commissions =
--                                     CampaignFundsT.hdSubtotal_FT_Commissions (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_With_Collect_Delegators_Commission"]
--                 , Tasty.testCase "Collecting commissions without updating CampaignFunds value must fail" $ do
--                     let
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
--                                 { LedgerApiV2.txOutValue =
--                                     LedgerApiV2.txOutValue (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_Without_FT_for_Commissions"]
--                 , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
--                     let
--                         available = CampaignHelpers.getCommissionsAvailable (tpDeadline tp) (campaignFunds_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Delegators tp) (CampaignFundsT.hdSubtotal_FT_Commissions_Collected_Delegators (campaignFunds_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
--                         withdraw_Commissions_MockData' = available + 1
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
--                                 |> setOutputs [campaignFunds_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData']
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
--                 , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setSignatories []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isSignedByAny delegatorsAdmins"]
--                 , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setValidyRange (Ledger.interval 0 300_000)
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isDateInRange"]
--                 ]

-- --------------------------------------------------------------------------------

-- campaignFunds_Validator_Redeemer_ReIndexing_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_ReIndexing_Tests tp =
--     let
--         ------------------------
--         txName = show Campaign_ReIndexing_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_ReIndexing_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_ReIndexing_TxContext tp
--             in
--                 [ Tasty.testCase "Re-index correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Not including protocol input ref must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setInputsRef []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
--                 , Tasty.testCase "Not including some CampaignFunds as ref must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setInputsRef [fund_UTxO_MockData tp]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["expected all but one CampaignFunds as input ref"]
--                 , Tasty.testCase "Not including invest unit input must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers
--                                     [ (campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
--                                     ]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["Expected exactly one InvestUnit input"]
--                 , Tasty.testCase "Not having a CampaignFunds output must fail" $ do
--                     let
--                         -- NOTE: pongo a la fuerza cualquier output, para que no falle por no tener outputs
--                         ctx' =
--                             ctx
--                                 |> setOutputs [protocol_UTxO_MockData tp]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["Expected CampaignFunds at output index 0"]
--                 , Tasty.testCase "Updating CampaignFunds must fail" $ do
--                     let
--                         modifiedDatumType =
--                             (campaignFunds_DatumType_With_Deposits_MockData tp)
--                                 { CampaignFundsT.hdSubtotal_FT_Commissions =
--                                     CampaignFundsT.hdSubtotal_FT_Commissions (campaignFunds_DatumType_With_Deposits_MockData tp) + 1
--                                 }
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
--                                 { LedgerApiV2.txOutDatum =
--                                     LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum_NotChanged"]
--                 , Tasty.testCase "Not updating the CampaignFunds value must fail" $ do
--                     let
--                         modifiedUTxO =
--                             (campaignFunds_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
--                                 { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (campaignFunds_UTxO_With_Deposits_MockData tp)
--                                 }
--                         ctx' =
--                             ctx
--                                 |> setOutputs [modifiedUTxO]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value_WithTokensExchanged"]
--                 , Tasty.testCase "Having an incorrect redeemer for the Invest Unit validator must fail" $ do
--                     let
--                         ctx' =
--                             ctx
--                                 |> setInputsAndAddRedeemers
--                                     [ (campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
--                                     , (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer (T.InvestUnit [("cc", "name", 0)]) (T.InvestUnit [("cc", "name", 0)]) (oracleReIdxData tp) (oracleReIdxSignature tp))
--                                     ]
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Redeemer_InvestUnit"]
--                 ]

-- --------------------------------------------------------------------------------

-- campaignFunds_Validator_Redeemer_BalanceAssets_Tests :: TestParams -> Tasty.TestTree
-- campaignFunds_Validator_Redeemer_BalanceAssets_Tests tp =
--     let
--         ------------------------
--         txName = show CampaignFunds_BalanceAssets_Tx
--         selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_BalanceAssets_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--     in
--         ------------------------

--         ------------------------
--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--             let
--                 ctx = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [0, 0] False []
--             in
--                 [ Tasty.testCase "Balancing assets correctly must succeed" $ do
--                     let
--                         ctx' = ctx
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing assets incorrect total must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -99] [0, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Values_SameTotal"]
--                 , Tasty.testCase "Balancing assets from fst input with zero deposits to other correctly must fail" $ do
--                     -- para el calculo y control del movimiento de comisiones, se necesita un deposito previo en la primer input
--                     -- es una restriccion de simplificacion y siempre se puede armar la misma tx usando la primer input como segunda y viceversa
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [0, deposit_MockData] [100, -100] [0, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["Expected oldCommissions firts input to be greater than 0"]
--                 , Tasty.testCase "Balancing assets from fst input with depositos to other with zero correctly must succeed" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, 0] [-100, 100] [0, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing commissions FT correctly must succeed" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [0, 0] [1000000, -1000000] False []
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing deposits and commissions FT correctly release must succeed" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] False []
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing deposits and commissions FT using correct custom release per month must succeed" $ do
--                     let
--                         -- con deposit_MockData se crea un datum con
--                         -- comission FT = 1991200
--                         -- rate = 221244444444
--                         -- con un cambio de [1000000, -1000000]
--                         -- deberia generarse un cambio [332355555554, 110133333334]
--                         -- eso lo saco del caso de testeo anterior, usando testContextWrapperTrace y leyendo el contexto.
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555554, 110133333334]
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing ALL deposits and ALL commissions FT correctly release must succeed" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData, deposit_MockData] [-1991200, 1991200] False []
--                     results <- testContextWrapper tp ctx'
--                     (Nothing, results)
--                         `assertResultsContainAnyOf` []
--                 -- TODO: deberia fallar pero no lo hace por que mi test context no verifica que ningun valor en value sea menor a cero. 
--                 -- deberia agregar ese control en el test context
--                 -- , Tasty.testCase "Balancing a LITLE MORE deposits and ALL commissions FT correctly release must fail" $ do
--                 --     let
--                 --         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData - 100, deposit_MockData + 100] [-1991200, 1991200] False []
--                 --     results <- testContextWrapperTrace tp ctx'
--                 --     (Nothing, results)
--                 --         `assertResultsContainAnyOf` []
--                 , Tasty.testCase "Balancing a ALL deposits and LITLE MORE commissions FT correctly release must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData, deposit_MockData] [-1991200 - 1, 1991200 + 1] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect newCommissions >=0 and newRelease >=0"]
--                 , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 1 per month must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555553, 110133333334]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Outputs_CampaignFunds_Datums_With_UpdatedCommissionsAndRate"]
--                 , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 2 per month must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555554, 110133333333]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Outputs_CampaignFunds_Datums_With_UpdatedCommissionsAndRate"]
--                 , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 3 per month must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000001, -1000001] True [332355555554, 110133333334]
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Outputs_CampaignFunds_Datums_With_UpdatedCommissionsAndRate"]
--                 , Tasty.testCase "Incorrect redeemer with more items in commissions list must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0, 0, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not len alterCommissionsFT == cantOutputs"]
--                 , Tasty.testCase "Incorrect redeemer with less items in commissions list invalid must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not len alterCommissionsFT == cantOutputs"]
--                 , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (plus zero) must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [1, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Outputs_Commissions_SameTotal"]
--                 , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (less zero) must fail" $ do
--                     let
--                         ctx' = campaignFunds_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [-1, 0] False []
--                     results <- testContextWrapper tp ctx'
--                     (Just selectedRedeemer, results)
--                         `assertResultsContainAnyOf` ["not isCorrect_Outputs_Commissions_SameTotal"]
--                 ]

--------------------------------------------------------------------------------

campaignFunds_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsDelete_Tx
        selectedRedeemer = RedeemerLogValidator (Just CampaignFunds_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = campaignFunds_Delete_TxContext tp
            in
                [ Tasty.testCase "Delete CampaignFunds correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                -- , Tasty.testCase "Not empty CampaignFunds value must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setInputsAndAddRedeemers
                --                     [ (fund_UTxO_With_Added_CampaignFunds_MockData tp, CampaignT.mkCampaignFundsDeleteRedeemer)
                --                     , (campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkDeleteRedeemer)
                --                     ]
                --     results <- testContextWrapper tp ctx'
                --     (Just (RedeemerLogPolicy (Just CampaignFunds_BurnID_TestRedeemer)), results)
                --         `assertResultsContainAnyOf` ["not isZeroAssets"]
                -- , Tasty.testCase "Not including fund admin sign must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setSignatories []
                --     results <- testContextWrapper tp ctx'
                --     (Just (RedeemerLogValidator (Just Campaign_CampaignFundsDelete_TestRedeemer)), results)
                --         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                -- , Tasty.testCase "Not including Fund UTXO as input must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_NoDeposits_MockData tp, CampaignFundsT.mkDeleteRedeemer)]
                --     results <- testContextWrapper tp ctx'
                --     (Just (RedeemerLogPolicy (Just CampaignFunds_BurnID_TestRedeemer)), results)
                --         `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                -- , Tasty.testCase "Not including Fund UTXO as output must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setOutputs [protocol_UTxO_MockData tp]
                --     -- NOTE: necesito agregar alguna output para que no falle por no tener outputs
                --     results <- testContextWrapper tp ctx'
                --     (Just selectedRedeemer, results)
                --         `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                -- , Tasty.testCase "Not burning CampaignFunds ID must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setMintAndAddRedeemers mempty
                --     results <- testContextWrapper tp ctx'
                --     (Just selectedRedeemer, results)
                --         `assertResultsContainAnyOf` ["not isBurningCampaignFundsID"]
                -- , Tasty.testCase "Too big range must fail" $ do
                --     let
                --         ctx' =
                --             ctx
                --                 |> setValidyRange (createInValidRange (tpTransactionDate tp))
                --     results <- testContextWrapper tp ctx'
                --     (Just selectedRedeemer, results)
                --         `assertResultsContainAnyOf` ["not isValidRange"]
                ]

--------------------------------------------------------------------------------
