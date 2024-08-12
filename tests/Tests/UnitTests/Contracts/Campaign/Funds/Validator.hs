{- |
Module      : Fund.Holding.Validator
Description : Validation logic and tests related to the Campaign Funds module.

This module defines the validation logic for the Campaign Funds contract.

It includes multiple test cases to ensure the integrity and correctness of the
validation script.
-}
module Contracts.Campaign.Funds.Validator where

-- Non-IOG imports
import qualified Test.Tasty                    as Tasty
import qualified Test.Tasty.HUnit              as Tasty

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                    as LedgerAda
import qualified Ledger.Address                as LedgerAddress
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import           PlutusTx.Prelude              (divide)
import           Prelude

-- Project imports

import qualified Campaign.Funds.Types          as CampaignFundsT
import qualified Campaign.Helpers              as CampaignHelpers
import           Contracts.Campaign.Funds.Data
import qualified Debug.Trace                   as DebugTrace
import qualified Helpers.OffChain              as OffChainHelpers (addressValidator)
import qualified Helpers.OnChain               as OnChainHelpers
import qualified Plutus.V2.Ledger.Contexts     as LedgerContextsV2
import qualified Protocol.InvestUnit.Types     as InvestUnitT
import qualified Protocol.PABTypes             as T
import           TestUtils.Common
import           TestUtils.Constants
import           TestUtils.InvestUnit
import           TestUtils.Types
import qualified Types                         as T

-- | Suite of tests validating the logic of the '(T.tpCampaignFundsValidator tp)'.
campaignFundsValidatorTests :: T.TestParams -> Tasty.TestTree
campaignFundsValidatorTests tp =
    Tasty.testGroup
        "Testing Campaign Funds Validator"
        [ depositTests tp
        , withdrawTests tp
        , collect_Protocol_Commissions_CommissionsTests tp
        , collect_Managers_Commissions_CommissionsTests tp
        , collect_Delegators_Commissions_Tests tp
        , reIndexTests tp
        , deleteTests tp
        ]

--------------------------------------------------------------------------------
-- DEPOSIT IN THE FUND
--------------------------------------------------------------------------------

-- | Test group for depositing in the fund.
depositTests :: T.TestParams -> Tasty.TestTree
depositTests tp =
    Tasty.testGroup
        "Deposit in the fund tests"
        [ Tasty.testCase "Depositing successful case" $
            let
                ctx = depositContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp) deposit_MockData)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase "Depositing without minting FT should fail" $
            let
                ctx = depositContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp) deposit_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingFT"]
        , Tasty.testCase "Depositing without increasing FT minted subtotal should fail" $
            let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Deposit_MockData tp)
                        { CampaignFundsT.cfdSubtotal_Sold_FT  =
                            CampaignFundsT.cfdSubtotal_Sold_FT  (fundHoldingDatumType_MockData tp)
                        }
                modifiedUTxO =
                    (fundHolding_UTxO_With_Deposit_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = depositContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp) deposit_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Updated_With_Deposit"]
        , Tasty.testCase "Depositing without paying invest units should fail" $
            let
                (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (T.tpDepositDate tp) deposit_MockData
                modifiedUTxO =
                    (fundHolding_UTxO_With_Deposit_MockData tp)
                        { LedgerApiV2.txOutValue =
                            LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) 1
                                <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) commissionsFT_MockData
                        }
                ctx = depositContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp) deposit_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Value_WithTokensAndFT"]
        , Tasty.testCase "Depositing date outside valid range should fail" $
            let
                ctx = depositContext tp
            in
                -- valid range for tx is created with tpDepositDate tp-1 to tpDepositDate tp+1
                -- so if we set in redeemer the date (T.tpDepositDate tp+10) it should fail

                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp + 10) deposit_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isDateInRange"]
        , Tasty.testCase "Depositing with invalid range should fail" $
            let
                ctx = depositContext tp OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createInValidRange (T.tpDepositDate tp))
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (campaignFundsDatum_MockData tp)
                    (CampaignFundsT.mkDepositRedeemer (T.tpDepositDate tp) deposit_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isValidRange"]
        ]

--------------------------------------------------------------------------------
-- WITHDRAW FROM THE FUND
--------------------------------------------------------------------------------

-- | Test group for withdrawing from the fund.
withdrawTests :: T.TestParams -> Tasty.TestTree
withdrawTests tp =
    Tasty.testGroup
        "Withdraw from the fund tests"
        [ Tasty.testCase "Withdrawing successful case" $
            let
                ctx = withdrawContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkWithdrawRedeemer (T.tpWithdrawDate tp) withdraw_MockData)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase "Withdrawing without burning FT should fail" $
            let
                ctx = withdrawContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkWithdrawRedeemer (T.tpWithdrawDate tp) withdraw_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningFT"]
        , Tasty.testCase "Withdrawing without updating FT minted subtotal should fail" $
            let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Withdraw_MockData tp)
                        { CampaignFundsT.cfdSubtotal_Sold_FT  =
                            CampaignFundsT.cfdSubtotal_Sold_FT  (fundHolding_DatumType_With_Deposit_MockData tp)
                        }
                modifiedUTxO =
                    (fundHolding_UTxO_With_Withdraw_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = withdrawContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkWithdrawRedeemer (T.tpWithdrawDate tp) withdraw_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Updated_With_Withdraw"]
        , Tasty.testCase "Withdrawing without user recovering commissions should fail" $
            let
                (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (T.tpWithdrawDate tp) withdraw_MockData
                modifiedUTxO =
                    (fundHolding_UTxO_With_Withdraw_MockData tp)
                        { LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                                -- <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
                                <> LedgerApiV2.singleton investUnitInitial_Token_CS investUnitInitial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnitInitial_Token_Amount) `divide` 100))
                        }
                ctx = withdrawContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkWithdrawRedeemer (T.tpWithdrawDate tp) withdraw_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Value_Without_Tokens_And_FT_for_Commissions"]
        ]

--------------------------------------------------------------------------------
-- COLLECT COMMISSIONS
--------------------------------------------------------------------------------

-- | Test group for collecting commissions by Protocol admins.
collect_Protocol_Commissions_CommissionsTests :: T.TestParams -> Tasty.TestTree
collect_Protocol_Commissions_CommissionsTests tp =
    Tasty.testGroup
        "Collect commissions by Protocol admin tests"
        [ Tasty.testCase "Collecting commissions successful case" $
            let
                ctx = collect_Protocol_Commissions_Context tp
            in
                -- DebugTrace.trace (show (fundHolding_DatumType_With_Deposit_MockData tp)) $
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase
            "Collecting commissions without updating remaining commissions should fail"
            $ let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Deposit_MockData tp)
                        { CampaignFundsT.hdSubtotal_FT_ForComission =
                            CampaignFundsT.hdSubtotal_FT_ForComission (fundHolding_DatumType_With_Deposit_MockData tp)
                        }
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = collect_Protocol_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
              in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Updated_With_Collect_Protocol_Commission"
                              ]
        , Tasty.testCase "Collecting commissions without updating Campaign Funds value should fail" $
            let
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                        }
                ctx = collect_Protocol_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Value_Without_FT_for_Commissions"
                              ]
        , Tasty.testCase "Trying to collect more commissions than the available should fail" $
            let
                available = CampaignHelpers.getCommissionsAvailable (T.tpDeadline tp) (fundHolding_DatumType_With_Deposit_MockData tp) (T.tpShare_InBP1e2_Protocol tp) (CampaignFundsT.hdSubtotal_Collected_Commissions_Protocol (fundHolding_DatumType_With_Deposit_MockData tp)) (T.tpCollectCommissionsDate tp)
                withdraw_Commissions_MockData' = available + 1
                ctx = collect_Protocol_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData']
            in
                -- DebugTrace.trace ("Commissions available:" ++ show available) $
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData')
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCommissionsAvailable"]
        , Tasty.testCase
            "Trying to send collected commissions to a random user without adding an admin signatory should fail"
            $ let
                ctx =
                    collect_Protocol_Commissions_Context tp
                        OffChainEval.|> OffChainEval.setSignatories []
                        OffChainEval.|> OffChainEval.setOutputs [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData]
              in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
        , Tasty.testCase "Collecting commissions outside valid range should fail" $
            let
                ctx =
                    collect_Protocol_Commissions_Context tp
                        OffChainEval.|> OffChainEval.setValidRange (Ledger.interval 0 300_000)
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkSellRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isDateInRange"]
        ]

-- | Test group for collecting commissions by Fund admins.
collect_Managers_Commissions_CommissionsTests :: T.TestParams -> Tasty.TestTree
collect_Managers_Commissions_CommissionsTests tp =
    Tasty.testGroup
        "Collect commissions by Managers tests"
        [ Tasty.testCase "Collecting commissions successful case" $
            let
                ctx = collect_Managers_Commissions_Context tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase
            "Collecting commissions without updating remaining commissions should fail"
            $ let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Deposit_MockData tp)
                        { CampaignFundsT.hdSubtotal_FT_ForComission =
                            CampaignFundsT.hdSubtotal_FT_ForComission (fundHolding_DatumType_With_Deposit_MockData tp)
                        }
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = collect_Managers_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
              in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Updated_With_Collect_Managers_Commission"
                              ]
        , Tasty.testCase "Collecting commissions without updating Campaign Funds value should fail" $
            let
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutValue =
                            LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                                <> LedgerApiV2.singleton T.exampleCS T.exampleTN 300
                                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) 1
                                <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) 200
                        }
                ctx = collect_Managers_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Value_Without_FT_for_Commissions"
                              ]
        , Tasty.testCase "Trying to collect more commissions than the available should fail" $
            let
                available = CampaignHelpers.getCommissionsAvailable (T.tpDeadline tp) (fundHolding_DatumType_With_Deposit_MockData tp) (T.tpShare_InBP1e2_Managers tp) (CampaignFundsT.hdSubtotal_Collected_Commissions_Managers (fundHolding_DatumType_With_Deposit_MockData tp)) (T.tpCollectCommissionsDate tp)
                withdraw_Commissions_MockData' = available + 1
                ctx = collect_Managers_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData']
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData')
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCommissionsAvailable"]
        , Tasty.testCase
            "Trying to send collected commissions to a random user without adding an admin signatory should fail"
            $ let
                randomUserUTxO =
                    LedgerApiV2.TxOut
                        (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "aa") Nothing)
                        ( LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens
                            <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) withdraw_Commissions_MockData
                        )
                        LedgerApiV2.NoOutputDatum
                        Nothing
                ctx =
                    collect_Managers_Commissions_Context tp
                        OffChainEval.|> OffChainEval.setSignatories []
                        OffChainEval.|> OffChainEval.setOutputs [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData, randomUserUTxO]
              in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
        , Tasty.testCase "Collecting commissions outside valid range should fail" $
            let
                ctx = collect_Managers_Commissions_Context tp OffChainEval.|> OffChainEval.setValidRange (Ledger.interval 0 300_000)
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkCollectRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isDateInRange"]
        ]

-- | Test group for collecting commissions by Delegators.
collect_Delegators_Commissions_Tests :: T.TestParams -> Tasty.TestTree
collect_Delegators_Commissions_Tests tp =
    Tasty.testGroup
        "Collect commissions by Delegators tests"
        [ Tasty.testCase "Collecting commissions successful case" $
            let
                ctx = collect_Delegators_Commissions_Context tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkGetBackRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase
            "Collecting commissions without updating remaining commissions should fail"
            $ let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Deposit_MockData tp)
                        { CampaignFundsT.hdSubtotal_FT_ForComission =
                            CampaignFundsT.hdSubtotal_FT_ForComission (fundHolding_DatumType_With_Deposit_MockData tp)
                        }
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = collect_Delegators_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
              in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkGetBackRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Updated_With_Collect_Delegators_Commission"
                              ]
        , Tasty.testCase "Collecting commissions without updating Campaign Funds value should fail" $
            let
                modifiedUTxO =
                    (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                        { LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                        }
                ctx = collect_Delegators_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkGetBackRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    Tasty.@?= [ "not isCorrect_Output_CampaignFundsDatum_Value_Without_FT_for_Commissions"
                              ]
        , Tasty.testCase "Trying to collect more commissions than the available should fail" $
            let
                available = CampaignHelpers.getCommissionsAvailable (T.tpDeadline tp) (fundHolding_DatumType_With_Deposit_MockData tp) (T.tpShare_InBP1e2_Delegators tp) (CampaignFundsT.hdSubtotal_Collected_Commissions_Delegators (fundHolding_DatumType_With_Deposit_MockData tp)) (T.tpCollectCommissionsDate tp)
                withdraw_Commissions_MockData' = available + 1
                ctx = collect_Delegators_Commissions_Context tp OffChainEval.|> OffChainEval.setOutputs [fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData']
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkGetBackRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData')
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCommissionsAvailable"]
        , Tasty.testCase "Collecting commissions outside valid range should fail" $
            let
                ctx = collect_Delegators_Commissions_Context tp OffChainEval.|> OffChainEval.setValidRange (Ledger.interval 0 300_000)
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkGetBackRedeemer (T.tpCollectCommissionsDate tp) withdraw_Commissions_MockData)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isDateInRange"]
        ]

--------------------------------------------------------------------------------
-- RE-INDEXING THE FUND
--------------------------------------------------------------------------------

-- | Test group for re-indexing the fund.
reIndexTests :: T.TestParams -> Tasty.TestTree
reIndexTests tp =
    Tasty.testGroup
        "Re-index fund tests"
        [ Tasty.testCase "Re-index successful case" $
            let
                ctx = reIndexContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    Tasty.@?= []
        , Tasty.testCase "Not including protocol input ref should fail" $
            let
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setRefInputs []
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Campaign input ref"]
        , Tasty.testCase "Not including some holding as ref should fail" $
            let
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setRefInputs [campaignUTxO_MockData tp]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["expected all but one CampaignFunds as input ref"]
        , Tasty.testCase "Not including invest unit input should fail" $
            let
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setInputs [fundHolding_UTxO_With_Deposit_MockData tp]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one InvestUnit input"]
        , Tasty.testCase "Not having a Campaign Funds output should fail" $
            let
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setOutputs [protocolUTxO_MockData tp] -- NOTE: pongo a la fuerza cualquier output, para que no falle por no tener outputs
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected CampaignFunds at output index 0"]
        , Tasty.testCase "Updating Campaign Funds datum should fail" $
            let
                modifiedDatumType =
                    (fundHolding_DatumType_With_Deposit_MockData tp)
                        { CampaignFundsT.hdSubtotal_FT_Circulation =
                            CampaignFundsT.hdSubtotal_FT_Circulation (fundHolding_DatumType_With_Deposit_MockData tp) + 1
                        }
                modifiedUTxO =
                    (output_ReIdx_CampaignFundsUTxO_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum modifiedDatumType
                        }
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_NotChanged"]
        , Tasty.testCase "Not updating the holding value should fail" $
            let
                modifiedUTxO =
                    (output_ReIdx_CampaignFundsUTxO_MockData tp)
                        { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                        }
                ctx = reIndexContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Value_WithTokensExchanged"]
        , Tasty.testCase "Having an incorrect redeemer for the Invest Unit validator should fail" $
            let
                ctx =
                    reIndexContext tp
                        OffChainEval.|> setInfoRedeemers
                            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 1]
                            [ InvestUnitT.mkReIndexingRedeemer
                                (T.InvestUnit [("cc", "name", 0)])
                                (T.InvestUnit [("cc", "name", 0)])
                                (oracleData tp)
                                (oracleSignature tp)
                            ]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    (CampaignFundsT.mkReIndexingRedeemer investUnitAfterReIdx investUnitInitial)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Redeemer_InvestUnitDatum"]
        ]

--------------------------------------------------------------------------------
-- DELETE Campaign Funds
--------------------------------------------------------------------------------

-- | Test group for deleting a Campaign Funds.
deleteTests :: T.TestParams -> Tasty.TestTree
deleteTests tp =
    Tasty.testGroup
        "Delete Campaign Funds tests"
        [ Tasty.testCase "Delete Campaign Funds successful case" $
            let
                ctx = deleteContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    CampaignFundsT.mkDeleteRedeemer
                    ctx
                    Tasty.@?= []
        -- , Tasty.testCase "Not including fund admin sign should fail" $
        --     let
        --         ctx = deleteContext tp OffChainEval.|> OffChainEval.setSignatories []
        --     in
        --         OffChainEval.evaluateScriptValidatorEX
        --             (T.tpCampaignFundsValidator tp)
        --             (fundHolding_Datum_With_Deposit_MockData tp)
        --             CampaignFundsT.mkDeleteRedeemer
        --             ctx
        --             `OffChainEval.logAassertContainsAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
        , Tasty.testCase "Not including fund utxo as output should fail" $
            let
                ctx = deleteContext tp OffChainEval.|> OffChainEval.setOutputs [protocolUTxO_MockData tp] -- NOTE: necesito agregar alguna output para que no falle por no tener outputs
             in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    CampaignFundsT.mkDeleteRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected Campaign at output index 0"]
        , Tasty.testCase "Not burning CampaignFunds ID should fail" $
            let
                ctx = deleteContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    CampaignFundsT.mkDeleteRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningCampaignFundsID"]
        , Tasty.testCase "Too big range should fail" $
            let
                ctx = deleteContext tp OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createInValidRange (T.tpTransactionDate tp))
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignFundsValidator tp)
                    (fundHolding_Datum_With_Deposit_MockData tp)
                    CampaignFundsT.mkDeleteRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isValidRange"]
        ]
