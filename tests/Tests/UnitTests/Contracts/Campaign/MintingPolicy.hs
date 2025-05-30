--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Fund.MintingPolicy
Description : Validation logic and tests related to the Campaign minting policy.

This module defines the validation logic for the Fund's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Campaign.MintingPolicy where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import           Prelude                                (show)
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.HUnit                       as Tasty

-- IOG imports
import           PlutusTx.Prelude                       as PTx

-- Project imports
import           TestUtils.Contracts.TxContext.Campaign
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Asserts
import           TestUtils.TypesINNOVATIO

--------------------------------------------------------------------------------

campaign_Policy_Tests :: TestParams -> Tasty.TestTree
campaign_Policy_Tests tp =
    Tasty.testGroup
        "Campaign Policy Tests"
        [
             campaign_Policy_Redeemer_MintID_Tests tp
            , campaign_Policy_Redeemer_BurnID_Tests tp
        --    , campaign_Policy_Redeemer_MintCampaignToken_Tests tp
        --    , campaign_Policy_Redeemer_BurnCampaignToken_Tests tp
        ]

--------------------------------------------------------------------------------

campaign_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
campaign_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show Campaign_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Campaign_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Not including Protocol input ref must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setInputsRef []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected exactly one Protocol input ref"]
                        -- , Tasty.testCase "Not including two output must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected at least two outputs to script addresses"]
                        -- -- , Tasty.testCase "Not including Campaign output must fail" $ do
                        -- --     let ctx' = ctx
                        -- --                 |> setOutputs [investUnit_UTxO_MockData tp, investUnit_UTxO_MockData tp]
                        -- --     results <- testContextWrapper tp ctx'
                        -- --     (Just selectedRedeemer, results)
                        -- --         `assertResultsContainAnyOf` ["Expected Campaign at output index 0"]
                        -- , Tasty.testCase "Not including invest unit output must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, campaign_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                        -- , Tasty.testCase "Not including utxo in policy parameter as input must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setInputsAndAddRedeemers []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isTxOutAnInput"]
                        -- , Tasty.testCase "Minting a different amount Campaign ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 2
                        --                         , CampaignT.mkMintIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isMintingIDs"]
                        -- , Tasty.testCase "Minting an extra asset with wrong token name must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 1
                        --                             <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                        --                         , CampaignT.mkMintIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isMintingIDs"]
                        -- , Tasty.testCase "Minting an asset with wrong token name must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                        --                         , CampaignT.mkMintIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isMintingIDs"]
                        -- , Tasty.testCase "Campaign utxo address different from the one in policy param must fail" $ do
                        --     let campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutAddress =
                        --                     OffChainHelpers.addressValidator $
                        --                         LedgerApiV2.ValidatorHash
                        --                             "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected Campaign at output index 0"]
                        -- , Tasty.testCase "IU utxo address different from the one in policy param must fail" $ do
                        --     let investUnit_UTxO_MockData' =
                        --             (investUnit_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutAddress =
                        --                     OffChainHelpers.addressValidator $
                        --                         LedgerApiV2.ValidatorHash
                        --                             "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, investUnit_UTxO_MockData']
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                        -- , Tasty.testCase "Campaign with wrong Commissions in datum must fail" $ do
                        --     let campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdCommission_PerYear_InBPx1e3 = ProtocolT.mmdMax (ProtocolT.pdCommissionCampaign_PerYear_InBPx1e3 (protocol_DatumType_MockData tp)) + sum_ANY_INVALID_NUMBER
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isInRange commissionCampaign_PerYear_InBPx1e3"]
                        -- , Tasty.testCase "Campaign with wrong Commissions Table values in datum must fail" $ do
                        --     let beginAt = CampaignT.fdBeginAt (campaign_DatumType_MockData tp)
                        --         deadline = CampaignT.fdDeadline (campaign_DatumType_MockData tp)
                        --         commission_PerYear_InBPx1e3 = CampaignT.fdCommission_PerYear_InBPx1e3 (campaign_DatumType_MockData tp) + sum_ANY_INVALID_NUMBER
                        --         monthsRemainingPlusOne = CampaignHelpers.getRemainingMonths deadline beginAt + 1
                        --         den = 120_000_000
                        --         commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]
                        --         ----------------------
                        --         campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_CommissionsTable"]
                        -- , Tasty.testCase "Campaign with wrong Commissions Table lenght in datum must fail" $ do
                        --     let ----------------------
                        --         beginAt = CampaignT.fdBeginAt (campaign_DatumType_MockData tp)
                        --         deadline = CampaignT.fdDeadline (campaign_DatumType_MockData tp)
                        --         commission_PerYear_InBPx1e3 = CampaignT.fdCommission_PerYear_InBPx1e3 (campaign_DatumType_MockData tp)
                        --         monthsRemaining_ = CampaignHelpers.getRemainingMonths deadline beginAt
                        --         den = 120_000_000
                        --         commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemaining_]]
                        --         ----------------------
                        --         campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdCommissions_Table_Numerator_1e6 = sum_ANY_INVALID_NUMBER : commissions_Table_Numerator_1e6
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_CommissionsTable"]
                        -- , Tasty.testCase "Campaign with wrong Category in datum must fail" $ do
                        --     let campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdFundCategoryNumber = ProtocolT.fcCategoryNumber (tpFundCategory tp) + sum_ANY_INVALID_NUMBER
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Can't find Campaign Category"]
                        -- , Tasty.testCase "Having initial funds count different from 0 must fail" $ do
                        --     let campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdFundsCount = 1
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum"]
                        -- , Tasty.testCase "Having initial wrong deadline must fail" $ do
                        --     let ----------------------
                        --         beginAt = CampaignT.fdBeginAt (campaign_DatumType_MockData tp)
                        --         deadline = tpTransactionDate tp - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        --         commission_PerYear_InBPx1e3 = CampaignT.fdCommission_PerYear_InBPx1e3 (campaign_DatumType_MockData tp)
                        --         monthsRemaining_ = CampaignHelpers.getRemainingMonths deadline beginAt
                        --         den = 120_000_000
                        --         commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemaining_]]
                        --         ----------------------
                        --         campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdDeadline = deadline
                        --                 , CampaignT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isDateNotReached deadline"]
                        -- , Tasty.testCase "Having initial deadline < beginAt must fail" $ do
                        --     let ----------------------
                        --         beginAt = tpTransactionDate tp + 100
                        --         deadline = beginAt - 1
                        --         commission_PerYear_InBPx1e3 = CampaignT.fdCommission_PerYear_InBPx1e3 (campaign_DatumType_MockData tp)
                        --         monthsRemaining_ = CampaignHelpers.getRemainingMonths deadline beginAt
                        --         den = 120_000_000
                        --         commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemaining_]]
                        --         ----------------------
                        --         campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdBeginAt = beginAt
                        --                 , CampaignT.fdDeadline = deadline
                        --                 , CampaignT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not deadline > beginAt"]
                        -- , Tasty.testCase "Having initial wrong Campaign lifetime must fail" $ do
                        --     let fundLifeTime = ProtocolT.mmdMax $ ProtocolT.pdFundLifeTime $ protocol_DatumType_MockData tp
                        --         campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdDeadline = CampaignT.fdBeginAt (campaign_DatumType_MockData tp) + fundLifeTime + LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isInRange fundLifeTime"]
                        -- , Tasty.testCase "Campaign with wrong MAYZ in datum must fail" $ do
                        --     let campaign_DatumType' =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.fdMAYZ = ProtocolT.fcRequiredMAYZ (tpFundCategory tp) + sum_ANY_INVALID_NUMBER
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum"]
                        -- , Tasty.testCase "MAYZ in Campaign output value not according to FundCategory of datum must fail" $ do
                        --     let campaign_UTxO_MockData' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaCampaignDatum
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 1
                        --                         <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ (tpFundCategory tp) + sum_ANY_INVALID_NUMBER)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not currentMAYZ == requiredMAYZ"]
                        -- , Tasty.testCase "Campaign output value without Campaign ID must fail" $ do
                        --     let campaign_UTxO_MockData'' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaCampaignDatum
                        --                         <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData'', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected Campaign at output index 0"]
                        -- , Tasty.testCase "Campaign output value with Campaign ID other token name must fail" $ do
                        --     let campaign_UTxO_MockData'' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaCampaignDatum
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) "OTHER NAME" 1
                        --                         <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData'', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected Campaign at output index 0"]
                        -- , Tasty.testCase "Campaign output value without minAda specified in datum must fail" $ do
                        --     let campaign_UTxO_MockData'' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 1
                        --                         <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData'', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Value"]
                        -- , Tasty.testCase "Campaign output value with extra tokens must fail" $ do
                        --     let campaign_UTxO_MockData'' =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaCampaignDatum
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.campaignID_TN 1
                        --                         <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) "ExtraToken" 1
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData'', investUnit_UTxO_MockData tp]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Value"]
                        -- , Tasty.testCase "IU output value without IU ID must fail" $ do
                        --     let investUnitUTxO'' =
                        --             (investUnit_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaIUDatum
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, investUnitUTxO'']
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                        -- , Tasty.testCase "IU output value with IU ID other token name must fail" $ do
                        --     let investUnitUTxO'' =
                        --             (investUnit_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaIUDatum
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) "OTHER NAME" 1
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, investUnitUTxO'']
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                        -- , Tasty.testCase "IU output value without minAda specified in datum must fail" $ do
                        --     let investUnitUTxO'' =
                        --             (investUnit_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.investUnitID_TN 1
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, investUnitUTxO'']
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value"]
                        -- , Tasty.testCase "IU output value with extra tokens must fail" $ do
                        --     let investUnitUTxO'' =
                        --             (investUnit_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     LedgerAda.lovelaceValueOf minAdaIUDatum
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) T.investUnitID_TN 1
                        --                         <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) "ExtraToken" 1
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, investUnitUTxO'']
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value"]
                    ]

--------------------------------------------------------------------------------

campaign_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
campaign_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show Campaign_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Campaign_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Burning IU ID, but not burning Campaign ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton
                        --                             (tpCampaignPolicy_CS tp)
                        --                             T.investUnitID_TN
                        --                             (negate 1)
                        --                         , CampaignT.mkBurnIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningIDs"]
                        -- , Tasty.testCase "Burning Campaign ID but not burning invest unit ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton
                        --                             (tpCampaignPolicy_CS tp)
                        --                             T.campaignID_TN
                        --                             (negate 1)
                        --                         , CampaignT.mkBurnIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningIDs"]
                        ]

--------------------------------------------------------------------------------

-- campaign_Policy_Redeemer_MintCampaignToken_Tests :: TestParams -> Tasty.TestTree
-- campaign_Policy_Redeemer_MintCampaignToken_Tests tp =
--     let
--         ------------------------
--         txName = show CampaignFunds_Deposit_Tx
--         selectedRedeemer = RedeemerLogPolicy (Just Campaign_MintCampaignToken_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--         ------------------------
--     in
--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--                 let
--                     ctx = campaign_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
--                 in
--                     [
--                         Tasty.testCase "Minting FT correctly must succeed" $ do
--                             let ctx' = ctx
--                             results <- testContextWrapper tp ctx'
--                             (Nothing, results)
--                                 `assertResultsContainAnyOf` []
--                         , Tasty.testCase "Not having Campaign UTxO as ref input must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsRef []
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected exactly one Campaign input ref"]
--                         , Tasty.testCase "Not having CampaignFunds UTxO as input must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsAndAddRedeemers []
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected exactly one CampaignFunds input"]
--                         , Tasty.testCase "Incorrect redeemer for CampaignFunds UTxO must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_NoDeposits_MockData tp, CampaignFundsT.mkWithdrawRedeemer 10 20 10)]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected CampaignFunds Validator RedeemerDeposit"]
--                         , Tasty.testCase "Minting another token name must fail" $ do
--                             let ctx' = ctx
--                                         |> setMintAndAddRedeemers
--                                             [
--                                                 ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
--                                                 , CampaignT.mkMintCampaignTokenRedeemer
--                                                 )
--                                             ]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["not isMintingFT"]
--                         , Tasty.testCase "Minting extra token must fail" $ do
--                             let ctx' = ctx
--                                         |> setMintAndAddRedeemers
--                                             [
--                                                 ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) deposit_MockData
--                                                     <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
--                                                 , CampaignT.mkMintCampaignTokenRedeemer
--                                                 )
--                                             ]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["not isMintingFT"]
--                         , Tasty.testCase "Minting more FT must fail" $ do
--                             let ctx' = ctx
--                                         |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) (deposit_MockData + sum_ANY_INVALID_NUMBER), CampaignT.mkMintCampaignTokenRedeemer)]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["not isMintingFT"]
--                         , Tasty.testCase "Minting less FT must fail" $ do
--                             let ctx' = ctx
--                                         |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) 1, CampaignT.mkMintCampaignTokenRedeemer)]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["not isMintingFT"]
--                         -- , Tasty.testCase "Minting FT with Campaign before beginAt must fail" $ do
--                         --     let before = CampaignT.fdBeginAt (campaign_DatumType_MockData tp) - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
--                         --         ctx' = ctx
--                         --                 |> setValidyRange (createValidRange before)
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isFundOpen"]
--                         -- , Tasty.testCase "Minting FT with Campaign after deadline must fail" $ do
--                         --     let after = CampaignT.fdDeadline (campaign_DatumType_MockData tp) + LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
--                         --         ctx' = ctx
--                         --                 |> setValidyRange (createValidRange after)
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isFundOpen"]
--                         -- , Tasty.testCase "Minting FT with Closed Campaign must fail" $ do
--                         --     let ctx' = ctx
--                         --                 |> setInputsRef [campaign_UTxO_MockData', investUnit_UTxO_MockData tp]

--                         --         campaign_DatumType' =
--                         --             (campaign_DatumType_MockData tp)
--                         --                 { CampaignT.fdClosedAt = Just $ tpDepositDate tp - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
--                         --                 }
--                         --         campaign_UTxO_MockData' =
--                         --             (campaign_UTxO_MockData tp)
--                         --                 { LedgerApiV2.txOutDatum =
--                         --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType'
--                         --                 }
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isFundOpen"]
--                         ]

-- --------------------------------------------------------------------------------

-- campaign_Policy_Redeemer_BurnCampaignToken_Tests :: TestParams -> Tasty.TestTree
-- campaign_Policy_Redeemer_BurnCampaignToken_Tests tp =
--     let
--         ------------------------
--         txName = show Campaign_Withdraw_Tx
--         selectedRedeemer = RedeemerLogPolicy (Just Campaign_MintCampaignToken_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--         ------------------------
--     in
--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--                 let
--                     ctx = campaign_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) withdraw_MockData
--                 in
--                     [
--                         Tasty.testCase "Burning FT correctly must succeed" $ do
--                             let ctx' = ctx
--                             results <- testContextWrapper tp ctx'
--                             (Nothing, results)
--                                 `assertResultsContainAnyOf` []
--                         , Tasty.testCase "Not having Campaign UTxO as ref input must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsRef []
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected exactly one Campaign input ref"]
--                         , Tasty.testCase "Not having CampaignFunds UTxO as input must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsAndAddRedeemers []
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected exactly one CampaignFunds input"]
--                         , Tasty.testCase "Incorrect redeemer for CampaignFunds UTxO must fail" $ do
--                             let ctx' = ctx
--                                         |> setInputsAndAddRedeemers [(campaignFunds_UTxO_With_Deposits_MockData tp, CampaignFundsT.mkDepositRedeemer 10 20)]
--                             results <- testContextWrapper tp ctx'
--                             (Just selectedRedeemer, results)
--                                 `assertResultsContainAnyOf` ["Expected CampaignFunds Validator RedeemerWithdraw"]
--                         -- , Tasty.testCase "Burning another token name must fail" $ do
--                         --     let ctx' = ctx
--                         --                 |> setMintAndAddRedeemers
--                         --                     [
--                         --                         ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") (-1)
--                         --                         , CampaignT.mkBurnCampaignTokenRedeemer
--                         --                         )
--                         --                     ]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isBurningFT"]
--                         -- , Tasty.testCase "Burning extra token must fail" $ do
--                         --     let
--                         --         !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
--                         --         (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
--                         --         ctx' = ctx
--                         --                 |> setMintAndAddRedeemers
--                         --                     [
--                         --                         ( LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) (- withdrawPlusCommissionsGetBack_MockData)
--                         --                             <> LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
--                         --                         , CampaignT.mkBurnCampaignTokenRedeemer
--                         --                         )
--                         --                     ]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isBurningFT"]
--                         -- , Tasty.testCase "Burning more FT must fail" $ do
--                         --     let
--                         --         !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
--                         --         (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
--                         --         ctx' = ctx
--                         --                 |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) (- (withdrawPlusCommissionsGetBack_MockData + sum_ANY_INVALID_NUMBER)), CampaignT.mkBurnCampaignTokenRedeemer)]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isBurningFT"]
--                         -- , Tasty.testCase "Burning less FT must fail" $ do
--                         --     let ctx' = ctx
--                         --                 |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpCampaignPolicy_CS tp) (tpFundFT_TN tp) (-1), CampaignT.mkBurnCampaignTokenRedeemer)]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just selectedRedeemer, results)
--                         --         `assertResultsContainAnyOf` ["not isBurningFT"]
--                         ]

--------------------------------------------------------------------------------
