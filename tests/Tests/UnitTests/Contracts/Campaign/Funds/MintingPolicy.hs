--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Campaign.Funds.MintingPolicy
Description : Validation logic and tests related to the CampaignFunds
              minting policy.

This module defines the validation logic for the CampaignFunds's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Campaign.Funds.MintingPolicy where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import           Prelude                                   (show)
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.HUnit                          as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import           TestUtils.Contracts.TxContext.CampaignFunds
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Asserts
import           TestUtils.TypesINNOVATIO

--------------------------------------------------------------------------------

campaignFunds_Policy_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Policy_Tests tp =
    Tasty.testGroup
        "CampaignFunds Policy Tests"
        [
            campaignFunds_Policy_Redeemer_MintID_Tests tp,
            campaignFunds_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

campaignFunds_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Policy_Redeemer_MintID_Tests tp =
   let
        ------------------------
        txName = show Campaign_FundsAdd_Tx
        selectedRedeemer = RedeemerLogPolicy (Just CampaignFunds_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaignFunds_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                        let
                            ctx' = ctx
                        results <- testContextWrapper tp ctx'
                        (Nothing, results)
                            `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Not including Fund UTxO input must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setInputsAndAddRedeemers []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                        -- , Tasty.testCase "Not including CampaignFunds UTxO output must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setOutputs []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["Expected at least two outputs to script addresses"]
                        -- , Tasty.testCase "Not setting valid Redeemer for consuming Fund Datum must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setInputsAndAddRedeemers [(campaign_UTxO_MockData tp, CampaignT.mkDatumUpdateRedeemer)]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["not isCorrect_Redeemer_Fund"]
                        -- , Tasty.testCase "Including a wrong CampaignFunds Datum must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_With_Added_CampaignFunds_MockData tp, wrongUTxO]
                        --         wrongDatum =
                        --             CampaignFundsT.mkCampaignFunds_DatumType
                        --                 10 -- hdCampaignFunds_Index
                        --                 0 -- hdSubtotal_FT_Minted_Accumulated
                        --                 0 -- hdSubtotal_FT_Minted
                        --                 22 -- hdSubtotal_FT_Commissions
                        --                 0 -- hdSubtotal_FT_Commissions_Release_PerMonth_1e6
                        --                 0 -- hdSubtotal_FT_Commissions_Collected_Protocol
                        --                 0 -- hdSubtotal_FT_Commissions_Collected_Managers
                        --                 0 -- hdSubtotal_FT_Commissions_Collected_Delegators
                        --                 minAdaCampaignFundsDatum
                        --         wrongUTxO =
                        --             (campaignFunds_UTxO_With_NoDeposits_MockData tp)
                        --                 { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum wrongDatum
                        --                 }
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFund_Datum"]
                        -- , Tasty.testCase "Including a wrong CampaignFunds Value must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, wrongUTxO]
                        --         wrongUTxO =
                        --             (campaignFunds_UTxO_With_NoDeposits_MockData tp)
                        --                 { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (campaignFunds_UTxO_With_NoDeposits_MockData tp) <> toAlter_Value_Adding_SomeADA
                        --                 }
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["not isCorrect_Output_CampaignFunds_Value"]
                        -- , Tasty.testCase "Including a wrong CampaignFunds UTxO Address must fail" $ do
                        --     let
                        --         ctx' = ctx
                        --                 |> setOutputs [campaign_UTxO_MockData tp, wrongUTxO]
                        --         wrongUTxO =
                        --             (campaignFunds_UTxO_With_NoDeposits_MockData tp)
                        --                 { LedgerApiV2.txOutAddress  = OffChainHelpers.addressValidator exampleValidatorHash
                        --                 }
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --             `assertResultsContainAnyOf` ["Expected CampaignFunds at output index 1"]

                    ]


--------------------------------------------------------------------------------

campaignFunds_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
campaignFunds_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsDelete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just CampaignFunds_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
        -- NOTE: con esta forma puedo generar conextos a partir de TxSpecs
        -- txSpecs = campaignFunds_Collect_Delegators_Commission_TxSpecs tp
        -- defaultTxSpecs = txSpecs txParams_Default
        -- defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        -- ctx = context_Gen tp txSpecs defaultTestCaseParams
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let

                    ctx = campaignFunds_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                        let
                            ctx' = ctx
                        results <- testContextWrapper tp ctx'
                        (Nothing, results)
                            `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
