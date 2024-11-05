--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Contracts.Campaign.Validator
Description : Validation logic and tests related to the Fund module.

This module defines the validation logic for the Fund contracts, including the
creation and management of CampaignFundss.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.Campaign.Validator where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import           Prelude                            (show)
import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.HUnit                   as Tasty

-- IOG imports
import qualified Ledger.Ada                         as LedgerAda
import qualified Ledger.Address                     as LedgerAddress
import qualified Plutus.V2.Ledger.Api               as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Helpers.OffChain            as OffChainHelpers
import qualified Constants                      as T
import qualified Campaign.Funds.Types        as CampaignFundsT
import qualified Campaign.Types                as CampaignT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Campaign
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesINNOVATIO

--------------------------------------------------------------------------------

campaign_Validator_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Tests tp =
    Tasty.testGroup
        "Campaign Validator Tests"
        [ campaign_Validator_Redeemer_DatumUpdate_Tests tp
        , campaign_Validator_Redeemer_UpdateMinADA_Tests tp
        , campaign_Validator_Redeemer_FundsAdd_Tests tp
        , campaign_Validator_Redeemer_FundsMerge_Tests tp
        , campaign_Validator_Redeemer_FundsDelete_Tests tp
        , campaign_Validator_Redeemer_FundsCollect_Tests tp
        , campaign_Validator_Redeemer_InitializeCampaign_Tests tp
        , campaign_Validator_Redeemer_ReachedCampaign_Tests tp
        , campaign_Validator_Redeemer_NotReachedCampaign_Tests tp
        , campaign_Validator_Redeemer_MilestoneAprobe_Tests tp
        , campaign_Validator_Redeemer_MilestoneReprobe_Tests tp
        , campaign_Validator_Redeemer_Emergency_Tests tp
        , campaign_Validator_Redeemer_Delete_Tests tp
        ]
--------------------------------------------------------------------------------

campaign_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_DatumUpdate_Tests tp =
    let
        ------------------------
        txName = show Campaign_DatumUpdate_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_DatumUpdate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_DatumUpdate_TxContext tp [] "aaff"
                in
                    [
                        Tasty.testCase "Update Datum with not change must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Emptying the list of admins must succeed" $ do
                        --     let modifiedDatumType = (campaign_DatumType_MockData tp) {CampaignT.cdAdmins = []}
                        --         modifiedUTxO =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Nothing, results)
                        --         `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Changing CampaignPolicyID must fail" $ do
                        --     let modifiedDatumType =
                        --             (campaign_DatumType_MockData tp)
                        --                 { CampaignT.cdCampaignPolicy_CS =
                        --                     "a4b8ef314629ab3c5012d3e58d6b4f2a3e9f5d47c2ad34e7f1e8c2fa"
                        --                 }
                        --         modifiedUTxO =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $
                        --                         CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum_Updated"]
                        -- , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                        --     let modifiedUTxO =
                        --             (campaign_UTxO_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     toAlter_Value_Adding_SomeADA
                        --                         <> LedgerApiV2.txOutValue (campaign_UTxO_MockData tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Value_NotChanged"]
                        ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_UpdateMinADA_Tests tp =
    let ------------------------
        txName = show Campaign_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_FundsAdd_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_FundsAdd_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsAdd_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_FundsAdd_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_FundsAdd_TxContext tp
                in
                    [
                        Tasty.testCase "Add CampaignFunds correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Not minting CampaignFunds ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers mempty
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isMintingCampaignFundsID"]
                        -- , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                        --     let modifiedUTxO =
                        --             (campaign_UTxO_With_Added_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     toAlter_Value_Adding_SomeADA
                        --                         <> LedgerApiV2.txOutValue (campaign_UTxO_With_Added_CampaignFunds_MockData tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Value_NotChanged"]
                        -- , Tasty.testCase "Not signed by admin must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setSignatories []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                        -- , Tasty.testCase "Not incrementing count must fail" $ do
                        --     let modifiedDatumType = (campaign_DatumType_With_Added_CampaignFunds_MockData tp) {CampaignT.cdFundsCount = 0}
                        --         modifiedUTxO =
                        --             (campaign_UTxO_With_Added_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum_With_CampaignFunds_Added"]
                        -- , Tasty.testCase "Not incrementing index must fail" $ do
                        --     let modifiedDatumType = (campaign_DatumType_With_Added_CampaignFunds_MockData tp) {CampaignT.cdFundsIndex = 0}
                        --         modifiedUTxO =
                        --             (campaign_UTxO_With_Added_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum_With_CampaignFunds_Added"]
                        ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_FundsMerge_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_FundsMerge_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsMerge_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_FundsMerge_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_FundsMerge_TxContext tp
                in
                    [
                        Tasty.testCase "Merge CampaignFunds correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_FundsCollect_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_FundsCollect_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsCollect_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_FundsCollect_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_FundsCollect_TxContext tp
                in
                    [
                        Tasty.testCase "Collect funds correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]


campaign_Validator_Redeemer_FundsDelete_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_FundsDelete_Tests tp =
    let
        ------------------------
        txName = show Campaign_FundsDelete_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_FundsDelete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_FundsDelete_TxContext tp
                in
                    [
                        Tasty.testCase "Delete CampaignFunds correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Not burning CampaignFunds ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningCampaignFundsID"]
                        -- , Tasty.testCase "Minting another CampaignFunds ID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton
                        --                             (CampaignT.cdCampaignFundsPolicyID_CS (campaign_DatumType_MockData tp))
                        --                             (mkCampaignFundsID_TN 0)
                        --                             1
                        --                         , CampaignFundsT.mkBurnIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningCampaignFundsID"]
                        -- , Tasty.testCase "Not decreasing funds Count must fail" $ do
                        --     let modifiedDatumType =
                        --             (campaign_DatumType_With_Deleted_CampaignFunds_MockData tp)
                        --                 { CampaignT.cdFundsCount =
                        --                     CampaignT.cdFundsCount (campaign_DatumType_With_Added_CampaignFunds_MockData tp)
                        --                 }
                        --         modifiedUTxO =
                        --             (campaign_UTxO_With_Deleted_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum_With_CampaignFunds_Deleted"]
                        -- , Tasty.testCase "Increasing funds Count must fail" $ do
                        --     let modifiedDatumType =
                        --             (campaign_DatumType_With_Deleted_CampaignFunds_MockData tp)
                        --                 { CampaignT.cdFundsCount =
                        --                     CampaignT.cdFundsCount (campaign_DatumType_With_Deleted_CampaignFunds_MockData tp) + 1
                        --                 }
                        --         modifiedUTxO =
                        --             (campaign_UTxO_With_Deleted_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutDatum =
                        --                     LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Datum_With_CampaignFunds_Deleted"]
                        -- , Tasty.testCase "Changing Fund UTxO value must fail" $ do
                        --     let modifiedUTxO =
                        --             (campaign_UTxO_With_Deleted_CampaignFunds_MockData tp)
                        --                 { LedgerApiV2.txOutValue =
                        --                     toAlter_Value_Adding_SomeADA
                        --                         <> LedgerApiV2.txOutValue (campaign_UTxO_With_Deleted_CampaignFunds_MockData tp)
                        --                 }
                        --         ctx' = ctx
                        --                 |> setOutputs [modifiedUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isCorrect_Output_Campaign_Value_NotChanged"]
                        ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_InitializeCampaign_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_InitializeCampaign_Tests tp =
    let
        ------------------------
        txName = show Campaign_InitializeCampaign_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_InitializeCampaign_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_InitializeCampaign_TxContext tp
                in
                    [
                        Tasty.testCase "Initialize campaign correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_ReachedCampaign_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_ReachedCampaign_Tests tp =
    let
        ------------------------
        txName = show Campaign_ReachedCampaign_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_ReachedCampaign_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_ReachedCampaign_TxContext tp
                in
                    [
                        Tasty.testCase "Mark campaign as reached correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_NotReachedCampaign_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_NotReachedCampaign_Tests tp =
    let
        ------------------------
        txName = show Campaign_NotReachedCampaign_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_NotReachedCampaign_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_NotReachedCampaign_TxContext tp
                in
                    [
                        Tasty.testCase "Mark campaign as not reached correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_MilestoneAprobe_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_MilestoneAprobe_Tests tp =
    let
        ------------------------
        txName = show Campaign_MilestoneAprobe_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_MilestoneAprobe_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_MilestoneAprobe_TxContext tp 0 -- Testing first milestone
                in
                    [
                        Tasty.testCase "Approve milestone correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_MilestoneReprobe_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_MilestoneReprobe_Tests tp =
    let
        ------------------------
        txName = show Campaign_MilestoneReprobe_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_MilestoneReprobe_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_MilestoneReprobe_TxContext tp 0 -- Testing first milestone
                in
                    [
                        Tasty.testCase "Reprove milestone correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_Emergency_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_Emergency_Tests tp =
    let
        ------------------------
        txName = show Campaign_Emergency_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_Emergency_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_Emergency_TxContext tp
                in
                    [
                        Tasty.testCase "Emergency withdraw correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

campaign_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
campaign_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show Campaign_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just Campaign_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = campaign_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Delete correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        -- , Tasty.testCase "Not burning CampaignID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     []
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningCampaignID"]
                        -- , Tasty.testCase "Minting another CampaignID must fail" $ do
                        --     let ctx' = ctx
                        --                 |> setMintAndAddRedeemers
                        --                     [
                        --                         ( LedgerApiV2.singleton
                        --                             (CampaignT.cdCampaignPolicy_CS (campaign_DatumType_MockData tp))
                        --                             T.campaignID_TN
                        --                             (negate 1)
                        --                             <> LedgerApiV2.singleton
                        --                                 (CampaignT.cdCampaignPolicy_CS (campaign_DatumType_MockData tp))
                        --                                 T.campaignID_TN
                        --                                 1
                        --                         , CampaignT.mkBurnIDRedeemer
                        --                         )
                        --                     ]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["not isBurningCampaignID"]
                        -- , -- Double satisfaction: try to steal the Protocol ID by replacing
                        -- -- it with another token.
                        -- Tasty.testCase "Double satisfaction must fail" $ do
                        --     let campaignPolicy_CS' = "00000000000000000000000000000000000000000000000000000000"
                        --         campaign_DatumType' =
                        --             (OffChainHelpers.getUnsafe_DatumType_From_TxOutOutputDatum (campaign_UTxO_MockData tp) CampaignT.getCampaign_DatumType)
                        --                 { CampaignT.cdCampaignPolicy_CS = campaignPolicy_CS'
                        --                 }
                        --         campaign_UTxO_MockData' =
                        --             LedgerApiV2.TxOut
                        --                 (OffChainHelpers.addressValidator (tpCampaignValidator_Hash tp))
                        --                 (LedgerAda.lovelaceValueOf minAdaCampaignDatum <> LedgerApiV2.singleton campaignPolicy_CS' T.campaignID_TN 1)
                        --                 (LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaign_DatumType')
                        --                 Nothing
                        --         walletUTxO =
                        --             LedgerApiV2.TxOut
                        --                 (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
                        --                 (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> LedgerApiV2.singleton campaignPolicy_CS' T.campaignID_TN 1)
                        --                 LedgerApiV2.NoOutputDatum
                        --                 Nothing
                        --         ctx' = ctx
                        --                 |> setInputsAndAddRedeemers
                        --                     [(campaign_UTxO_MockData tp, CampaignT.mkDeleteRedeemer), (campaign_UTxO_MockData', CampaignT.mkDeleteRedeemer)]
                        --                 |> setOutputs
                        --                     [walletUTxO]
                        --     results <- testContextWrapper tp ctx'
                        --     (Just selectedRedeemer, results)
                        --         `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                        ]

--------------------------------------------------------------------------------
