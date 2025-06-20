--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Tests.UnitTests.Protocol.Validator
Description : Validation logic and unit tests related to the Protocol validator.

This module defines the validation logic for the Protocol's contract.

It includes multiple unit test cases to ensure the integrity and correctness of
the validator script.
-}
module Contracts.Protocol.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import           Prelude                                (show)
import           Test.QuickCheck.Instances.ByteString   ()
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.HUnit                       as Tasty
-- IOG imports
import qualified Ledger.Ada                             as LedgerAda
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports

import qualified Protocol.Types                as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Protocol
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesINNOVATIO
import qualified TestUtils.TypesINNOVATIO as T
--------------------------------------------------------------------------------

protocol_Validator_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Tests tp =
    Tasty.testGroup
        "Protocol Validator Tests"
        [ protocol_Validator_Redeemer_DatumUpdate_Tests tp
        , protocol_Validator_Redeemer_UpdateMinADA_Tests tp
        ]

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Redeemer_DatumUpdate_Tests tp =
    let
        ------------------------
        txName = show Protocol_DatumUpdate_Tx
        selectedRedeemer = RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_DatumUpdate_TxContext tp [] "aaff"
                in
                    [
                        -- Happy Path
                        Tasty.testCase "Update Datum with no change must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                        
                        -- Testing valid datum update with changes
                        , Tasty.testCase "Update Datum with valid changes must succeed" $ do
                            let 
                                newAdmins = newAdmin : T.tpProtocolAdmins tp
                                newTokenAdminPolicy = newTokenPolicy_CS
                                ctx' = protocol_DatumUpdate_TxContext tp newAdmins newTokenAdminPolicy
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []

                        -- Testing authorization failures
                        , Tasty.testCase "Missing admin signature must fail" $ do
                            let
                                ctx' = ctx |> setSignatories []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]

                        -- Testing invalid authorization signature
                        , Tasty.testCase "Invalid admin signature must fail" $ do
                            let 
                                ctx' = ctx |> setSignatories [newAdmin]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]

                        -- Testing invalid transaction range
                        , Tasty.testCase "Invalid transaction range must fail" $ do
                            let
                                ctx' = ctx |> setValidyRange (createInValidRange (tpTransactionDate tp))
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isValidRange"]

                        -- Testing missing protocol output
                        , Tasty.testCase "Missing protocol output must fail" $ do
                            let
                                ctx' = ctx |> setOutputs []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["Expected at least one output to scripts addresses"]

                        -- Testing changing protected datum fields
                        , Tasty.testCase "Changing protocol version must fail" $ do
                            let
                                invalidDatum = (protocol_DatumType_MockData tp) { ProtocolT.pdProtocolVersion = 999 }
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp) 
                                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum invalidDatum }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Updated"]

                        -- Testing value preservation
                        , Tasty.testCase "Changing output value must fail" $ do
                            let
                                extraValue = LedgerAda.lovelaceValueOf 5_000_000
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp) 
                                    { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (protocol_UTxO_MockData tp) <> extraValue }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Value_NotChanged"]

                        -- Testing emergency path
                        , Tasty.testCase "Emergency withdrawal without token must fail" $ do
                            let 
                                ctx' = ctx |> setInputsAndAddRedeemers [(protocol_UTxO_MockData tp, ProtocolT.mkEmergencyRedeemer)]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_Emergency_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isEmergencyAdminTokenPresent"]

                        -- Testing multiple inputs
                        , Tasty.testCase "Multiple protocol inputs must fail" $ do
                            let
                                extraInput = protocol_UTxO_MockData tp
                                ctx' = ctx |> addInputsWithTxOutRef [(extraInput, LedgerApiV2.TxOutRef extraTxId 1)]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["Expected exactly one Protocol input"]
                    ]

--------------------------------------------------------------------------------

protocol_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
protocol_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show Protocol_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        -- Happy Path
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []

                        -- Testing invalid minADA value
                        , Tasty.testCase "Zero minADA value must fail" $ do
                            let
                                ctx' = protocol_UpdateMinADA_TxContext tp 0
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not min ADA > 0"]

                        -- Testing auth failure
                        , Tasty.testCase "Missing admin signature must fail" $ do
                            let
                                ctx' = ctx |> setSignatories []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]

                        -- Testing invalid value adjustment
                        , Tasty.testCase "Incorrect ADA value adjustment must fail" $ do
                            let
                                wrongValueAdjustment = toAlter_minAda - 1
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (protocol_UTxO_MockData tp) 
                                        <> LedgerAda.lovelaceValueOf wrongValueAdjustment }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Value_ChangedADA"]

                        -- Testing invalid datum update
                        , Tasty.testCase "Changing other datum fields must fail" $ do
                            let
                                invalidDatum = (protocol_DatumType_MockData tp) 
                                    { ProtocolT.pdMinADA = toAlter_minAda
                                    , ProtocolT.pdAdmins = [newAdmin] -- Should not change other fields
                                    }
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum invalidDatum }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_UpdatedMinADA"]
                        
                        -- Testing invalid datum update
                        , Tasty.testCase "Invalid datum fields must fail" $ do
                            let
                                invalidDatum = (protocol_DatumType_MockData tp) 
                                    { ProtocolT.pdMinADA = toAlter_minAda -1
                                    }
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum invalidDatum }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Value_ChangedADA"]


                        -- Testing transaction range
                        , Tasty.testCase "Invalid transaction range must fail" $ do
                            let
                                ctx' = ctx |> setValidyRange (createInValidRange (tpTransactionDate tp))
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isValidRange"]

                        -- Testing missing output
                        , Tasty.testCase "Missing protocol output must fail" $ do
                            let
                                ctx' = ctx |> setOutputs []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["Expected at least one output to scripts addresses"]
                    ]


-- protocol_Validator_Redeemer_DatumUpdate_Tests :: TestParams -> Tasty.TestTree
-- protocol_Validator_Redeemer_DatumUpdate_Tests tp =
--     let
--         ------------------------
--         txName = show Protocol_DatumUpdate_Tx
--         selectedRedeemer = RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--         ------------------------
--     in
--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--                 let
--                     ctx = protocol_DatumUpdate_TxContext tp [] "aaff"
--                 in
--                     [
--                         Tasty.testCase "Update Datum with not change must succeed" $ do
--                         let
--                             ctx' = ctx
--                         results <- testContextWrapper tp ctx'
--                         (Nothing, results)
--                             `assertResultsContainAnyOf` []
--                         -- , Tasty.testCase "Updating modifiable fields must succeed" $ do
--                         --     let newPubKeyHash = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf90000"
--                         --         outputDatum =
--                         --             (protocol_DatumType_MockData tp)
--                         --                 { ProtocolT.pdOraclePaymentPubKey =
--                         --                     LedgerAddress.PaymentPubKey $
--                         --                         Ledger.PubKey $
--                         --                             LedgerApiV2.LedgerBytes $
--                         --                                 LedgerApiV2.getPubKeyHash $
--                         --                                     LedgerAddress.unPaymentPubKeyHash $
--                         --                                         LedgerAddress.PaymentPubKeyHash newPubKeyHash
--                         --                 , ProtocolT.pdAdmins = [newPubKeyHash]
--                         --                 , ProtocolT.pdFundCategories = [ProtocolT.FundCategory 4_000_000 4_000_000 4_000_000]
--                         --                 , ProtocolT.pdFundLifeTime = ProtocolT.mkMinMaxDef 1 1 1
--                         --                 , ProtocolT.pdRequiredMAYZForSwapOffer = 4_000_000
--                         --                 , ProtocolT.pdRequiredMAYZForBuyOrder = 4_000_000
--                         --                 , ProtocolT.pdCommissionCampaign_PerYear_InBPx1e3 = ProtocolT.mkMinMaxDef 1 1 1
--                         --                 , ProtocolT.pdCommissionSwapOffer_InBPx1e3 = ProtocolT.mkMinMaxDef 1 1 1
--                         --                 , ProtocolT.pdCommissionBuyOrder_InBPx1e3 = ProtocolT.mkMinMaxDef 1 1 1
--                         --                 , ProtocolT.pdShare_InBPx1e2_Protocol = 400_000
--                         --                 , ProtocolT.pdShare_InBPx1e2_Delegators = 300_000
--                         --                 , ProtocolT.pdShare_InBPx1e2_Managers = 300_000
--                         --                 , ProtocolT.pdDelegatorsAdmins = [newPubKeyHash]
--                         --                 }
--                         --         outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
--                         --         ctx' = ctx
--                         --                 |> setOutputs [outputProtocolUTxO]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Nothing, results)
--                         --         `assertResultsContainAnyOf` []
--                         -- , Tasty.testCase "Updating ProtocolT.pdScriptPolicyID_CS must fail" $ do
--                         --     let outputDatum =
--                         --             (protocol_DatumType_MockData tp)
--                         --                 { ProtocolT.pdScriptPolicyID_CS =
--                         --                     "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
--                         --                 }
--                         --         outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
--                         --         ctx' = ctx
--                         --                 |> setOutputs [outputProtocolUTxO]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Updated"]
--                         -- , Tasty.testCase "Updating minAda must fail" $ do
--                         --     let outputDatum =
--                         --             (protocol_DatumType_MockData tp)
--                         --                 { ProtocolT.pdMinADA = 1_000_000
--                         --                 }
--                         --         outputProtocolUTxO = (protocol_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
--                         --         ctx' = ctx
--                         --                 |> setOutputs [outputProtocolUTxO]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Updated"]
--                         -- , Tasty.testCase "Changing value must fail" $ do
--                         --     let outputProtocolUTxO =
--                         --             (protocol_UTxO_MockData tp)
--                         --                 { LedgerApiV2.txOutValue =
--                         --                     toAlter_Value_Adding_SomeADA
--                         --                         <> LedgerApiV2.txOutValue (protocol_UTxO_MockData tp)
--                         --                 }
--                         --         ctx' = ctx
--                         --                 |> setOutputs [outputProtocolUTxO]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Value_NotChanged"]
--                         -- , Tasty.testCase "Not signed by any must fail" $ do
--                         --     let
--                         --         ctx' = ctx
--                         --                 |> setSignatories []
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
--                         -- , Tasty.testCase "Not signed by admins must fail" $ do
--                         --     let
--                         --         ctx' = ctx
--                         --                 |> setSignatories ["aabb"]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
--                         -- , Tasty.testCase "Too big range must fail" $ do
--                         --     let
--                         --         ctx' = ctx
--                         --                 |> setValidyRange (createInValidRange (tpTransactionDate tp))
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["not isValidRange"]
--                         -- , Tasty.testCase "No protocol output must fail" $ do
--                         --     let
--                         --         ctx' = ctx
--                         --                 |> setOutputs []
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["Expected at least one output to script addresses"]
--                         -- , Tasty.testCase "Double satisfaction with 2 inputs from same address must fail" $ do
--                         --     let
--                         --         walletUTxO_With_NFT_ID =
--                         --             LedgerApiV2.TxOut
--                         --                 (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
--                         --                 (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
--                         --                 LedgerApiV2.NoOutputDatum
--                         --                 Nothing
--                         --         exampleCS' = "00000000000000000000000000000000000000000000000000000000"
--                         --         protocolUTxO_Other_NFT_ID =
--                         --             LedgerApiV2.TxOut
--                         --                 (OffChainHelpers.addressValidator (tpProtocolValidator_Hash tp))
--                         --                 (LedgerAda.lovelaceValueOf minAdaProtocolDatum <> LedgerApiV2.singleton exampleCS' T.protocolID_TN 1)
--                         --                 (LedgerApiV2.OutputDatum $ ProtocolT.mkDatum (protocol_DatumType_MockData tp))
--                         --                 Nothing
--                         --         ctx' = ctx
--                         --                 |> setInputsAndAddRedeemers
--                         --                     [(protocol_UTxO_MockData tp, ProtocolT.mkDatumUpdateRedeemer), (protocolUTxO_Other_NFT_ID, ProtocolT.mkDatumUpdateRedeemer)]
--                         --                 |> setOutputs
--                         --                     [walletUTxO_With_NFT_ID, protocolUTxO_Other_NFT_ID]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["Expected Protocol at output to script index 0"]
--                         -- , Tasty.testCase "Double satisfaction with both inputs had the same NFT (not realistic) must fail" $ do
--                         --     let
--                         --         walletUTxO_With_NFT_ID =
--                         --             LedgerApiV2.TxOut
--                         --                 (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
--                         --                 (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens <> LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
--                         --                 LedgerApiV2.NoOutputDatum
--                         --                 Nothing
--                         --         protocolUTxO_SameNFT_ID_NOT_REALISTIC =
--                         --             LedgerApiV2.TxOut
--                         --                 (OffChainHelpers.addressValidator (tpProtocolValidator_Hash tp))
--                         --                 (LedgerAda.lovelaceValueOf minAdaProtocolDatum <> LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
--                         --                 (LedgerApiV2.OutputDatum $ ProtocolT.mkDatum (protocol_DatumType_MockData tp))
--                         --                 Nothing
--                         --         ctx' = ctx
--                         --                 |> setInputsAndAddRedeemers
--                         --                     [(protocolUTxO_SameNFT_ID_NOT_REALISTIC, ProtocolT.mkDatumUpdateRedeemer), (protocol_UTxO_MockData tp, ProtocolT.mkDatumUpdateRedeemer)]
--                         --                 |> setOutputs
--                         --                     [walletUTxO_With_NFT_ID, protocolUTxO_SameNFT_ID_NOT_REALISTIC]
--                         --     results <- testContextWrapper tp ctx'
--                         --     (Just (RedeemerLogValidator (Just Protocol_DatumUpdate_TestRedeemer)), results)
--                         --         `assertResultsContainAnyOf` ["Expected exactly one Protocol input"]

--                         ]

-- --------------------------------------------------------------------------------

-- protocol_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
-- protocol_Validator_Redeemer_UpdateMinADA_Tests tp =
--     let
--         ------------------------
--         txName = show Protocol_UpdateMinADA_Tx
--         selectedRedeemer = RedeemerLogValidator (Just Protocol_UpdateMinADA_TestRedeemer)
--         redeemerName = getRedeemerNameFromLog selectedRedeemer
--         ------------------------
--     in
--         Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
--                 let
--                     ctx = protocol_UpdateMinADA_TxContext tp toAlter_minAda
--                 in
--                     [
--                         Tasty.testCase "Changing min ADA correctly must succeed" $ do
--                             let ctx' = ctx
--                             results <- testContextWrapper tp ctx'
--                             (Nothing, results)
--                                 `assertResultsContainAnyOf` []
--                     ]

--------------------------------------------------------------------------------
