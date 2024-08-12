{- |
Module      : Contracts.Campaign.Validator
Description : Validation logic and tests related to the Fund module.

This module defines the validation logic for the Fund contracts, including the
creation and management of Campaign Fundss.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}
module Contracts.Campaign.Validator where

-- Non-IOG imports
import qualified Test.Tasty              as Tasty
import qualified Test.Tasty.HUnit        as Tasty

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada              as LedgerAda
import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Campaign.Types          as CampaignT
import qualified Constants               as T
import           Contracts.Campaign.Data
import           Contracts.Protocol.Data
import qualified Helpers.OffChain        as OffChainHelpers
import qualified Helpers.OffChainEval    as OffChainEval
import qualified Protocol.Types          as ProtocolT
import qualified TestUtils.Common        as TestUtilsCommon
import qualified TestUtils.Constants     as T
import qualified TestUtils.Types         as T

-- | Suite of tests validating the logic of the '(T.tpCampaignValidator tp)'.
campaignValidatorTests :: T.TestParams -> Tasty.TestTree
campaignValidatorTests tp =
    Tasty.testGroup
        "Testing Fund Validator"
        [ updateDatumTests tp
        , deleteTests tp
        , addCampaignFundsTests tp
        , deleteCampaignFundsTests tp
        ]

-- | Test group for updating the datum.
updateDatumTests :: T.TestParams -> Tasty.TestTree
updateDatumTests tp =
    Tasty.testGroup
        "Update Datum tests"
        [ 
             Tasty.testGroup
                "Testing size and resources"
                [
                    Tasty.testCase "Test Valid Update Tx; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                    let
                        ctx = updateCampaignDatumContext tp
                        getValidator ad = TestUtilsCommon.findValidator tp ad
                        getMintingPolicy cs = TestUtilsCommon.findMintingPolicy tp cs
                        (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                    in do
                        eval_log `OffChainEval.assertContainsAnyOf` []
                        OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
                ]
        , Tasty.testCase "Successful case" $
            let
                ctx = updateCampaignDatumContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
        , Tasty.testCase "Emptying the list of admins should succeed" $
            let
                modifiedDatumType = (campaignDatumType_MockData tp) {CampaignT.cdAdmins = []}
                modifiedUTxO =
                    (campaignUTxO_MockData tp)
                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx = updateCampaignDatumContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
        , Tasty.testCase "Changing CampaignPolicy_CS should fail" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdCampaignPolicy_CS =
                            "a4b8ef314629ab3c5012d3e58d6b4f2a3e9f5d47c2ad34e7f1e8c2ff"
                        }
                modifiedUTxO =
                    (campaignUTxO_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $
                                CampaignT.mkDatum modifiedDatumType
                        }
                ctx = updateCampaignDatumContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_Updated"]
        , Tasty.testCase "Changing Campaign UTxO value should fail" $
            let
                modifiedUTxO =
                    (campaignUTxO_MockData tp)
                        { LedgerApiV2.txOutValue =
                            T.toAlter_Value_Adding_SomeADA
                                <> LedgerApiV2.txOutValue (campaignUTxO_MockData tp)
                        }
                ctx = updateCampaignDatumContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_Value_NotChanged"]
        ]

-- | Test group for burning Campaign ID tokens.
deleteTests :: T.TestParams -> Tasty.TestTree
deleteTests tp =
    Tasty.testGroup
        "Burn the Campaign ID of a Campaign UTxO"
        [ Tasty.testCase "Successful case" $
            let
                ctx = deleteCampaignDatumContext tp
            in
                -- OffChainEval.evaluateScriptValidatorEX
                --     (T.tpCampaignValidator tp)
                --     (CampaignT.mkDatum (campaignDatumType_MockData tp))
                --     CampaignT.mkDeleteRedeemer
                --     ctx
                --     `OffChainEval.logAassertContainsAnyOf` []
                OffChainEval.testContext (TestUtilsCommon.findValidator tp) (TestUtilsCommon.findMintingPolicy tp) ctx
                    `OffChainEval.logAassertContainsAnyOf` []
        , Tasty.testCase "Not burning Campaign ID should fail" $
            let
                ctx =
                    deleteCampaignDatumContext tp
                        OffChainEval.|> OffChainEval.setMintAndRedeemers
                            []
            in
                -- OffChainEval.evaluateScriptValidatorEX
                --     (T.tpCampaignValidator tp)
                --     (CampaignT.mkDatum (campaignDatumType_MockData tp))
                --     CampaignT.mkDeleteRedeemer
                --     ctx
                OffChainEval.testContext (TestUtilsCommon.findValidator tp) (TestUtilsCommon.findMintingPolicy tp) ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningCampaignID"]
        , Tasty.testCase "Minting another Campaign ID should fail" $
            let
                ctx =
                    deleteCampaignDatumContext tp
                        OffChainEval.|> OffChainEval.addMintAndRedeemers
                            [
                                ( LedgerApiV2.singleton
                                    (CampaignT.cdCampaignPolicy_CS (campaignDatumType_MockData tp))
                                    T.campaignID_TN
                                    1
                                , CampaignT.mkMintIDRedeemer
                                )
                            ]
            in
                -- OffChainEval.evaluateScriptValidatorEX
                --     (T.tpCampaignValidator tp)
                --     (CampaignT.mkDatum (campaignDatumType_MockData tp))
                --     CampaignT.mkDeleteRedeemer
                --     ctx
                OffChainEval.testContext (TestUtilsCommon.findValidator tp) (TestUtilsCommon.findMintingPolicy tp) ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningCampaignID"]
        , -- Double satisfaction: try to steal the Protocol ID by replacing
          -- it with another token.
          Tasty.testCase "Double satisfaction must fail" $
            let
                campaignPolicy_CS' = "00000000000000000000000000000000000000000000000000000000"
                campaignDatumType' =
                    (OffChainHelpers.getUnsafe_DatumType_From_TxOutOutputDatum (campaignUTxO_MockData tp) CampaignT.getCampaignDatumType)
                        { CampaignT.cdCampaignPolicy_CS = campaignPolicy_CS'
                        }
                campaignUTxO_MockData' =
                    LedgerApiV2.TxOut
                        (OffChainHelpers.addressValidator (T.tpCampaignValidator_Hash tp))
                        (LedgerAda.lovelaceValueOf T.minAdaCampaignDatum <> LedgerApiV2.singleton campaignPolicy_CS' T.campaignID_TN 1)
                        (LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaignDatumType')
                        Nothing
                walletUTxO =
                    LedgerApiV2.TxOut
                        (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
                        (LedgerAda.lovelaceValueOf T.minAdaForUTxOWithTokens <> LedgerApiV2.singleton campaignPolicy_CS' T.campaignID_TN 1)
                        LedgerApiV2.NoOutputDatum
                        Nothing
                ctx =
                    deleteCampaignDatumContext tp
                        OffChainEval.|> OffChainEval.setInputsAndAddRedeemers
                            [(campaignUTxO_MockData tp, CampaignT.mkDeleteRedeemer), (campaignUTxO_MockData', CampaignT.mkDeleteRedeemer)]
                        OffChainEval.|> OffChainEval.setOutputs
                            [walletUTxO]
            in
                -- OffChainEval.evaluateScriptValidatorEX
                --     (T.tpCampaignValidator tp)
                --     (CampaignT.mkDatum campaignDatumType')
                --     CampaignT.mkDeleteRedeemer
                --     ctx
                OffChainEval.testContext (TestUtilsCommon.findValidator tp) (TestUtilsCommon.findMintingPolicy tp) ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Campaign input"]
        ]

-- | Test group for adding Campaign Funds.
addCampaignFundsTests :: T.TestParams -> Tasty.TestTree
addCampaignFundsTests tp =
    Tasty.testGroup
        "Add Campaign Funds tests"
        [ Tasty.testCase "Successful case" $
            let
                ctx = addCampaignFundsContext tp
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
        , Tasty.testCase "Not minting CampaignFunds ID should fail" $
            let
                ctx = addCampaignFundsContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingCampaignFundsID"]
        , Tasty.testCase "Changing Campaign UTxO value should fail" $
            let
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutValue =
                            T.toAlter_Value_Adding_SomeADA
                                <> LedgerApiV2.txOutValue (campaignUTxOWithFunds_MockData tp)
                        }
                ctx = addCampaignFundsContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_Value_NotChanged"]
        , Tasty.testCase "Not signed by admin should fail" $
            let
                ctx = addCampaignFundsContext tp OffChainEval.|> OffChainEval.setSignatories []
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
        , Tasty.testCase "Not incrementing count should fail" $
            let
                modifiedDatumType = (campaignDatumType_MockData tp) {CampaignT.cdFundsCount = 0}
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx = addCampaignFundsContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_With_CampaignFundsAdded"]
        , Tasty.testCase "Not incrementing index should fail" $
            let
                modifiedDatumType = (campaignDatumType_MockData tp) {CampaignT.cdFundsIndex = 0}
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx = addCampaignFundsContext tp OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    CampaignT.mkCampaignFundsAddRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_With_CampaignFundsAdded"]
        ]

-- | Test group for burning HoldingIDs.
deleteCampaignFundsTests :: T.TestParams -> Tasty.TestTree
deleteCampaignFundsTests tp =
    Tasty.testGroup
        "Burn the CampaignFundsID of a CampaignFunds UTxO"
        [ Tasty.testCase "Successful case" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount =
                            CampaignT.cdFundsCount (campaignDatumType_MockData tp) - 1
                        }
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                (negate 1)
                            )
                        OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
        , Tasty.testCase "Not burning HoldingID should fail" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount =
                            CampaignT.cdFundsCount (campaignDatumType_MockData tp) - 1
                        }
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                (negate 0)
                            )
                        OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningCampaignFundsID"]
        , Tasty.testCase "Minting another HoldingID should fail" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount =
                            CampaignT.cdFundsCount (campaignDatumType_MockData tp) - 1
                        }
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                1
                            )
                        OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningCampaignFundsID"]
        , Tasty.testCase "Not decreasing fundsCount should fail" $
            let
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                (negate 1)
                            )
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted"]
        , Tasty.testCase "Increasing fundsCount should fail" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount =
                            CampaignT.cdFundsCount (campaignDatumType_MockData tp) + 1
                        }
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                (negate 1)
                            )
                        OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted"]
        , Tasty.testCase "Changing Campaign UTxO value should fail" $
            let
                modifiedDatumType =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount =
                            CampaignT.cdFundsCount (campaignDatumType_MockData tp) - 1
                        }
                modifiedUTxO =
                    (campaignUTxOWithFunds_MockData tp)
                        { LedgerApiV2.txOutValue =
                            T.toAlter_Value_Adding_SomeADA
                                <> LedgerApiV2.txOutValue (campaignUTxOWithFunds_MockData tp)
                        , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignT.mkDatum modifiedDatumType
                        }
                ctx =
                    addCampaignFundsContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (CampaignT.cdCampaignFundsPolicyID_CS (campaignDatumType_MockData tp))
                                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                                (negate 1)
                            )
                        OffChainEval.|> OffChainEval.setOutputs [modifiedUTxO]
            in
                OffChainEval.evaluateScriptValidatorEX
                    (T.tpCampaignValidator tp)
                    (CampaignT.mkDatum (campaignDatumType_MockData tp))
                    (CampaignT.mkCampaignFundsDeleteRedeemer 1)
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum_Value_NotChanged"]
        ]
