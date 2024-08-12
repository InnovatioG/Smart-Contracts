{- |
Module      : Tests.UnitTests.Protocol.Validator
Description : Validation logic and unit tests related to the Protocol validator.

This module defines the validation logic for the Protocol's contract.

It includes multiple unit test cases to ensure the integrity and correctness of
the validator script.
-}
module Contracts.Protocol.Validator
    ( protocolValidatorTests
    ) where

-- Non-IOG imports
import           Test.QuickCheck.Instances.ByteString ()
import qualified Test.Tasty                           as Tasty
import qualified Test.Tasty.HUnit                     as Tasty

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                           as LedgerAda
import qualified Ledger.Address                       as LedgerAddress
import qualified Ledger.Interval                      as LedgerInterval
import qualified Plutus.V2.Ledger.Api                 as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Constants                            as T
import           Contracts.Protocol.Data
import qualified Helpers.OffChain                     as OffChainHelpers
import qualified Helpers.OffChainEval                 as OffChainEval
import qualified Protocol.Types                       as ProtocolT
import qualified TestUtils.Common                     as TestUtilsCommon
import qualified TestUtils.Constants                  as T
import qualified TestUtils.Types                      as T

protocolValidatorTests :: T.TestParams -> Tasty.TestTree
protocolValidatorTests tp =
    Tasty.testGroup
        "Protocol Validator Tests"
        [
            Tasty.testGroup
                "Testing size and resources"
                [
                    Tasty.testCase "Test Valid Update Tx; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                    let
                        ctx = updateProtocolContext tp
                        getValidator ad = TestUtilsCommon.findValidator tp ad
                        getMintingPolicy cs = TestUtilsCommon.findMintingPolicy tp cs
                        (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                    in do
                        eval_log `OffChainEval.assertContainsAnyOf` []
                        OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
                ]
            ,

            Tasty.testCase
            "Datum not changed must succeed"
            ( OffChainEval.evaluateScriptValidatorEX
                (T.tpProtocolValidator tp)
                (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                ProtocolT.mkDatumUpdateRedeemer
                (updateProtocolContext tp)
                `OffChainEval.logAassertContainsAnyOf` []
            )
        , Tasty.testCase "Updating modifiable fields must succeed" $
            let
                newPubKeyHash = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf90000"
                outputDatum =
                    (protocolDatumType_MockData tp)
                        {
                            ProtocolT.pdAdmins = [newPubKeyHash]
                        }
                outputProtocolUTxO = (protocolUTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setOutputs
                            [ outputProtocolUTxO
                            ]
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
                )

        , Tasty.testCase "Updating minAda must fail" $
            let
                outputDatum =
                    (protocolDatumType_MockData tp)
                        { ProtocolT.pdMinADA = 1_000_000
                        }
                outputProtocolUTxO = (protocolUTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum outputDatum}
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setOutputs
                            [ outputProtocolUTxO
                            ]
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_ProtocolDatum_Updated"]
                )
        , Tasty.testCase "Changing value must fail" $
            let
                outputProtocolUTxO =
                    (protocolUTxO_MockData tp)
                        { LedgerApiV2.txOutValue =
                            T.toAlter_Value_Adding_SomeADA
                                <> LedgerApiV2.txOutValue (protocolUTxO_MockData tp)
                        }
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setOutputs
                            [ outputProtocolUTxO
                            ]
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_ProtocolDatum_Value_NotChanged"]
                )
        , Tasty.testCase "Not signed by admins must fail" $
            let
                ctx = updateProtocolContext tp OffChainEval.|> OffChainEval.setSignatories []
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                )
        , Tasty.testCase "Too big tx range must fail" $
            let
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createInValidRange (T.tpTransactionDate tp) T.validTxTimeRange)
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isValidRange"]
                )
        , Tasty.testCase "No protocol output must fail" $
            let
                ctx = updateProtocolContext tp OffChainEval.|> OffChainEval.setOutputs []
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected at least one output to scripts addresses"]
                )
        , -- , Tasty.testCase "More than one protocol output must fail" $
          --     let ctx =
          --             updateProtocolContext tp
          --                 OffChainEval.|> OffChainEval.setOutputs [protocolUTxO_MockData tp, protocolUTxO_MockData tp]
          --      in ( OffChainEval.evaluateScriptValidatorEX
          --             (T.tpProtocolValidator tp)
          --             (ProtocolT.mkDatum (protocolDatumType_MockData tp))
          --             ProtocolT.mkDatumUpdateRedeemer
          --             ctx
          --             `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Protocol output"]
          --         )
          -- Double satisfaction: try to steal the Protocol ID by replacing
          -- it with another token.
          Tasty.testCase "Double satisfaction must fail" $
            let
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setInputs
                            [protocolUTxO_MockData tp, protocolUTxO']
                        OffChainEval.|> OffChainEval.setOutputs
                            [walletUTxO, protocolUTxO']
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected Protocol at output to script index 0"]
                )
        , -- Double satisfaction: if two Protocol UTxOs have the same NFT, the
          -- attack succeeds.
          Tasty.testCase "Double satisfaction must fail  with both inputs had the same NFT" $
            let
                ctx =
                    updateProtocolContext tp
                        OffChainEval.|> OffChainEval.setInputs
                            [protocolUTxO'', protocolUTxO_MockData tp ]
                        OffChainEval.|> OffChainEval.setOutputs
                            [walletUTxO, protocolUTxO'']
            in
                ( OffChainEval.evaluateScriptValidatorEX
                    (T.tpProtocolValidator tp)
                    (ProtocolT.mkDatum (protocolDatumType_MockData tp))
                    ProtocolT.mkDatumUpdateRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Protocol input"]
                )
        ]
    where
        exampleCS' = "00000000000000000000000000000000000000000000000000000000"
        walletUTxO =
            LedgerApiV2.TxOut
                (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "a2") Nothing)
                (LedgerAda.lovelaceValueOf T.minAdaForUTxOWithTokens <> LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
                LedgerApiV2.NoOutputDatum
                Nothing
        protocolUTxO' =
            LedgerApiV2.TxOut
                (OffChainHelpers.addressValidator (T.tpProtocolValidator_Hash tp))
                (LedgerAda.lovelaceValueOf T.minAdaProtocolDatum <> LedgerApiV2.singleton exampleCS' T.protocolID_TN 1)
                (LedgerApiV2.OutputDatum $ ProtocolT.mkDatum (protocolDatumType_MockData tp))
                Nothing
        protocolUTxO'' =
            LedgerApiV2.TxOut
                (OffChainHelpers.addressValidator (T.tpProtocolValidator_Hash tp))
                (LedgerAda.lovelaceValueOf T.minAdaProtocolDatum <> LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
                (LedgerApiV2.OutputDatum $ ProtocolT.mkDatum (protocolDatumType_MockData tp))
                Nothing
