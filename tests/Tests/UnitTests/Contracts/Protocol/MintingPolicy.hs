module Contracts.Protocol.MintingPolicy
    ( protocolMPTests
    ) where

-- Non-IOG imports
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Address            as LedgerAddress
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import           Contracts.Protocol.Data
import qualified Helpers.OffChainEval                 as OffChainEval
import qualified Helpers.OffChain              as OffChainHelpers
import qualified Constants                   as T
import qualified Protocol.Types              as ProtocolT
import qualified TestUtils.Common                     as TestUtilsCommon
import qualified TestUtils.Constants                  as T
import qualified TestUtils.Types                      as T

protocolMPTests :: T.TestParams -> Tasty.TestTree
protocolMPTests tp =
    Tasty.testGroup
        "Testing Protocol Minting Policy"
        [ 
            Tasty.testGroup
                "Testing size and resources"
                [
                    Tasty.testCase "Test Valid Minting Tx; TxValidSize < 16Kb; Mem < 14Mb; Cpu < 10_000M" $
                    let
                        ctx = mintProtocolIDContext tp
                        getValidator ad = TestUtilsCommon.findValidator tp ad
                        getMintingPolicy cs = TestUtilsCommon.findMintingPolicy tp cs
                        (eval_log, eval_err, eval_size) = OffChainEval.testContext getValidator getMintingPolicy ctx
                    in do
                        eval_log `OffChainEval.assertContainsAnyOf` []
                        OffChainEval.assertBudgetAndSize eval_err eval_size OffChainEval.maxMemory OffChainEval.maxCPU OffChainEval.maxTxSize
                ]
        ,  Tasty.testCase "Successful case" $
            let
                ctx = mintProtocolIDContext tp
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpProtocolPolicyID tp)
                    ProtocolT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` []
                )
        , Tasty.testCase "Not consuming UTxO referenced in MP parameter must fail" $
            let
                ctx =
                    mintProtocolIDContext tp OffChainEval.|> OffChainEval.setRefInputs [] OffChainEval.|> OffChainEval.setInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpProtocolPolicyID tp)
                    ProtocolT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isTxOutAnInput"]
                )
        , Tasty.testCase "Minting a different amount should fail" $
            let
                ctx =
                    mintProtocolIDContext tp
                        OffChainEval.|> OffChainEval.setMint
                            (LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 2)
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpProtocolPolicyID tp)
                    ProtocolT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingID"]
                )
        , Tasty.testCase "Invalid protocol datum value should fail" $
            let
                outputProtocolUTxO =
                    (protocolUTxO_MockData tp)
                        { LedgerApiV2.txOutValue =
                            T.toAlter_Value_Adding_SomeADA
                                <> LedgerApiV2.txOutValue (protocolUTxO_MockData tp)
                        }
                ctx = mintProtocolIDContext tp OffChainEval.|> OffChainEval.setOutputs [outputProtocolUTxO]
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpProtocolPolicyID tp)
                    ProtocolT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_ProtocolDatum_Value"]
                )
        ]
