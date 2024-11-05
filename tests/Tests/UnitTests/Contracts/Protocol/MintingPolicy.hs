--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

module Contracts.Protocol.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import           Prelude                                (show)
import qualified Test.Tasty                             as Tasty
import qualified Test.Tasty.HUnit                       as Tasty

-- IOG imports
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Constants                              as T
import qualified Helpers.OffChain                       as OffChainHelpers
import qualified Ledger.Ada                             as LedgerAda
import qualified Protocol.Types                         as ProtocolT
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Protocol
import           TestUtils.HelpersINNOVATIO
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import qualified TestUtils.TypesINNOVATIO               as T
import           TestUtils.TypesINNOVATIO

--------------------------------------------------------------------------------

protocol_Policy_Tests :: TestParams -> Tasty.TestTree
protocol_Policy_Tests tp =
    Tasty.testGroup
        "Protocol Policy Tests"
        [
            protocol_Policy_Redeemer_MintID_Tests tp
        ]

--------------------------------------------------------------------------------

protocol_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
protocol_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show Protocol_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = protocol_Create_TxContext tp
                in
                    [
                         -- Happy Path
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results) `assertResultsContainAnyOf` []

                        -- Testing invalid transaction range
                        , Tasty.testCase "Invalid transaction range must fail" $ do
                            let
                                invalidRange = createInValidRange (tpTransactionDate tp)
                                ctx' = ctx |> setValidyRange invalidRange
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isValidRange"]

                        -- Testing when UTxO is not consumed as input
                        , Tasty.testCase "Missing required UTxO input must fail" $ do
                            let
                                -- Remove the input UTxO that should be consumed
                                ctx' = ctx |> setInputsAndAddRedeemers []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isTxOutAnInput"]

                        -- Testing incorrect minting value
                        , Tasty.testCase "Incorrect minting value must fail" $ do
                            let
                                -- Try to mint 2 tokens instead of 1
                                ctx' = ctx |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 2, ProtocolT.mkMintIDRedeemer)]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isMintingID"]

                        -- Testing incorrect output Protocol datum version
                        , Tasty.testCase "Incorrect protocol version in output datum must fail" $ do
                            let
                                invalidDatum = (protocol_DatumType_MockData tp) { ProtocolT.pdProtocolVersion = 999 }
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum invalidDatum }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum"]

                        -- Testing incorrect output value (wrong minADA)
                        , Tasty.testCase "Incorrect min ADA in output value must fail" $ do
                            let
                                wrongMinAda = toAlter_minAda -- Different from the expected minADA 
                                invalidValue = LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1
                                             <> LedgerAda.lovelaceValueOf wrongMinAda
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutValue = invalidValue }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum_Value"]

                        -- Testing missing protocol output
                        , Tasty.testCase "Missing protocol output must fail" $ do
                            let
                                ctx' = ctx |> setOutputs []
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["Expected at least one output to scripts addresses"]

                        -- Testing output to wrong script address
                        , Tasty.testCase "Invalid protocol ID in output must fail" $ do
                            let
                                -- Create output with wrong protocol ID
                                invalidValue = LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) invalidID_TN 1
                                             <> LedgerAda.lovelaceValueOf minAdaProtocolDatum
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutValue = invalidValue }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["Expected Protocol at output to script index 0"]

                        -- Testing incorrect admin list
                        , Tasty.testCase "Unordered admin list in output datum must fail" $ do
                            let
                                -- Create datum with unordered admin list
                                unorderedAdmins = reverse $ sort (newAdmin : T.tpProtocolAdmins tp  )
                                invalidDatum = (protocol_DatumType_MockData tp) { ProtocolT.pdAdmins = unorderedAdmins }
                                invalidProtocolUTxO = (protocol_UTxO_MockData tp)
                                    { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ ProtocolT.mkDatum invalidDatum }
                                ctx' = ctx |> setOutputs [invalidProtocolUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just (RedeemerLogPolicy (Just Protocol_MintID_TestRedeemer)), results)
                                `assertResultsContainAnyOf` ["not isCorrect_Output_Protocol_Datum"]
                    ]


--------------------------------------------------------------------------------
