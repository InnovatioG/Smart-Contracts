{- |
Module      : Fund.MintingPolicy
Description : Validation logic and tests related to the Fund minting policy.

This module defines the validation logic for the Fund's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Campaign.MintingPolicy where

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

import qualified Campaign.Funds.Types    as CampaignFundsT
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

--------------------------------------------------------------------------------

campaignMPTests :: T.TestParams -> Tasty.TestTree
campaignMPTests tp =
    Tasty.testGroup
        "Testing Campaign Minting Policy"
        [ mintingIDTests tp
        , burningIDTests tp
        , mintingFTTests tp
        , burningFTTests tp
        ]

--------------------------------------------------------------------------------
-- MINTING Campaign ID TOKEN
--------------------------------------------------------------------------------

mintingIDTests :: T.TestParams -> Tasty.TestTree
mintingIDTests tp =
    Tasty.testGroup
        "Tests for Campaign ID minting"
        [ Tasty.testCase "Minting ID successful case" $
            let
                ctx = mintCampaignIDContext tp
            in
                (OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx Tasty.@?= [])
        , Tasty.testCase "Not including Protocol input ref should fail" $
            let
                ctx = mintCampaignIDContext tp OffChainEval.|> OffChainEval.setRefInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Protocol input ref"]
                )
        , Tasty.testCase "Not including Campaign output should fail" $
            let
                ctx = mintCampaignIDContext tp OffChainEval.|> OffChainEval.setOutputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected Campaign at output index 0"]
                )
        , Tasty.testCase "Not including utxo in policy parameter as input should fail" $
            let
                ctx = mintCampaignIDContext tp OffChainEval.|> OffChainEval.setInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isTxOutAnInput"]
                )
        , Tasty.testCase "Not minting Campaign ID should fail" $
            let
                ctx =
                    mintCampaignIDContext tp

            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingIDs"]
                )
        , Tasty.testCase "Not minting Invest Unit ID NFT should fail" $
            let
                ctx =
                    mintCampaignIDContext tp
                        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) T.campaignID_TN 1)
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingIDs"]
                )
        , Tasty.testCase "Campaign UTXO address different from the one in policy param should fail" $
            let
                campaignUTxO_MockData' =
                    (campaignUTxO_MockData tp)
                        { LedgerApiV2.txOutAddress =
                            OffChainHelpers.addressValidator $
                                LedgerApiV2.ValidatorHash
                                    "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
                        }
                ctx =
                    mintCampaignIDContext tp
                        OffChainEval.|> OffChainEval.setOutputs [campaignUTxO_MockData', investUnitUTxO_MockData tp]
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected Campaign at output index 0"]
                )
        , Tasty.testCase "Having initial Campaign Funds count different from 0 should fail" $
            let
                campaignDatumType' =
                    (campaignDatumType_MockData tp)
                        { CampaignT.cdFundsCount = 1
                        }
                campaignUTxO_MockData' =
                    (campaignUTxO_MockData tp)
                        { LedgerApiV2.txOutDatum =
                            LedgerApiV2.OutputDatum $ CampaignT.mkDatum campaignDatumType'
                        }
                ctx =
                    mintCampaignIDContext tp
                        OffChainEval.|> OffChainEval.setOutputs [campaignUTxO_MockData', investUnitUTxO_MockData tp]
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignDatum"]
                )

        ]

--------------------------------------------------------------------------------
-- BURNING Campaign ID TOKEN
--------------------------------------------------------------------------------

burningIDTests :: T.TestParams -> Tasty.TestTree
burningIDTests tp =
    Tasty.testGroup
        "Tests for Campaign ID burning"
        [ Tasty.testCase "Burning ID successful case" $
            let
                ctx = burnCampaignIDContext tp
            in
                (OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnIDRedeemer ctx Tasty.@?= [])
        , Tasty.testCase "Not burning Campaign ID should fail" $
            let
                ctx =
                    burnCampaignIDContext tp

            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningIDs"]
                )
        , Tasty.testCase "Not burning invest unit ID NFT should fail" $
            let
                ctx =
                    burnCampaignIDContext tp
                        OffChainEval.|> OffChainEval.setMint
                            ( LedgerApiV2.singleton
                                (T.tpCampaignPolicy_CS tp)
                                T.campaignID_TN
                                (negate 1)
                            )
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnIDRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningIDs"]
                )
        ]

--------------------------------------------------------------------------------
-- MINTING FUND FT TOKEN
--------------------------------------------------------------------------------

mintingFTTests :: T.TestParams -> Tasty.TestTree
mintingFTTests tp =
    Tasty.testGroup
        "Tests for fund FT minting"
        [ Tasty.testCase "Minting FT successful case" $
            let
                ctx = mintFTSuccessfulContext tp
            in
                (OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintFTRedeemer ctx Tasty.@?= [])
        , Tasty.testCase "Not minting FT should fail" $
            let
                ctx = mintFTSuccessfulContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isMintingFT"]
                )
        , Tasty.testCase "Not having Campaign UTxO as ref input should fail" $
            let
                ctx = mintFTSuccessfulContext tp OffChainEval.|> OffChainEval.setRefInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Campaign input ref"]
                )
        , Tasty.testCase "Not having Campaign Funds UTxO as input should fail" $
            let
                ctx = mintFTSuccessfulContext tp OffChainEval.|> OffChainEval.setInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one CampaignFunds input"]
                )
        , Tasty.testCase "Incorrect redeemer for Campaign Funds UTxO should fail" $
            let
                ctx =
                    mintFTSuccessfulContext tp
                        OffChainEval.|> setInfoRedeemers
                            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
                            [CampaignFundsT.mkWithdrawRedeemer 10 20] --
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkMintFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Redeemer_CampaignFundsDatum"]
                )
        ]

--------------------------------------------------------------------------------
-- BURNING FUND FT TOKEN
--------------------------------------------------------------------------------

burningFTTests :: T.TestParams -> Tasty.TestTree
burningFTTests tp =
    Tasty.testGroup
        "Tests for fund FT burning"
        [ Tasty.testCase "Burning FT successful case" $
            let
                ctx = burnFTSuccessfulContext tp
            in
                (OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnFTRedeemer ctx Tasty.@?= [])
        , Tasty.testCase "Not burning FT should fail" $
            let
                ctx = burnFTSuccessfulContext tp OffChainEval.|> OffChainEval.setMint mempty
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isBurningFT"]
                )
        , Tasty.testCase "Not having Campaign UTxO as ref input should fail" $
            let
                ctx = burnFTSuccessfulContext tp OffChainEval.|> OffChainEval.setRefInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Campaign input ref"]
                )
        , Tasty.testCase "Not having Campaign Funds UTxO as input should fail" $
            let
                ctx = burnFTSuccessfulContext tp OffChainEval.|> OffChainEval.setInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one CampaignFunds input"]
                )
        , Tasty.testCase "Incorrect redeemer for Campaign Funds UTxO should fail" $
            let
                ctx =
                    burnFTSuccessfulContext tp
                        OffChainEval.|> setInfoRedeemers
                            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
                            [CampaignFundsT.mkDepositRedeemer 10 20] --
            in
                ( OffChainEval.evaluateScriptPolicyEX (T.tpCampaignPolicy tp) CampaignT.mkBurnFTRedeemer ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Redeemer_CampaignFundsDatum"]
                )
        ]

--------------------------------------------------------------------------------
