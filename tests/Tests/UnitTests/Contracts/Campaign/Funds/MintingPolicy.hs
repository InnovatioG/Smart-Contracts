{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Fund.Holding.MintingPolicy
Description : Validation logic and tests related to the CampaignFunds
              minting policy.

This module defines the validation logic for the CampaignFunds's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Campaign.Funds.MintingPolicy
    ( fundHoldingMPTests
    ) where

-- Non-IOG imports
import qualified Test.Tasty                    as Tasty
import qualified Test.Tasty.HUnit              as Tasty

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                    as LedgerAda
import qualified Ledger.Address                as LedgerAddress
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Campaign.Funds.Types          as CampaignFundsT
import qualified Campaign.Types                as CampaignT
import           Contracts.Campaign.Funds.Data
import qualified Helpers.OffChain              as OffChainHelpers (addressValidator)
import qualified Protocol.PABTypes             as T
import           TestUtils.Common
import           TestUtils.Constants
import           TestUtils.Types

--------------------------------------------------------------------------------

fundHoldingMPTests :: T.TestParams -> Tasty.TestTree
fundHoldingMPTests tp =
    Tasty.testGroup
        "Tests for CampaignFunds ID minting"
        [ Tasty.testCase "Successful case" $
            let
                ctx = mintFHIDSuccessfulContext tp
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    Tasty.@?= []
                )
        , Tasty.testCase "Not including Campaign UTxO input should fail" $
            let
                ctx = mintFHIDSuccessfulContext tp OffChainEval.|> OffChainEval.setInputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected exactly one Campaign input"]
                )
        , Tasty.testCase "Not including CampaignFunds UTxO output should fail" $
            let
                ctx = mintFHIDSuccessfulContext tp OffChainEval.|> OffChainEval.setOutputs []
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected at least two outputs to scripts addresses"]
                )
        , Tasty.testCase "Not including AddCampaignFunds Redeemer should fail" $
            let
                ctx = mintFHIDSuccessfulContext tp OffChainEval.|> setInfoRedeemers [] []
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Redeemer_CampaignDatum"]
                )
        , Tasty.testCase "Including a wrong CampaignFunds UTxO Datum should fail" $
            let
                ctx =
                    mintFHIDSuccessfulContext tp
                        OffChainEval.|> OffChainEval.setOutputs [campaignUTxO_MockData tp, wrongUTxO]
                wrongDatum =
                    CampaignFundsT.mkCampaignFundsDatumType
                        10 -- cfdIndex
                        0 -- cfdSubtotal_Avalaible_FT
                        0 -- cfdSubtotal_Sold_FT
                        0 -- hdSubtotal_FT_Circulation
                        0 -- hdSubtotal_FT_ForComission
                        22 -- hdSubtotal_FT_ForComission_Acumulated
                        0 -- hdSubtotal_Commissions_RatePerMonth_Numerator1e6
                        0 -- hdSubtotal_Collected_Commissions_Protocol
                        0 -- hdSubtotal_Collected_Commissions_Delegators
                        0 -- hdSubtotal_Collected_Commissions_Managers
                        minAdaCampaignFundsDatum

                wrongUTxO =
                    (fundHoldingUTxO_MockData tp)
                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum wrongDatum
                        }
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum"]
                )
        , Tasty.testCase "Including a wrong CampaignFunds UTxO Value should fail" $
            let
                ctx = mintFHIDSuccessfulContext tp OffChainEval.|> OffChainEval.setOutputs [campaignUTxO_MockData tp, wrongUTxO]
                wrongUTxO =
                    (fundHoldingUTxO_MockData tp)
                        { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (fundHoldingUTxO_MockData tp) <> toAlter_Value_Adding_SomeADA
                        }
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["not isCorrect_Output_CampaignFundsDatum_Value"]
                )
        , Tasty.testCase "Including a wrong CampaignFunds UTxO Address should fail" $
            let
                ctx = mintFHIDSuccessfulContext tp OffChainEval.|> OffChainEval.setOutputs [campaignUTxO_MockData tp, wrongUTxO]
                wrongUTxO =
                    (fundHoldingUTxO_MockData tp)
                        { LedgerApiV2.txOutAddress  = OffChainHelpers.addressValidator T.exampleValidatorHash
                        }
            in
                ( OffChainEval.evaluateScriptPolicyEX
                    (T.tpCampaignFundsPolicyID tp)
                    CampaignFundsT.mkMintIDRedeemer
                    ctx
                    `OffChainEval.logAassertContainsAnyOf` ["Expected Campaign Funds at output index 1"]
                )
        ]
