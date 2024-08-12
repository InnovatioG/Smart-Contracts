{- |
Module      : Contracts.Campaign.Data
Description : Mock Data and Auxiliary Functions for testing the Fund.
-}
module Contracts.Campaign.Data where

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                    as LedgerAda
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Helpers.OffChainEval          as OffChainEval
import qualified Helpers.OffChain       as OffChainHelpers
import qualified Helpers.OnChain        as OnChainHelpers
import qualified Campaign.Funds.Types as CampaignFundsT
import qualified Campaign.Helpers     as CampaignHelpers
import qualified Campaign.Types       as CampaignT
import qualified Constants            as T
import qualified Protocol.Types       as ProtocolT
import qualified TestUtils.Constants           as T
import qualified TestUtils.Types               as T
import qualified TestUtils.Common               as TestUtilsCommon
import qualified Contracts.Protocol.Data as T

--------------------------------------------------------------------------------
-- Campaign Contract Data
--------------------------------------------------------------------------------

-- LedgerApiV2.ScriptContexts
--------------------------------------------------------------------------------

-- | Successful LedgerApiV2.ScriptContext for updating the Campaign Datum.
updateCampaignDatumContext :: T.TestParams -> LedgerApiV2.ScriptContext
updateCampaignDatumContext tp =
    OffChainEval.mkBaseValidatorContext [campaignUTxO_MockData tp] [campaignUTxO_MockData tp] 0
        OffChainEval.|> OffChainEval.setInputsAndAddRedeemers [(campaignUTxO_MockData tp, CampaignT.mkDatumUpdateRedeemer)]
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

deleteCampaignDatumContext :: T.TestParams -> LedgerApiV2.ScriptContext
deleteCampaignDatumContext tp =
    OffChainEval.mkBaseValidatorContext [] [] 0
        OffChainEval.|> OffChainEval.setInputsAndAddRedeemers [(campaignUTxO_MockData tp, CampaignT.mkDeleteRedeemer)]
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setMintAndRedeemers
            [
                ( LedgerApiV2.singleton
                    (CampaignT.cdCampaignPolicy_CS (campaignDatumType_MockData tp))
                    T.campaignID_TN
                    (negate 1)
                , CampaignT.mkBurnIDRedeemer
                )
            ]
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

-- | Successful LedgerApiV2.ScriptContext for adding a Campaign Funds.
addCampaignFundsContext :: T.TestParams -> LedgerApiV2.ScriptContext
addCampaignFundsContext tp =
    OffChainEval.mkBaseValidatorContext [campaignUTxO_MockData tp] [campaignUTxOWithFunds_MockData tp] 0
        OffChainEval.|> OffChainEval.setMint
            ( LedgerApiV2.singleton
                (T.tpCampaignFundsPolicyID_CS tp)
                (TestUtilsCommon.mkCampaignFundsID_TN 0)
                1
            )
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

-- | Script Context for successfully minting a Campaign ID.
mintCampaignIDContext :: T.TestParams -> LedgerApiV2.ScriptContext
mintCampaignIDContext tp =
    OffChainEval.mkBaseValidMintingPolicyContext [] [campaignUTxO_MockData tp] (T.tpCampaignPolicy_CS tp)
        OffChainEval.|> OffChainEval.setInputWithRef (campaign_spend_UTxO_And_TxOutRef_MockData tp)
        OffChainEval.|> OffChainEval.setRefInputs [T.protocolUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setMint
            ( LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) T.campaignID_TN 1
            )
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

-- | Script Context for successfully burning a Campaign ID.
burnCampaignIDContext :: T.TestParams -> LedgerApiV2.ScriptContext
burnCampaignIDContext tp =
    OffChainEval.mkBaseValidMintingPolicyContext [] [campaignUTxO_MockData tp] (T.tpCampaignPolicy_CS tp)
        OffChainEval.|> OffChainEval.setMint
            ( LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) T.campaignID_TN (negate 1)
            )
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

-- -- | Script Context for successfully minting Fund Tokens.
-- mintFTSuccessfulContext :: T.TestParams -> LedgerApiV2.ScriptContext
-- mintFTSuccessfulContext tp =
--     OffChainEval.mkBaseValidMintingPolicyContext tp [fundHoldingUTxO_MockData tp] [] (T.tpCampaignPolicy_CS tp)
--         OffChainEval.|> OffChainEval.setRefInputs [campaignUTxO_MockData tp]
--         OffChainEval.|> setInfoRedeemers
--             [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
--             [CampaignFundsT.mkDepositRedeemer 10 20]
--         OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) 3)
--         OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpDepositDate tp))

-- -- | Script Context for successfully burning Fund Tokens.
-- burnFTSuccessfulContext :: T.TestParams -> LedgerApiV2.ScriptContext
-- burnFTSuccessfulContext tp =
--     OffChainEval.mkBaseValidMintingPolicyContext tp [fundHoldingUTxO_MockData tp] [] (T.tpCampaignPolicy_CS tp)
--         OffChainEval.|> OffChainEval.setRefInputs [campaignUTxO_MockData tp]
--         OffChainEval.|> setInfoRedeemers
--             [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
--             [CampaignFundsT.mkWithdrawRedeemer 10 20]
--         OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (negate 2))
--         OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpWithdrawDate tp))

--------------------------------------------------------------------------------


-- | Campaign DatumType that doesn't contain any Campaign Funds yet.
campaignDatumType_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaignDatumType_MockData tp =
    CampaignT.mkCampaignDatumType
        (T.tpCampaignPolicy_CS tp) -- cdCampaignPolicy_CS
        (T.tpCampaignFundsPolicyID_CS tp) -- cdCampaignFundsPolicyID_CS
        (T.tpCampaignAdmins tp) -- cdAdmins
        (T.tpTokenAdminPolicy_CS tp) -- cdTokenAdminPolicy_CS
        (T.tpMint_CampaignFT tp)-- cdMint_CampaignFT
        (T.tpCampaignPolicy_CS tp) -- cdCampaignFT_CS
        (T.tpCampaignFT_TN tp) -- cdCampaignFT_TN
        (T.tpCampaignFT_PriceADA tp)-- cdCampaignFT_PriceADA
        (T.tpRequestedMaxADA tp)-- cdRequestedMaxADA
        (T.tpRequestedMinADA tp)-- cdRequestedMinADA
        (T.tpFundedADA       tp)
        (T.tpCollectedADA        tp)
        (T.tpBeginAt tp)-- cdBeginAt
        (T.tpDeadline tp)-- cdDeadline
        (T.tpStatus tp)-- cdStatus
        (T.tpMilestones tp)-- cdMilestones
        0-- cdFundsCount
        0-- cdFundsIndex
        T.minAdaCampaignDatum-- cdMinADA

-- | Campaign Datum that doesn't contain any Campaign Funds yet.
campaignDatum_MockData :: T.TestParams -> LedgerApiV2.Datum
campaignDatum_MockData tp = CampaignT.mkDatum $ campaignDatumType_MockData tp

-- | Campaign UTxO that doesn't contain any Campaign Funds yet.
campaignUTxO_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaignUTxO_MockData tp =
    let
        datum = campaignDatum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf T.minAdaCampaignDatum
                <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) T.campaignID_TN 1
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

-- | Campaign DatumType that contains Campaign Funds.
campaignDatumTypeWithHolding_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaignDatumTypeWithHolding_MockData tp =
    (campaignDatumType_MockData tp)
        { CampaignT.cdFundsCount = 1
        , CampaignT.cdFundsIndex = 1
        }

campaignDatumWithFunds_MockData :: T.TestParams -> LedgerApiV2.Datum
campaignDatumWithFunds_MockData tp = CampaignT.mkDatum $ campaignDatumTypeWithHolding_MockData tp

-- | Campaign UTxO that contains Campaign Funds.
campaignUTxOWithFunds_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaignUTxOWithFunds_MockData tp =
    let
        datum = campaignDatumWithFunds_MockData tp
    in
        (campaignUTxO_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

-----------------

-- | UTxO that is spent in order to mint the Campaign ID.
campaign_spend_UTxO_And_TxOutRef_MockData :: T.TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
campaign_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        T.basicAddress
        (LedgerAda.lovelaceValueOf T.minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , T.tpCampaignPolicy_TxOutRef tp
    )
