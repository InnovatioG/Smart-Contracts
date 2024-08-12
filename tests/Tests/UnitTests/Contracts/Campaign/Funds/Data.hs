{-# LANGUAGE ImportQualifiedPost #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

{- |
Module      : Contracts.Campaign.Funds.Data
Description : Mock Data and Auxiliary Functions for testing the CampaignFunds.
-}
module Contracts.Campaign.Funds.Data where

-- IOG imports
import qualified Ledger.Ada                  as LedgerAda
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Helpers.OffChain     as OffChainHelpers
import qualified Helpers.OnChain      as OnChainHelpers
import qualified Constants          as T
import qualified Campaign.Helpers       as CampaignHelpers
import qualified Campaign.Funds.Types as CampaignFundsT
import qualified Campaign.Types         as CampaignT
import qualified Types              as T
import qualified TestUtils.Types as T
import qualified Helpers.OffChainEval as OffChainEval
import qualified TestUtils.Common as TestUtilsCommon


-- ScriptContexts
--------------------------------------------------------------------------------

-- Deposit Operation

depositContext :: T.TestParams -> LedgerApiV2.ScriptContext
depositContext tp =
    mkBaseValidatorContext
        tp
        [fundHoldingUTxO_MockData tp]  
        [fundHolding_UTxO_With_Deposit_MockData tp]
        0
        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) deposit_MockData)
        OffChainEval.|> OffChainEval.setRefInputs [campaignUTxO_MockData tp, investUnitUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpDepositDate tp))

--------------------------------------------------------------------------------

-- Withdraw Operation

withdrawContext :: T.TestParams -> LedgerApiV2.ScriptContext
withdrawContext tp =
    let
        (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (T.tpWithdrawDate tp) withdraw_MockData
    in
        mkBaseValidatorContext
            tp
            [fundHolding_UTxO_With_Deposit_MockData tp]
            [fundHolding_UTxO_With_Withdraw_MockData tp]
            0
            OffChainEval.|> OffChainEval.setMint
                ( LedgerApiV2.singleton
                    (T.tpCampaignPolicy_CS tp)
                    (T.tpFundFT_TN tp)
                    (-withdrawPlusCommissionsGetBack_MockData)
                )
            OffChainEval.|> OffChainEval.setRefInputs [campaignUTxO_MockData tp, investUnitUTxO_MockData tp]
            OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpWithdrawDate tp))

--------------------------------------------------------------------------------
-- Collect Protocol Commissions Operation

collect_Protocol_Commissions_Context :: T.TestParams -> LedgerApiV2.ScriptContext
collect_Protocol_Commissions_Context tp =
    OffChainHelpers.mkBaseValidatorContext tp [fundHolding_UTxO_With_Deposit_MockData tp] [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData] 0
        OffChainEval.|> OffChainEval.setRefInputs [protocolUTxO_MockData tp, campaignUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setSignatories (T.tpProtocolAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------
-- Collect Managers Commissions Operation

collect_Managers_Commissions_Context :: T.TestParams -> LedgerApiV2.ScriptContext
collect_Managers_Commissions_Context tp =
    OffChainHelpers.mkBaseValidatorContext tp [fundHolding_UTxO_With_Deposit_MockData tp] [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData] 0
        OffChainEval.|> OffChainEval.setRefInputs [protocolUTxO_MockData tp, campaignUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------
-- Collect Delegator Commissions Operation

collect_Delegators_Commissions_Context :: T.TestParams -> LedgerApiV2.ScriptContext
collect_Delegators_Commissions_Context tp =
    OffChainHelpers.mkBaseValidatorContext tp [fundHolding_UTxO_With_Deposit_MockData tp] [fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData] 0
        OffChainEval.|> OffChainEval.setRefInputs [protocolUTxO_MockData tp, campaignUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setSignatories (T.tpDelegatorsAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpCollectCommissionsDate tp))

--------------------------------------------------------------------------------

-- ReIndexing Operation

reIndexContext :: T.TestParams -> LedgerApiV2.ScriptContext
reIndexContext tp =
    mkBaseValidatorContext
        tp
        [fundHolding_UTxO_With_Deposit_MockData tp, investUnitUTxO_MockData tp]
        [output_ReIdx_CampaignFundsUTxO_MockData tp]
        0
        OffChainEval.|> OffChainEval.setRefInputs [campaignUTxOWithFunds_MockData tp]
        OffChainEval.|> setInfoRedeemers
            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 1]
            [ InvestUnitgT.mkReIndexingRedeemer
                investUnitAfterReIdx
                investUnitInitial
                (oracleData tp)
                (oracleSignature tp)
            ]
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpReIdxDate tp))

    
--------------------------------------------------------------------------------
-- Delete Operation

-- | LedgerApiV2.ScriptContext for successfully deleting a Campaign Funds.
deleteContext :: T.TestParams -> LedgerApiV2.ScriptContext
deleteContext tp =
    mkBaseValidatorContext
        tp
        [campaignUTxOWithFunds_MockData tp, fundHolding_UTxO_With_Deposit_MockData tp]
        [campaignUTxO_MockData tp]
        0
        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) $ -1)
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

--------------------------------------------------------------------------------
-- Mint CampaignFundsID Operation

-- | Script Context for successfully minting a CampaignFunds ID.
mintFHIDSuccessfulContext :: T.TestParams -> LedgerApiV2.ScriptContext
mintFHIDSuccessfulContext tp =
    OffChainEval.mkBaseValidMintingPolicyContext tp [campaignUTxO_MockData tp] [campaignUTxO_MockData tp, fundHoldingUTxO_MockData tp] (T.tpCampaignFundsPolicyID_CS tp)
        OffChainEval.|> setInfoRedeemers
            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
            [CampaignT.mkCampaignFundsAddRedeemer]
        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) 1)
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

--------------------------------------------------------------------------------
-- Burn CampaignFundsID Operation

-- | Script Context for successfully minting a CampaignFunds ID.
burnFHIDSuccessfulContext :: T.TestParams -> LedgerApiV2.ScriptContext
burnFHIDSuccessfulContext tp =
    OffChainEval.mkBaseValidMintingPolicyContext tp [fundHoldingUTxO_MockData tp, campaignUTxOWithFunds_MockData tp] [campaignUTxO_MockData tp] (T.tpCampaignFundsPolicyID_CS tp)
        OffChainEval.|> setInfoRedeemers
            [LedgerApiV2.Spending $ LedgerApiV2.TxOutRef someTxId 0]
            [CampaignT.mkCampaignFundsDeleteRedeemer]
        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) (negate 1))
        OffChainEval.|> OffChainEval.setSignatories (T.tpCampaignAdmins tp)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

--------------------------------------------------------------------------------



fundHolding_DatumType_With_Deposit_MockData :: T.TestParams -> CampaignFundsT.CampaignFundsDatumType
fundHolding_DatumType_With_Deposit_MockData tp =
    let
        (userFT_MockData, commissionsFT_MockData, depositCommsRPMNum1e6_MockData) = calculateDepositCommissionsUsingMonths_ tp (T.tpDepositDate tp) deposit_MockData
    in
        CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Deposit (fundHoldingDatumType_MockData tp) deposit_MockData userFT_MockData commissionsFT_MockData depositCommsRPMNum1e6_MockData

-- CampaignFundsT.mkCampaignFundsDatumType
--     0 -- cfdIndex
--     (userFT_MockData + commissionsFT_MockData + 100) -- cfdSubtotal_Avalaible_FT -- puedo sumar cualquier valor, este acumulador es solo de muestra
--     (userFT_MockData + commissionsFT_MockData) -- cfdSubtotal_Sold_FT 
--     userFT_MockData -- hdSubtotal_FT_Circulation
--     commissionsFT_MockData -- hdSubtotal_FT_ForComission
--     (commissionsFT_MockData + 10) -- hdSubtotal_FT_ForComission_Acumulated -- puedo sumar cualquier valor, este acumulador es solo de muestra
--     depositCommsRPMNum1e6_MockData -- hdSubtotal_Commissions_RatePerMonth_Numerator1e6
--     10 -- hdSubtotal_Collected_Commissions_Protocol
--     10 -- hdSubtotal_Collected_Commissions_Delegators
--     10 -- hdSubtotal_Collected_Commissions_Managers
--     minAdaCampaignFundsDatum -- cfdMinADA

fundHolding_Datum_With_Deposit_MockData :: T.TestParams -> LedgerApiV2.Datum
fundHolding_Datum_With_Deposit_MockData tp = CampaignFundsT.mkDatum $ fundHolding_DatumType_With_Deposit_MockData tp

fundHolding_UTxO_With_Deposit_MockData :: T.TestParams -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Deposit_MockData tp =
    let
        (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (T.tpDepositDate tp) deposit_MockData
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                <> LedgerApiV2.singleton investUnitInitial_Token_CS investUnitInitial_Token_TN ((deposit_MockData * investUnitInitial_Token_Amount) `divide` 100)
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) 1
                <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) commissionsFT_MockData
            )
            (LedgerApiV2.OutputDatum (fundHolding_Datum_With_Deposit_MockData tp))
            Nothing

-------------------

fundHolding_DatumType_With_Withdraw_MockData :: T.TestParams -> CampaignFundsT.CampaignFundsDatumType
fundHolding_DatumType_With_Withdraw_MockData tp =
    let
        (commissionsFTToGetBack_MockData, _, withdrawCommsRPMNum1e6_MockData) = calculateWithdrawCommissionsUsingMonths_ tp (T.tpWithdrawDate tp) withdraw_MockData
    in
        CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Withdraw (fundHolding_DatumType_With_Deposit_MockData tp) withdraw_MockData commissionsFTToGetBack_MockData withdrawCommsRPMNum1e6_MockData

fundHolding_Datum_With_Withdraw_MockData :: T.TestParams -> LedgerApiV2.Datum
fundHolding_Datum_With_Withdraw_MockData tp = CampaignFundsT.mkDatum $ fundHolding_DatumType_With_Withdraw_MockData tp

fundHolding_UTxO_With_Withdraw_MockData :: T.TestParams -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Withdraw_MockData tp =
    let
        (commissionsFTToGetBack_MockData, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (T.tpWithdrawDate tp) withdraw_MockData
    in
        (fundHolding_UTxO_With_Deposit_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                    <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
                    <> LedgerApiV2.singleton investUnitInitial_Token_CS investUnitInitial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnitInitial_Token_Amount) `divide` 100))
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (fundHolding_Datum_With_Withdraw_MockData tp)
            }

-------------------

fundHolding_UTxO_With_Collected_Protocol :: T.TestParams -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData' =
    let
        newDatumType = CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Collect_Protocol_Commission (fundHolding_DatumType_With_Deposit_MockData tp) withdraw_Commissions_MockData' 
    in
        (fundHolding_UTxO_With_Deposit_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                    <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (-withdraw_Commissions_MockData' )
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum newDatumType
            }

fundHolding_UTxO_With_Collected_Managers :: T.TestParams ->  Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData' =
    let
        newDatumType = CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Collect_Managers_Commission (fundHolding_DatumType_With_Deposit_MockData tp) withdraw_Commissions_MockData' 
    in
        (fundHolding_UTxO_With_Deposit_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                    <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (-withdraw_Commissions_MockData' )
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum newDatumType
            }

fundHolding_UTxO_With_Collected_Delegators :: T.TestParams ->  Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData' =
    let
        newDatumType = CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Collect_Delegators_Commission (fundHolding_DatumType_With_Deposit_MockData tp) withdraw_Commissions_MockData' 
    in
        (fundHolding_UTxO_With_Deposit_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposit_MockData tp)
                    <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) (T.tpFundFT_TN tp) (-withdraw_Commissions_MockData' )
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ CampaignFundsT.mkDatum newDatumType
            }




-- Campaign Funds Contract Mock Data
--------------------------------------------------------------------------------

deposit_MockData :: Integer
deposit_MockData = 20_000_000

withdraw_MockData :: Integer
withdraw_MockData = 10_000_000

withdraw_Commissions_MockData :: Integer
withdraw_Commissions_MockData = 10

-- Campaign Funds Contract) UTxOs
--------------------------------------------------------------------------------

fundHoldingDatumType_MockData :: T.TestParams -> CampaignFundsT.CampaignFundsDatumType
fundHoldingDatumType_MockData _ =
    CampaignFundsT.mkCampaignFundsDatumType
        0 -- cfdIndex
        0 -- cfdSubtotal_Avalaible_FT
        0 -- cfdSubtotal_Sold_FT 
        0 -- hdSubtotal_FT_Circulation
        0 -- hdSubtotal_FT_ForComission
        0 -- hdSubtotal_FT_ForComission_Acumulated
        0 -- hdSubtotal_Commissions_RatePerMonth_Numerator1e6
        0 -- hdSubtotal_Collected_Commissions_Protocol
        0 -- hdSubtotal_Collected_Commissions_Delegators
        0 -- hdSubtotal_Collected_Commissions_Managers
        minAdaCampaignFundsDatum -- cfdMinADA

campaignFundsDatum_MockData :: T.TestParams -> LedgerApiV2.Datum
campaignFundsDatum_MockData tp = CampaignFundsT.mkDatum $ fundHoldingDatumType_MockData tp

fundHoldingUTxO_MockData :: T.TestParams -> LedgerApiV2.TxOut
fundHoldingUTxO_MockData tp =
    let
        datum = campaignFundsDatum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (TestUtilsCommon.mkCampaignFundsID_TN 0) 1
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

-----
