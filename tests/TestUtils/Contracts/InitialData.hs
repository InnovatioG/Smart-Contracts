--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

{- |
Module      : TestUtils.Contracts.InitialData
Description : Mock Data
-}

module TestUtils.Contracts.InitialData where

-- Non-IOG imports

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada               as LedgerAda
import qualified Plutus.V2.Ledger.Api     as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Campaign.Funds.Types     as CampaignFundsT
import qualified Campaign.Helpers         as CampaignHelpers
import qualified Campaign.Types           as CampaignT
import qualified Constants                as T
import qualified Helpers.OffChain         as OffChainHelpers
import qualified Helpers.Types            as T
import qualified Protocol.Types           as ProtocolT
import qualified TestUtils.TypesINNOVATIO as T

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

minAdaForUTxOWithTokens :: Integer
minAdaForUTxOWithTokens = 3_100_000

minAdaProtocolDatum :: Integer
minAdaProtocolDatum = 3_200_000

minAdaCampaignDatum :: Integer
minAdaCampaignDatum = 3_300_000

minAdaCampaignFundsDatum :: Integer
minAdaCampaignFundsDatum = 3_500_000

minAdaScriptDatum :: Integer
minAdaScriptDatum = 50_000_000

toAlter_minAda :: Integer
toAlter_minAda = 16_222_000

toAlter_Value_Adding_SomeADA :: LedgerApiV2.Value
toAlter_Value_Adding_SomeADA = LedgerAda.lovelaceValueOf 10_000_000

basicAddress :: Ledger.Address
basicAddress = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash "a2") Nothing

exampleValidatorHash :: LedgerApiV2.ValidatorHash
exampleValidatorHash = LedgerApiV2.ValidatorHash "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c916ed"



newTokenPolicy_CS :: LedgerApiV2.CurrencySymbol
newTokenPolicy_CS = LedgerApiV2.CurrencySymbol "00000000000000000000000000000000000000000000000000000000"

extraTxId :: LedgerApiV2.TxId
extraTxId = LedgerApiV2.TxId "00000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------

protocolAdmins :: [ T.WalletPaymentPKH]
protocolAdmins = ["0000000000000000000000000000000000000000000000000000000000000004"]

campaignAdmins :: [ T.WalletPaymentPKH]
campaignAdmins = ["0000000000000000000000000000000000000000000000000000000000000005"]

newAdmin :: T.WalletPaymentPKH
newAdmin = "0000000000000000000000000000000000000000000000000000000000000011"

tokenEmergencyAdminPolicy_CS ::LedgerApiV2.CurrencySymbol
tokenEmergencyAdminPolicy_CS = "0000000000000000000000000000000000000000000000000000000000000003"

tokenAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
tokenAdminPolicy_CS = "0000000000000000000000000000000000000000000000000000000000000002"

invalidID_TN :: T.TN
invalidID_TN = LedgerApiV2.TokenName "InvalidID"

campaignToken_TN :: T.TN
campaignToken_TN = "CampaignToken"

campaignToken_PriceADA :: Integer
campaignToken_PriceADA = 1_000_000

requestedMaxADA :: Integer
requestedMaxADA = 10_000_000

requestedMinADA :: Integer
requestedMinADA = 5_000_000

beginDate :: LedgerApiV2.POSIXTime
beginDate = 1

deadlineDate :: LedgerApiV2.POSIXTime
deadlineDate = LedgerApiV2.POSIXTime ((10 * 30 * 24 * 60 * 60 * 1000) :: Integer)


milestones :: CampaignT.CampaignMilestones
milestones = CampaignT.CampaignMilestones {
        CampaignT.cmPerncentage           = 100
        , CampaignT.cmStatus                =CampaignT.MsCreated
        }

transactionDate :: LedgerApiV2.POSIXTime
transactionDate = LedgerApiV2.POSIXTime ((2 * 30 * 24 * 60 * 60 * 1000) :: Integer)

deposit_MockData :: Integer
deposit_MockData = requestedMaxADA `divide`  campaignToken_PriceADA

withdraw_MockData :: Integer
withdraw_MockData = 100

collect_MockData :: Integer
collect_MockData = 100

--------------------------------------------------------------------------------

protocol_Datum_MockData :: T.TestParams -> LedgerApiV2.Datum
protocol_Datum_MockData tp = ProtocolT.mkDatum $ protocol_DatumType_MockData tp

protocol_DatumType_MockData :: T.TestParams -> ProtocolT.ProtocolDatumType
protocol_DatumType_MockData tp =
    ProtocolT.mkProtocol_DatumType
        (T.tpProtocolAdmins tp) -- pdAdmins
        (T.tpTokenAdminPolicy_CS tp) -- pdTokenAdminPolicy_CS
        minAdaProtocolDatum -- pdMinADA

protocol_UTxO_MockData :: T.TestParams -> LedgerApiV2.TxOut
protocol_UTxO_MockData tp =
    let
        datum = protocol_Datum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpProtocolValidator_Hash tp)
            (LedgerAda.lovelaceValueOf minAdaProtocolDatum <> LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
            (LedgerApiV2.OutputDatum datum)
            Nothing

-----------------

protocol_spend_UTxO_And_TxOutRef_MockData :: T.TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
protocol_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        basicAddress
        (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , T.tpProtocolPolicyID_TxOutRef tp
    )


--------------------------------------------------------------------------------

-- | Campaign DatumType that doesn't contain any CampaignFunds yet.
campaign_DatumType_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaign_DatumType_MockData tp =
    CampaignT.mkCampaign_DatumType
        (T.tpCampaignPolicy_CS tp) -- cdCampaignPolicy_CS
        (T.tpCampaignFundsPolicyID_CS tp) -- cdCampaignFundsPolicyID_CS
        (T.tpCampaignAdmins tp) -- cdAdmins
        (T.tpTokenAdminPolicy_CS tp) -- cdTokenAdminPolicy_CS
        False -- cdMint_CampaignToken
        (T.tpCampaignPolicy_CS tp) -- cdCampaignToken_CS
        (T.tpCampaignToken_TN tp) -- cdCampaignToken_TN
        (T.tpCampaignToken_PriceADA tp)-- cdCampaignToken_PriceADA
        (T.tpRequestedMaxADA tp)-- cdRequestedMaxADA
        (T.tpRequestedMinADA tp)-- cdRequestedMinADA
        0 -- (T.tpFundedADA       tp)
        0 -- (T.tpCollectedADA        tp)
        (T.tpBeginAt tp)-- cdBeginAt
        (T.tpDeadline tp)-- cdDeadline
        CampaignT.CsCreated -- cdStatus
        (T.tpMilestones tp)-- cdMilestones
        0-- cdFundsCount
        0-- cdFundsIndex
        minAdaCampaignDatum-- cdMinADA

-- | Campaign Datum that doesn't contain any CampaignFunds yet.
campaign_Datum_MockData :: T.TestParams -> LedgerApiV2.Datum
campaign_Datum_MockData tp = CampaignT.mkDatum $ campaign_DatumType_MockData tp

-- | Campaign UTxO that doesn't contain any CampaignFunds yet.
campaign_UTxO_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaign_UTxO_MockData tp =
    let
        datum = campaign_Datum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaCampaignDatum
                <> LedgerApiV2.singleton (T.tpCampaignPolicy_CS tp) T.campaignID_TN 1
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

-----------------

-- | Campaign DatumType that contains CampaignFunds.
campaign_DatumType_With_Added_CampaignFunds_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaign_DatumType_With_Added_CampaignFunds_MockData tp =
    (campaign_DatumType_MockData tp)
        { CampaignT.cdFundsCount = 1
        , CampaignT.cdFundsIndex = 1
        }

campaign_Datum_With_Added_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.Datum
campaign_Datum_With_Added_CampaignFunds_MockData tp = CampaignT.mkDatum $ campaign_DatumType_With_Added_CampaignFunds_MockData tp

-- | Campaign UTxO that contains CampaignFunds.
campaign_UTxO_With_Added_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaign_UTxO_With_Added_CampaignFunds_MockData tp =
    let
        datum = campaign_Datum_With_Added_CampaignFunds_MockData tp
    in
        (campaign_UTxO_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }


campaign_DatumType_Reached_With_Added_CampaignFunds_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaign_DatumType_Reached_With_Added_CampaignFunds_MockData tp =
    (campaign_DatumType_MockData tp)
        { CampaignT.cdFundsCount = 1
        , CampaignT.cdFundsIndex = 1
        , CampaignT.cdStatus = CampaignT.CsReached
        , CampaignT.cdFundedADA = requestedMaxADA
        }

campaign_Datum_Reached_With_Added_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.Datum
campaign_Datum_Reached_With_Added_CampaignFunds_MockData tp = CampaignT.mkDatum $ campaign_DatumType_Reached_With_Added_CampaignFunds_MockData tp


-- | Campaign UTxO that contains CampaignFunds.
campaign_UTxO_Reached_With_Added_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaign_UTxO_Reached_With_Added_CampaignFunds_MockData tp =
    let
        datum = campaign_Datum_Reached_With_Added_CampaignFunds_MockData tp
    in
        (campaign_UTxO_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

-----------------

campaign_DatumType_With_Deleted_CampaignFunds_MockData :: T.TestParams -> CampaignT.CampaignDatumType
campaign_DatumType_With_Deleted_CampaignFunds_MockData tp =
    (campaign_DatumType_With_Added_CampaignFunds_MockData tp)
        { CampaignT.cdFundsCount = CampaignT.cdFundsCount (campaign_DatumType_With_Added_CampaignFunds_MockData tp) -1
        }

campaign_Datum_With_Deleted_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.Datum
campaign_Datum_With_Deleted_CampaignFunds_MockData tp = CampaignT.mkDatum $ campaign_DatumType_With_Deleted_CampaignFunds_MockData tp

campaign_UTxO_With_Deleted_CampaignFunds_MockData :: T.TestParams -> LedgerApiV2.TxOut
campaign_UTxO_With_Deleted_CampaignFunds_MockData tp =
    let
        datum = campaign_Datum_With_Deleted_CampaignFunds_MockData tp
    in
        (campaign_UTxO_With_Added_CampaignFunds_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

-----------------

-- | UTxO that is spent in order to mint the Campaign ID.
campaign_spend_UTxO_And_TxOutRef_MockData :: T.TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
campaign_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        basicAddress
        (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , T.tpCampaignPolicy_TxOutRef tp
    )

--------------------------------------------------------------------------------

campaignFunds_DatumType_With_NoDeposits_MockData :: T.TestParams -> Integer -> CampaignFundsT.CampaignFundsDatumType
campaignFunds_DatumType_With_NoDeposits_MockData tp idx =
     CampaignFundsT.mkCampaignFunds_DatumType
       idx -- index
       (T.tpCampaignPolicy_CS tp) -- campaignPolicy_CS
       (T.tpCampaignFundsPolicyID_CS tp) -- campaignFundsPolicyID_CS
       0 -- subtotal_Avalaible_CampaignToken
       0 -- subtotal_Sold_CampaignToken
       0 -- subtotal_Avalaible_ADA
       0 -- subtotal_Collected_ADA
      minAdaCampaignFundsDatum  -- minADA

campaignFunds_Datum_With_NoDeposits_MockData :: T.TestParams -> Integer -> LedgerApiV2.Datum
campaignFunds_Datum_With_NoDeposits_MockData tp idx = CampaignFundsT.mkDatum $ campaignFunds_DatumType_With_NoDeposits_MockData tp idx

campaignFunds_UTxO_With_NoDeposits_MockData :: T.TestParams -> Integer -> LedgerApiV2.TxOut
campaignFunds_UTxO_With_NoDeposits_MockData tp idx =
    let
        datum = campaignFunds_Datum_With_NoDeposits_MockData tp idx
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (CampaignHelpers.mkCampaignFundsID_TN idx) 1
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

--------------------------------------------------------------------------------

campaignFunds_DatumType_With_Deposits_MockData :: T.TestParams -> Integer -> Integer ->CampaignFundsT.CampaignFundsDatumType
campaignFunds_DatumType_With_Deposits_MockData tp idx deposit =
     CampaignFundsT.mkCampaignFunds_DatumType
       idx -- index
       (T.tpCampaignPolicy_CS tp) -- campaignPolicy_CS
       (T.tpCampaignFundsPolicyID_CS tp) -- campaignFundsPolicyID_CS
       deposit-- subtotal_Avalaible_CampaignToken
       0 -- subtotal_Sold_CampaignToken
       0 -- subtotal_Avalaible_ADA
       0 -- subtotal_Collected_ADA
      minAdaCampaignFundsDatum  -- minADA

campaignFunds_Datum_With_Deposits_MockData :: T.TestParams -> Integer -> Integer ->LedgerApiV2.Datum
campaignFunds_Datum_With_Deposits_MockData tp idx deposit = CampaignFundsT.mkDatum $ campaignFunds_DatumType_With_Deposits_MockData tp idx deposit

campaignFunds_UTxO_With_Deposits_MockData :: T.TestParams -> Integer -> Integer -> LedgerApiV2.TxOut
campaignFunds_UTxO_With_Deposits_MockData tp idx  deposit =
    let
        datum = campaignFunds_Datum_With_Deposits_MockData tp idx deposit
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaCampaignFundsDatum
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (CampaignHelpers.mkCampaignFundsID_TN idx) 1
                <> LedgerApiV2.singleton (T.tpCampaignToken_CS tp) (T.tpCampaignToken_TN tp) deposit
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing


campaignFunds_DatumType_With_Deposits_And_ADA_MockData :: T.TestParams -> Integer -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
campaignFunds_DatumType_With_Deposits_And_ADA_MockData tp idx deposit adaAmount =
        let base = campaignFunds_DatumType_With_Deposits_MockData tp idx deposit
        in base { CampaignFundsT.cfdSubtotal_Avalaible_ADA = adaAmount }

campaignFunds_Datum_With_Deposits_And_ADA_MockData :: T.TestParams -> Integer -> Integer -> Integer ->LedgerApiV2.Datum
campaignFunds_Datum_With_Deposits_And_ADA_MockData tp idx deposit adaAmount =
    CampaignFundsT.mkDatum $ campaignFunds_DatumType_With_Deposits_And_ADA_MockData tp idx deposit adaAmount

campaignFunds_UTxO_With_Deposits_And_ADA_MockData :: T.TestParams -> Integer -> Integer -> Integer -> LedgerApiV2.TxOut
campaignFunds_UTxO_With_Deposits_And_ADA_MockData tp idx deposit adaAmount =
    let
        datum = campaignFunds_Datum_With_Deposits_And_ADA_MockData tp idx deposit adaAmount
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf (minAdaCampaignFundsDatum + adaAmount)
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (CampaignHelpers.mkCampaignFundsID_TN idx) 1
                <> LedgerApiV2.singleton (T.tpCampaignToken_CS tp) (T.tpCampaignToken_TN tp) deposit
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing



campaignFunds_DatumType_With_Deposits_ADA_And_Sold_MockData :: T.TestParams -> Integer -> Integer -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
campaignFunds_DatumType_With_Deposits_ADA_And_Sold_MockData tp idx deposit adaAmount soldAmount =
    let base = campaignFunds_DatumType_With_Deposits_And_ADA_MockData tp idx deposit adaAmount
    in base { CampaignFundsT.cfdSubtotal_Sold_CampaignToken = soldAmount }

campaignFunds_Datum_With_Deposits_ADA_And_Sold_MockData :: T.TestParams -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.Datum
campaignFunds_Datum_With_Deposits_ADA_And_Sold_MockData tp idx deposit adaAmount soldAmount =
    CampaignFundsT.mkDatum $ campaignFunds_DatumType_With_Deposits_ADA_And_Sold_MockData tp idx deposit adaAmount soldAmount

campaignFunds_UTxO_With_Deposits_ADA_And_Sold_MockData :: T.TestParams -> Integer -> Integer -> Integer -> Integer -> LedgerApiV2.TxOut
campaignFunds_UTxO_With_Deposits_ADA_And_Sold_MockData tp idx deposit adaAmount soldAmount =
    let
        datum = campaignFunds_Datum_With_Deposits_ADA_And_Sold_MockData tp idx deposit adaAmount soldAmount
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpCampaignFundsValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf (minAdaCampaignFundsDatum + adaAmount)
                <> LedgerApiV2.singleton (T.tpCampaignFundsPolicyID_CS tp) (CampaignHelpers.mkCampaignFundsID_TN idx) 1
                <> LedgerApiV2.singleton (T.tpCampaignToken_CS tp) (T.tpCampaignToken_TN tp) (deposit - soldAmount)
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

--------------------------------------------------------------------------------
