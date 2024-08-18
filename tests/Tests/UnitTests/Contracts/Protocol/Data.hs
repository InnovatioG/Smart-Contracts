{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module      : Contracts.Protocol.Data
Description : Mock Data and Auxiliary Functions for testing the Protocol.
-}
module Contracts.Protocol.Data where

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada              as LedgerAda
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import           PlutusTx.Prelude
import qualified Prelude                 as P

-- Project imports
import qualified Helpers.OffChainEval    as OffChainEval
import qualified Helpers.OffChain as OffChainHelpers
import qualified Constants      as T
import qualified Protocol.Types as ProtocolT
import qualified TestUtils.Constants     as T
import qualified TestUtils.Types         as T

--------------------------------------------------------------------------------
-- Protocol Contract Data
--------------------------------------------------------------------------------

-- Script Context
--------------------------------------------------------------------------------

-- | LedgerApiV2.ScriptContext succesful update ctx
updateProtocolContext :: T.TestParams -> LedgerApiV2.ScriptContext
updateProtocolContext tp  =
    OffChainEval.mkBaseValidatorContext [] [] 0
        OffChainEval.|> OffChainEval.setSignatories (T.tpProtocolAdmins tp)
        OffChainEval.|> OffChainEval.setInputsAndAddRedeemers [(protocolUTxO_MockData tp, ProtocolT.mkDatumUpdateRedeemer)]
        OffChainEval.|> OffChainEval.setSpendPurpose 0
        OffChainEval.|> OffChainEval.setOutputs [protocolUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

-- | Successful LedgerApiV2.ScriptContext for minting the Protocol ID.
mintProtocolIDContext :: T.TestParams -> LedgerApiV2.ScriptContext
mintProtocolIDContext tp =
    OffChainEval.mkBaseValidMintingPolicyContext [] [] (T.tpProtocolPolicyID_CS tp)
        OffChainEval.|> OffChainEval.setInputWithRef (protocol_spend_UTxO_And_TxOutRef_MockData tp)
        OffChainEval.|> OffChainEval.setOutputs [protocolUTxO_MockData tp]
        OffChainEval.|> OffChainEval.setMint (LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
        OffChainEval.|> OffChainEval.setValidRange (OffChainEval.createValidRange (T.tpTransactionDate tp) T.validTxTimeRange)

--------------------------------------------------------------------------------

-- Protocol Mock Data
--------------------------------------------------------------------------------

protocolDatum_MockData :: T.TestParams -> LedgerApiV2.Datum
protocolDatum_MockData tp = ProtocolT.mkDatum $ protocolDatumType_MockData tp

protocolDatumType_MockData :: T.TestParams -> ProtocolT.ProtocolDatumType
protocolDatumType_MockData tp =
    ProtocolT.mkProtocolDatumType
        (T.tpProtocolAdmins tp) -- pdAdmins
        (T.tpTokenAdminPolicy_CS tp) -- pdTokenAdminPolicy_CS
        T.minAdaProtocolDatum -- pdMinADA

protocolUTxO_MockData :: T.TestParams -> LedgerApiV2.TxOut
protocolUTxO_MockData tp =
    let
        datum = protocolDatum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ T.tpProtocolValidator_Hash tp)
            (LedgerAda.lovelaceValueOf T.minAdaProtocolDatum <> LedgerApiV2.singleton (T.tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
            (LedgerApiV2.OutputDatum datum)
            Nothing

-- Protocol Contract) UTxOs
--------------------------------------------------------------------------------

-- | UTxO that is spent in order to mint the ProtocolID NFT.
protocol_spend_UTxO_And_TxOutRef_MockData :: T.TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
protocol_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        T.basicAddress
        (LedgerAda.lovelaceValueOf T.minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , T.tpProtocolPolicyID_TxOutRef tp
    )


