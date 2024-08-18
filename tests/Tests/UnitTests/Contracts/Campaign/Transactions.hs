{-# LANGUAGE TypeApplications #-}

{- |
Module      : Contracts.Campaign.Validator
Description : Validation logic and tests related to the Fund module.

This module defines the validation logic for the Fund contracts, including the
creation and management of Campaign Fundss.

It includes multiple test cases to ensure the integrity and correctness of the
validation scripts.
-}

module Contracts.Campaign.Transactions where

-- Non-IOG imports
import qualified Test.Tasty              as Tasty
import qualified Test.Tasty.HUnit        as Tasty
import qualified Control.Monad as ControlMonad (replicateM, unless)

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada              as LedgerAda
import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                 as P
import qualified Data.ByteString         as BS
import qualified Flat
import qualified UntypedPlutusCore

-- Project imports
import qualified Campaign.OnChain        as CampaignOnChain
import qualified Campaign.Types          as CampaignT
import qualified Constants               as T
import           Contracts.Protocol.Data
import           Contracts.Campaign.Data

import qualified Helpers.Deploy          as Deploy
import qualified Helpers.OffChain        as OffChainHelpers
import qualified Helpers.OffChainEval    as OffChainEval
import qualified Protocol.Types          as ProtocolT
import qualified TestUtils.Common        as TestUtilsCommon
import qualified TestUtils.Constants     as T
import qualified TestUtils.Types         as T
import qualified Plutus.Model as PlutusSimpleModel
import qualified Campaign.Types as T

-- | Suite of tests validating the logic of the '(T.tpCampaignValidator tp)'.
campaignTransactionsTests :: T.TestParams -> Tasty.TestTree
campaignTransactionsTests tp =
    Tasty.testGroup
        "Transactions Test with Plutus Simple Model"
        [
            OffChainEval.goodCase "updateDatumTest" updateDatumTest
        ]
    where
        --------------------
        updateDatumTest :: PlutusSimpleModel.Run ()
        updateDatumTest = do
            --------------------
            users <- setupUsers
            --------------------
            PlutusSimpleModel.logInfo "STARTING...."
            --------------------
            let
                --------------------
                [u1, u2] = users
                --------------------
                policy =  policyPSM tp
                validator =  validatorPSM tp
                --------------------
            ct <- PlutusSimpleModel.currentTime
            --------------------
            PlutusSimpleModel.logInfo $ "TIME: " ++ P.show ct
            --------------------
            let 
                tp2 = tp {T.tpBeginAt = ct, T.tpDeadline = ct P.+ (10 P.* 30 P.* 24 P.* 60 P.* 60 P.* 1000) }
                
                campaignDatum_For_Deploy =
                    T.mkCampaignDatumType
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
                        0
                        (T.tpCollectedADA        tp)
                        (T.tpBeginAt tp2)-- cdBeginAt
                        (T.tpDeadline tp2)-- cdDeadline
                        (T.tpStatus tp)-- cdStatus
                        (T.tpMilestones tp)-- cdMilestones
                        0-- cdFundsCount
                        0-- cdFundsIndex
                        T.minAdaCampaignDatum-- cdMinADA
             --------------------
            PlutusSimpleModel.logInfo $ "DATUM: " ++ P.show campaignDatum_For_Deploy
            --------------------
            deployTx tp2 policy validator u1 campaignDatum_For_Deploy
            --------------------
            PlutusSimpleModel.waitUntil waitBeforeConsumingTx
            --------------------
            updateTx tp2 validator u1 campaignDatum_For_Deploy campaignDatum_For_Deploy (T.ValidatorRedeemerDatumUpdate T.ValidatorRedeemerDatumUpdateType)
            --------------------

----------------------------------------

-- | 'setupUsers' sets up two users with an initial amount of ADA.
setupUsers :: PlutusSimpleModel.Run [LedgerApiV2.PubKeyHash]
setupUsers = ControlMonad.replicateM 2 $ PlutusSimpleModel.newUser $ PlutusSimpleModel.ada (PlutusSimpleModel.Lovelace 100_000_00)

type PolicyPSM = PlutusSimpleModel.TypedPolicy T.PolicyRedeemer
type ValidatorPSM = PlutusSimpleModel.TypedValidator T.ValidatorDatum T.ValidatorRedeemer

policyPSM :: T.TestParams -> PolicyPSM
policyPSM tp = PlutusSimpleModel.TypedPolicy $ PlutusSimpleModel.toV2  (T.tpCampaignPolicy tp)

validatorPSM :: T.TestParams -> ValidatorPSM
validatorPSM tp = PlutusSimpleModel.TypedValidator $ PlutusSimpleModel.toV2  (T.tpCampaignValidator tp)

waitBeforeConsumingTx :: LedgerApiV2.POSIXTime
waitBeforeConsumingTx = 1000

----------------------------------------

deployTx :: T.TestParams ->  PolicyPSM -> ValidatorPSM -> LedgerApiV2.PubKeyHash -> T.CampaignDatumType -> PlutusSimpleModel.Run ()
deployTx tp policy validator user campaignDatum_Out = do
    --------------------
    PlutusSimpleModel.logInfo "DEPLOY ..."
    --------------------
    let
        campaignPolicy_CS = PlutusSimpleModel.scriptCurrencySymbol policy
        valueFor_Mint_CampaignID = LedgerApiV2.singleton campaignPolicy_CS T.campaignID_TN 1
        minADA_For_CampaignDatum = T.cdMinADA campaignDatum_Out
        value_MinADA_For_CampaignDatum = PlutusSimpleModel.adaValue minADA_For_CampaignDatum
        valueFor_CampaignDatum_Out = valueFor_Mint_CampaignID <> value_MinADA_For_CampaignDatum
    -- let valueFee = PlutusSimpleModel.adaValue 100 -- for Fees
    spUTXO <- PlutusSimpleModel.spend user value_MinADA_For_CampaignDatum -- Get user's UTXO that we should spend
    PlutusSimpleModel.logInfo $ P.show spUTXO
    let tx = P.mconcat
            [ 
                 PlutusSimpleModel.userSpend spUTXO
             , PlutusSimpleModel.mintValue policy (T.PolicyRedeemerMintID T.PolicyRedeemerMintIDType) valueFor_Mint_CampaignID
             , PlutusSimpleModel.payToScript validator (PlutusSimpleModel.InlineDatum (T.CampaignDatum campaignDatum_Out)) valueFor_CampaignDatum_Out
            
            ]
    ct <- PlutusSimpleModel.currentTimeRad T.validTxTimeRange
    tx' <- PlutusSimpleModel.validateIn ct tx
    PlutusSimpleModel.submitTx user tx'

updateTx :: T.TestParams ->  ValidatorPSM -> LedgerApiV2.PubKeyHash -> T.CampaignDatumType -> T.CampaignDatumType -> T.ValidatorRedeemer ->PlutusSimpleModel.Run ()
updateTx tp validator user  campaignDatum_In campaignDatum_Out redeemerConsume  = do
    --------------------
    PlutusSimpleModel.logInfo "UPDATE ..."
    --------------------
    let valueFee = PlutusSimpleModel.adaValue 100 -- for Fees
    spUTXO <- PlutusSimpleModel.spend user valueFee -- Get user's UTXO that we should spend
    utxos <- PlutusSimpleModel.utxoAt validator -- Query blockchain to get all UTxOs at script
    let
        (ref, out) = head utxos -- We know there is only one UTXO (the one we created before)
        getValue =  LedgerApiV2.txOutValue out
    let tx = P.mconcat
            [
                PlutusSimpleModel.spendScript validator ref redeemerConsume (T.CampaignDatum campaignDatum_In)
            , PlutusSimpleModel.payToScript validator (PlutusSimpleModel.InlineDatum (T.CampaignDatum campaignDatum_Out))  getValue
            ]
    ct <- PlutusSimpleModel.currentTimeRad T.validTxTimeRange
    tx' <- PlutusSimpleModel.validateIn ct tx
    PlutusSimpleModel.submitTx user tx'


----------------------------------------


