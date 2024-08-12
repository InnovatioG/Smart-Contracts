{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

----------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
----------------------------------------------------------------------------2

module Campaign.Funds.OnChain where

----------------------------------------------------------------------------2
-- Import Externos
----------------------------------------------------------------------------2

import qualified Data.List                     as DataList
import qualified Ledger
import qualified Ledger.Ada                    as LedgerAda
import qualified Ledger.Value                  as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts     as LedgerContextsV2
import qualified PlutusTx
import qualified PlutusTx.AssocMap             as TxAssocMap
import           PlutusTx.Prelude              (AdditiveGroup ((-)), AdditiveSemigroup ((+)), Bool (..), BuiltinData, Eq ((==)), Integer, Maybe (Just, Nothing), MultiplicativeSemigroup ((*)), Ord (max, (<), (<=), (>), (>=)), Semigroup ((<>)), all,
                                                and, divide, error, foldl, fromMaybe, fst, head, length, map, negate, null, otherwise, remainder, snd, sort, sum, traceError, traceIfFalse, traceIfTrue, uncurry, zip, zipWith, (!!), ($), (&&), (++),
                                                (.), (/=), (<$>), (||), not)
import qualified Ledger.Ada as LedgerADA

----------------------------------------------------------------------------2
-- Import Internos
----------------------------------------------------------------------------2

import qualified Helpers.Constants             as T
import qualified Helpers.OnChain        as Helpers
import qualified Helpers.OnChain        as OnChainHelpers
import qualified Helpers.Types                 as T
import qualified Campaign.Funds.Types as T
import qualified Campaign.Helpers     as CampaignHelpers
import qualified Campaign.Types       as CampaignT
import qualified Constants            as T
import qualified Protocol.Types       as ProtocolT
import qualified Types                as T

----------------------------------------------------------------------------2
-- Modulo
----------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !campaignPolicy_CS !campaignFundsValidator_Hash) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && validate
        then ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = campaignPolicy_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !campaignFundsPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        !ownMintingValue = OnChainHelpers.getUnsafeOwnMintingValue ctx
        ------------------
        validate :: Bool
        !validate = validateMintAndBurnIDRedeemers redeemer campaignPolicy_CS campaignFundsPolicyID_CS campaignFundsValidator_Hash ownMintingValue info ctx

----------------------------------------------------------------------------2

{-# INLINEABLE validateMintAndBurnIDRedeemers #-}
validateMintAndBurnIDRedeemers :: T.PolicyRedeemer -> LedgerApiV2.CurrencySymbol-> LedgerApiV2.CurrencySymbol-> LedgerApiV2.ValidatorHash ->  Ledger.Value ->LedgerContextsV2.TxInfo -> LedgerContextsV2.ScriptContext -> Bool
validateMintAndBurnIDRedeemers redeemer campaignPolicy_CS campaignFundsPolicyID_CS campaignFundsValidator_Hash ownMintingValue info ctx =
    case redeemer of
        (T.PolicyRedeemerMintID _) ->
                ------------------
                -- it runs along with Campaign Validator (ValidatorRedeemerFundsAdd)
                    -- validateAdminAction
                    -- traceIfFalse "not isCorrect_Output_CampaignDatum_With_CampaignFundsAdded" isCorrect_Output_CampaignDatum_With_CampaignFundsAdded
                    -- && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" isCorrect_Output_CampaignDatum_Value_NotChanged
                    -- && traceIfFalse "not isMintingCampaignFundsID" isMintingCampaignFundsID
                    -- && traceIfFalse "not isCampaignOpen" isCampaignOpen
                ------------------
                -- Que se consuma CampaignDatum con redeemer correcto (ValidatorRedeemerFundsAdd)
                -- Para identificar el correcto CampaignDatum necesita la póliza Campaign ID que está en los parámetros de esta póliza.
                -- Que se genere salida con nuevo HoldingDatum en Holding Val (Holding Val está indicada en CampaignDatum)
                -- Que el HoldingDatum sea correcto
                -- Que se mintee CampaignFunds ID con own póliza
                -- Que el HoldingDatum tenga el CampaignFunds ID
                ------------------
                traceIfFalse "not isMintingCampaignFundsID" isMintingCampaignFundsID &&
                traceIfFalse "not isCorrect_Redeemer_CampaignDatum" (isCorrect_Redeemer_CampaignDatum isCampaignValidatorRedeemerFundsAdd ) &&
                traceIfFalse "not isCorrect_Output_CampaignFundsDatum" isCorrect_Output_CampaignFundsDatum &&
                traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value" isCorrect_Output_CampaignFundsDatum_Value
                ------------------
            where
                ------------------
                !outputs_txOuts = [ txOut | txOut <- LedgerApiV2.txInfoOutputs info,
                    OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)  ]
                ------------------
                !_ = if length outputs_txOuts < 2
                    then traceError "Expected at least two outputs to scripts addresses"
                    else ()
                ------------------
                -- 0 out is the CampaignDatum
                -- 1 out is the CampaignFundsDatum
                ------------------
                !input_TxOut_And_CampaignDatum = (\(_, txOut, datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_CampaignDatum
                ------------------
                !campaignDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_CampaignDatum
                ------------------
                !campaignFunds_Index = CampaignT.cdFundsIndex campaignDatum_In
                !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
                !campaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
                ------------------
                !campaignFundsValidator_Address = Ledger.scriptHashAddress campaignFundsValidator_Hash
                ------------------
                !output_Own_TxOut_And_CampaignFundsDatum =  fromMaybe
                    (traceError "Expected Campaign Funds at output index 1")
                    (OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                        @T.ValidatorDatum
                        @T.CampaignFundsDatumType
                        ctx
                        (outputs_txOuts!!1)
                        campaignFundsID_AC
                        (Just campaignFundsValidator_Address)
                        T.getCampaignFundsDatumType)
                ------------------
                !campaignFundsDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_CampaignFundsDatum
                ------------------
                !valueFor_Mint_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC 1
                ------------------
                !minADA_For_CampaignFundsDatum_Out = T.cfdMinADA campaignFundsDatum_Out
                !value_MinADA_For_CampaignFundsDatum_Out = LedgerAda.lovelaceValueOf minADA_For_CampaignFundsDatum_Out
                !valueFor_CampaignFundsDatum_Out_Control = valueFor_Mint_CampaignFundsID <> value_MinADA_For_CampaignFundsDatum_Out
                ------------------
                !campaignFundsDatum_Out_Control = T.CampaignFundsDatumType  {
                        T.cfdIndex = campaignFunds_Index,
                        T.cfdCampaignPolicy_CS = campaignPolicy_CS,
                        T.cfdCampaignFundsPolicyID_CS = campaignFundsPolicyID_CS,
                        T.cfdSubtotal_Avalaible_FT = 0,
                        T.cfdSubtotal_Sold_FT  = 0,
                        T.cfdSubtotal_Avalaible_ADA = 0,
                        T.cfdSubtotal_Collected_ADA = 0,
                        T.cfdMinADA = minADA_For_CampaignFundsDatum_Out
                    }
                ------------------
                isCampaignValidatorRedeemerFundsAdd :: CampaignT.ValidatorRedeemer -> Bool
                isCampaignValidatorRedeemerFundsAdd redemeerToCheck = case redemeerToCheck of
                    CampaignT.ValidatorRedeemerFundsAdd _ -> True
                    _                                     -> False
                ------------------
                isMintingCampaignFundsID :: Bool
                isMintingCampaignFundsID = ownMintingValue `OnChainHelpers.isEqValue` valueFor_Mint_CampaignFundsID
                -----------------
                isCorrect_Output_CampaignFundsDatum :: Bool
                isCorrect_Output_CampaignFundsDatum =
                    campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                ------------------
                isCorrect_Output_CampaignFundsDatum_Value :: Bool
                isCorrect_Output_CampaignFundsDatum_Value =
                    let !valueOf_CampaignFundsDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignFundsDatum
                    in  valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Out_Control
                ------------------
        (T.PolicyRedeemerBurnID _) ->
                ------------------
                -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
                -- 2 - it runs along Campaign Funds ID Policy (PolicyRedeemerBurnID)
                -- 3 - it runs along Campaign Funds Validator,  many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
                ------------------
                -- 1 - Que sea Protocol or Campaig Admin
                -- 1 y 3 - Que se quemen CampaignFundsIDs con la correcta póliza indicada en CampaignDatum o CampaignFundDatum
                -- 2 - Que se quemen CampaignFundsIDs con own póliza
                -- 2 - Qque coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
                -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de Campaign Funds Validator (en caso de Merge hay una input que no se quema)
                -- 2 - que el redeemer de Campaign validator sea correcto
                -- 2 - que el redeemer de todos los Campaign Funds Validator sea correcto
                -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
                -- 1 - Que el CampaignDatum se actualiza con el Campaign Funds eliminados
                -- 1 - Que el CampaignDatum value no cambie
                -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
                -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
                -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
                -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
                -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
                -- no hay restricciones temporales
                ------------------
                traceIfFalse "not isBurningCampaignFundsID" isBurningCampaignFundsID
                && traceIfFalse "not isCorrect_Redeemer_CampaignDatum" (isCorrect_Redeemer_CampaignDatum isCampaignValidatorRedeemerFundsDelete)
                && traceIfFalse "not isZeroAssets" isZeroAssets
                ------------------
            where
                ------------------
                !input_TxOutRef_TxOut_And_CampaignFundsDatum =
                    case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                        @T.ValidatorDatum
                        @T.CampaignFundsDatumType
                        ctx
                        inputs_TxOutRefs_TxOuts
                        campaignFundsPolicyID_CS
                        T.getCampaignFundsDatumType of
                    [x] -> x
                    _   -> traceError "Expected exactly one CampaignFunds input"
                ------------------
                !input_TxOut_And_CampaignFundsDatum = (\(_, txOut, datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_CampaignFundsDatum
                ------------------
                !campaignFundsDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_CampaignFundsDatum
                ------------------
                !campaignFunds_Index = T.cfdIndex campaignFundsDatum_In
                !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
                !campaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
                ------------------
                !valueFor_Mint_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC 1
                !valueFor_Burn_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC (negate 1)
                ------------------
                isBurningCampaignFundsID :: Bool
                isBurningCampaignFundsID = ownMintingValue `OnChainHelpers.isEqValue` valueFor_Burn_CampaignFundsID
                -----------------
                isCampaignValidatorRedeemerFundsDelete :: CampaignT.ValidatorRedeemer -> Bool
                isCampaignValidatorRedeemerFundsDelete redemeerToCheck = case redemeerToCheck of
                    CampaignT.ValidatorRedeemerFundsDelete _ -> True
                    _                                        -> False
                ------------------
                isZeroAssets :: Bool
                !isZeroAssets =
                    let
                        ------------------
                        !minADA_For_CampaignFundsDatum_In = T.cfdMinADA campaignFundsDatum_In
                        !value_MinADA_For_CampaignFundsDatum_In = LedgerAda.lovelaceValueOf minADA_For_CampaignFundsDatum_In
                        !valueFor_CampaignFundsDatum_In_Control = valueFor_Mint_CampaignFundsID <> value_MinADA_For_CampaignFundsDatum_In
                        ------------------
                        !valueOf_CampaignFundsDatum_In = OnChainHelpers.getValue_In_TxOut_And_Datum input_TxOut_And_CampaignFundsDatum
                    in  valueOf_CampaignFundsDatum_In `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_In_Control
    where
        ------------------
        !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        ------------------
        !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info,
            OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)]
        ------------------
        !input_TxOutRef_TxOut_And_CampaignDatum =
            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                @CampaignT.ValidatorDatum
                @CampaignT.CampaignDatumType
                ctx
                inputs_TxOutRefs_TxOuts
                campaignID_AC
                CampaignT.getCampaignDatumType of
                    [x] -> x
                    _   -> traceError "Expected exactly one Campaign input"
        ------------------
        isCorrect_Redeemer_CampaignDatum :: (CampaignT.ValidatorRedeemer -> Bool) -> Bool
        isCorrect_Redeemer_CampaignDatum isRedeemerType =
            let !redeemerFor_CampaignDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef ) input_TxOutRef_TxOut_And_CampaignDatum) info
            in case redeemerFor_CampaignDatum' of
                Nothing -> False
                Just redeemerFor_CampaignDatum -> case LedgerApiV2.fromBuiltinData @CampaignT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_CampaignDatum of
                    Just x -> isRedeemerType x
                    _      -> False

----------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && validate
        then ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        validate :: Bool
        !validate = case redeemer of
            -- Si esta el token de emergencia se saltea todos los controles
            (T.ValidatorRedeemerEmergency _) -> validateEmergencyRedeemer
            _                                -> validateNonEmergencyRedeemers
        ----------------------------------------------------------------------------2
        validateEmergencyRedeemer :: Bool
        validateEmergencyRedeemer =
            let
                !tokenEmergencyAdmin_AC = LedgerValue.AssetClass (tokenEmergencyAdminPolicy_CS, T.tokenEmergencyAdmin_TN)
                -- search emergency admin token in output 0
                !isEmergencyAdminTokenPresent = OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head (LedgerApiV2.txInfoOutputs info)) tokenEmergencyAdmin_AC
            in
                traceIfFalse "not isEmergencyAdminTokenPresent" isEmergencyAdminTokenPresent
        --------------------------------------------------------------------------------2
        validateNonEmergencyRedeemers ::  Bool
        validateNonEmergencyRedeemers  =
                ------------------
                traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
                && validateRedeemers
                ------------------
            where
                ------------------
                !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                !campaignFunds_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                ------------------
                -- NOTE: inputsRef_TxOuts es optional, por eso no se usa el BANG !
                ------------------
                inputsRef_TxOuts_ =
                    [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                    ]
                ------------------
                !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info,
                            let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                            in  OnChainHelpers.isScriptAddress address && address == campaignFunds_Validator_Address]
                ------------------
                !outputs_Own_txOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info,
                            let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                            in  OnChainHelpers.isScriptAddress address && address == campaignFunds_Validator_Address]
                ------------------
                -- NOTE: inputs_Own_TxOuts_And_CampaignFundsDatums es optional, por eso no se usa el BANG !
                ------------------
                inputs_Own_TxOuts_And_CampaignFundsDatums_ =
                    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                            @T.ValidatorDatum
                            @T.CampaignFundsDatumType
                            ctx
                            inputs_Own_TxOuts
                            campaignFundsPolicyID_CS
                            T.getCampaignFundsDatumType
                ------------------
                outputs_Own_TxOuts_And_CampaignFundsDatums_ =
                    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                            @T.ValidatorDatum
                            @T.CampaignFundsDatumType
                            ctx
                            outputs_Own_txOuts
                            campaignFundsPolicyID_CS
                            T.getCampaignFundsDatumType
                ------------------
                !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                ------------------
                !campaignFundsDatum_In = T.getCampaignFundsDatumType datum
                ------------------
                !campaignPolicy_CS = T.cfdCampaignPolicy_CS campaignFundsDatum_In
                !campaignFundsPolicyID_CS = T.cfdCampaignFundsPolicyID_CS campaignFundsDatum_In
                ------------------
                !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
                ------------------
                -- !campaignFunds_Index = T.cfdIndex campaignFundsDatum_In
                -- !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
                -- !campaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
                ------------------
                !valueOf_CampaignFundsDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                ------------------
                -- NOTE: campaignFundsDatums_In, campaignFundsDatums_Out, valueOf_CampaignFundsDatums_In y valueOf_CampaignFundsDatums_Out son optionales, por eso no se usa el BANG !
                ------------------
                campaignFundsDatums_In_ = OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_CampaignFundsDatums_
                campaignFundsDatums_Out_ = OnChainHelpers.getDatum_In_TxOut_And_Datum <$> outputs_Own_TxOuts_And_CampaignFundsDatums_
                ------------------
                valueOf_CampaignFundsDatums_In_ = OnChainHelpers.getValue_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_CampaignFundsDatums_
                valueOf_CampaignFundsDatums_Out_ = OnChainHelpers.getValue_In_TxOut_And_Datum <$> outputs_Own_TxOuts_And_CampaignFundsDatums_
                --------------------------------------------------------------------------------
                -- NOTE: campaignDatum y ProtocolDatum por inputRef son optionales, por eso no se usa el BANG !
                ------------------
                inputRef_TxOut_And_CampaignDatum_ =
                    case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                        @CampaignT.ValidatorDatum
                        @CampaignT.CampaignDatumType
                        ctx
                        inputsRef_TxOuts_
                        campaignID_AC
                        CampaignT.getCampaignDatumType of
                            [x] -> x
                            _   -> traceError "Expected exactly one Campaign input ref"
                ------------------
                campaignDatum_In_ = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_CampaignDatum_
                ------------------
                protocolID_AC_ = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                ------------------
                inputRef_TxOut_And_ProtocolDatum'_ =
                    case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                        @ProtocolT.ValidatorDatum
                        @ProtocolT.ProtocolDatumType
                        ctx
                        inputsRef_TxOuts_
                        protocolID_AC_
                        ProtocolT.getProtocolDatumType of
                            [x] -> Just x
                            _   -> Nothing
                ------------------
                isAdminTokenPresent :: CampaignT.CampaignDatumType -> Bool
                isAdminTokenPresent !campaignDatum_In =
                ------------------
                -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
                ------------------
                    case LedgerApiV2.txInfoOutputs info of
                        []         -> False
                        -- search admin token in output 0
                        (output:_) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                        where
                            !tokenAdminPolicy_CS = T.getAdminToken_CS campaignDatum_In
                            !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.tokenAdmin_TN)
                ------------------
                isCampaignOpen :: CampaignT.CampaignDatumType -> Bool
                isCampaignOpen !campaignDatum_In = OnChainHelpers.isDateReached (CampaignT.cdBeginAt campaignDatum_In) info && OnChainHelpers.isDateNotReached (CampaignT.cdDeadline campaignDatum_In) info
                 ------------------
                isCampaignClosed :: CampaignT.CampaignDatumType -> Bool
                isCampaignClosed  = not . isCampaignOpen
                ------------------
                isCampaignStatus :: CampaignT.CampaignDatumType -> CampaignT.CapaignStatus -> Bool
                isCampaignStatus !campaignDatum_In !status = CampaignT.cdStatus campaignDatum_In == status
                ------------------
                validateCampaignAdminAction :: CampaignT.CampaignDatumType -> Bool
                validateCampaignAdminAction !campaignDatum_In =
                ------------------
                -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
                ------------------
                    -- Que este el token de admin presente
                    -- o Que sea Campaign Admin
                    ------------------
                    traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent campaignDatum_In)
                    where
                        !admins = T.getAdmins campaignDatum_In_
                ------------------
                validateProtocolOrCampaignAdminAction :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> CampaignT.CampaignDatumType -> Bool
                validateProtocolOrCampaignAdminAction !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_In =
                ------------------
                -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
                ------------------
                    -- Que este el token de admin presente
                    -- o Que sea Campaign Admin
                    -- o Que sea Protocol Admin si hay input ref protocol
                    ------------------
                    traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent campaignDatum_In)
                    where
                        ------------------
                        !admins = T.getAdmins campaignDatum_In_ ++
                            case inputRef_TxOut_And_ProtocolDatum' of
                                Just x ->
                                    let
                                        ------------------
                                        !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum x
                                        ------------------
                                    in T.getAdmins  protocolDatum_In
                                _ -> []
                ------------------
                validateRedeemers :: Bool
                !validateRedeemers =
                    case redeemer of
                        (T.ValidatorRedeemerDelete _)        -> validate_OneOrMore_Inputs_No_Outputs_Redeemers
                        (T.ValidatorRedeemerBalanceAssets _) -> validate_Many_Inputs_Same_Outputs_Redeemers
                        _                                    -> validate_OneOrMore_Inputs_One_Output_Redeemers campaignFundsDatums_Out_ valueOf_CampaignFundsDatums_Out_
                --------------------------------------------------------------------------------
                -- NOTE: todos los validadores son optionales al redeemer, por eso no se usa el BANG !
                --------------------------------------------------------------------------------
                validate_OneOrMore_Inputs_No_Outputs_Redeemers :: Bool
                validate_OneOrMore_Inputs_No_Outputs_Redeemers =
                    traceIfFalse "not null outputs_Own_txOuts" ( null outputs_Own_txOuts  ) &&
                    case redeemer of
                        (T.ValidatorRedeemerDelete _) -> validateRedeemerDelete
                        _                             -> False
                --------------------------------------------------------------------------------
                validate_Many_Inputs_Same_Outputs_Redeemers :: Bool
                validate_Many_Inputs_Same_Outputs_Redeemers =
                    traceIfFalse "not isSameOutputsAsInputs" isSameOutputsAsInputs &&
                    traceIfFalse "not length inputs_Own_TxOuts > 1" (length inputs_Own_TxOuts > 1) &&
                    case redeemer of
                        (T.ValidatorRedeemerBalanceAssets _) ->
                            validateBalanceRedeemer campaignDatum_In_ inputs_Own_TxOuts_And_CampaignFundsDatums_ outputs_Own_TxOuts_And_CampaignFundsDatums_
                        _ -> False
                    ------------------
                    where
                        isSameOutputsAsInputs :: Bool
                        !isSameOutputsAsInputs = length inputs_Own_TxOuts == length outputs_Own_txOuts
                --------------------------------------------------------------------------------
                validate_OneOrMore_Inputs_One_Output_Redeemers :: [T.CampaignFundsDatumType] ->[Ledger.Value] -> Bool
                validate_OneOrMore_Inputs_One_Output_Redeemers [!campaignFundsDatum_Out] [!valueOf_CampaignFundsDatum_Out] =
                    case redeemer of
                        (T.ValidatorRedeemerMerge _)        -> validate_Many_Inputs_One_Output_Redeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerUpdateMinADA _) -> validate_One_Input_One_Output_Redeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerDeposit _)      -> validate_One_Input_One_Output_Redeemers  campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerWithdraw _)     -> validate_One_Input_One_Output_Redeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerSell _)         -> validate_One_Input_One_Output_Redeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerGetBack _)      -> validate_One_Input_One_Output_Redeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerCollect _)      -> validate_One_Input_One_Output_Redeemers  campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        _                                   -> False
                validate_OneOrMore_Inputs_One_Output_Redeemers _ _ =
                    traceIfFalse "not length outputs_Own_txOuts == 1" (length outputs_Own_txOuts == 1)
                --------------------------------------------------------------------------------
                validate_Many_Inputs_One_Output_Redeemers :: T.CampaignFundsDatumType -> Ledger.Value ->  Bool
                validate_Many_Inputs_One_Output_Redeemers  !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out  =
                    traceIfFalse "not length inputs_Own_TxOuts > 1" (length inputs_Own_TxOuts > 1) &&
                    case redeemer of
                        (T.ValidatorRedeemerMerge _) -> validateRedeemerMerge campaignFundsDatums_In_ valueOf_CampaignFundsDatums_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        _                            -> False
                --------------------------------------------------------------------------------
                validate_One_Input_One_Output_Redeemers :: T.CampaignFundsDatumType -> Ledger.Value ->  Bool
                validate_One_Input_One_Output_Redeemers !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out  =
                    traceIfFalse "not length inputs_Own_TxOuts == 1" (length inputs_Own_TxOuts == 1) &&
                    case redeemer of
                        (T.ValidatorRedeemerUpdateMinADA _) ->  validateBasicUpdateRedeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerDeposit redeemer')      ->  validateDepositRedeemer redeemer' campaignDatum_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerWithdraw redeemer')     -> validateWithdrawRedeemer redeemer' campaignDatum_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerCollect redeemer')      -> validateCollectRedeemer redeemer'  campaignDatum_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerSell redeemer')         -> validateSellRedeemer redeemer' campaignDatum_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        (T.ValidatorRedeemerGetBack redeemer')      -> validateGetBackRedeemer redeemer' campaignDatum_In_ campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        _                                   -> False
                ----------------------------------------------------------------------------
                validateBasicUpdateRedeemers :: T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateBasicUpdateRedeemers !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    case redeemer of
                        (T.ValidatorRedeemerUpdateMinADA _) -> validateUpdateMinADARedeemers campaignFundsDatum_Out valueOf_CampaignFundsDatum_Out
                        _                                   -> False
                ----------------------------------------------------------------------------2
                validateDepositRedeemer :: T.ValidatorRedeemerDepositType -> CampaignT.CampaignDatumType -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateDepositRedeemer (T.ValidatorRedeemerDepositType _ amount) !campaignDatum_In !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que sea Protocol or Campaign Admin
                    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos depositos
                    -- Que el CampaignFundsDatum value cambie con los tokens depositados
                    -- Que el CampaignDatum este en estado CsCreated
                    ------------------
                        validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_In  &&
                        traceIfFalse "not isCampaignStatus CsCreated" (isCampaignStatus campaignDatum_In CampaignT.CsCreated) && 
                        traceIfFalse "not amount > 0" (amount > 0) && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Updated_With_Deposits" isCorrect_Output_CampaignFundsDatum_Updated_With_Deposits && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Deposits" isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Deposits
                    where
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Updated_With_Deposits :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Updated_With_Deposits =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Deposits
                                        campaignFundsDatum_In
                                        amount
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Deposits :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Deposits =
                            let
                                !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                                ------------------
                                !valueOf_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount
                                ------------------
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> valueOf_CampaignFT
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------2
                validateWithdrawRedeemer :: T.ValidatorRedeemerWithdrawType ->  CampaignT.CampaignDatumType -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateWithdrawRedeemer (T.ValidatorRedeemerWithdrawType _ amount)  !campaignDatum_In  !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que sea Protocol or Campaign Admin
                    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos withdraw
                    -- Que el CampaignFundsDatum value cambie con los tokens withdraw
                    -- Que el CampaignDatum este en estado CsCreated
                    ------------------
                        validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_In  &&
                        traceIfFalse "not isCampaignStatus CsCreated" (isCampaignStatus campaignDatum_In CampaignT.CsCreated) && 
                        traceIfFalse "not amount > 0" (amount > 0) && 
                        traceIfFalse "not avalaible_FT >= amount" (T.cfdSubtotal_Avalaible_FT campaignFundsDatum_In >= amount) && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Updated_With_Withdraw" isCorrect_Output_CampaignFundsDatum_Updated_With_Withdraw && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Withdraw" isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Withdraw
                    where
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Updated_With_Withdraw :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Updated_With_Withdraw =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Withdraw
                                        campaignFundsDatum_In
                                        amount
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Withdraw :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Withdraw =
                            let
                                !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                                ------------------
                                !valueOf_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount
                                ------------------
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> negate valueOf_CampaignFT
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------2
                validateSellRedeemer :: T.ValidatorRedeemerSellType ->  CampaignT.CampaignDatumType -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateSellRedeemer (T.ValidatorRedeemerSellType date amount_FT)  !campaignDatum_In  !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos vendidos
                    -- Que el CampaignFundsDatum value cambie con el nuevo ADA y menos los tokens vendidos
                    -- Que el CampaignDatum este en estado CsInitialized
                    -- Que la fecha sea entre Begin y Deadline
                    ------------------
                        traceIfFalse "not isCampaignOpen" (isCampaignOpen campaignDatum_In) && 
                        traceIfFalse "not isCampaignStatus CsInitialized" (isCampaignStatus campaignDatum_In CampaignT.CsInitialized) && 
                        traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info) && 
                        traceIfFalse "not amount_FT > 0" (amount_FT > 0) && 
                        traceIfFalse "not avalaible_FT >= amount_FT" (T.cfdSubtotal_Avalaible_FT campaignFundsDatum_In >= amount_FT) && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Updated_With_SoldTokens" isCorrect_Output_CampaignFundsDatum_Updated_With_SoldTokens && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Changed_WithSoldTokens" isCorrect_Output_CampaignFundsDatum_Value_Changed_WithSoldTokens
                    where
                        ------------------
                        !campaignFT_PriceADA  = CampaignT.cdCampaignFT_PriceADA campaignDatum_In
                        !amount_ADA = amount_FT * campaignFT_PriceADA
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Updated_With_SoldTokens :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Updated_With_SoldTokens =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_SoldTokens
                                        campaignFundsDatum_In
                                        amount_FT amount_ADA
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Changed_WithSoldTokens :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Changed_WithSoldTokens =
                            let
                                !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                                ------------------
                                !valueOf_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount_FT
                                ------------------
                                !valueOf_totalADA = LedgerADA.lovelaceValueOf amount_ADA
                                ------------------
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> negate valueOf_CampaignFT <> valueOf_totalADA
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------2
                validateGetBackRedeemer :: T.ValidatorRedeemerGetBackType ->  CampaignT.CampaignDatumType ->  T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateGetBackRedeemer (T.ValidatorRedeemerGetBackType date amount_FT)  !campaignDatum_In !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que el CampaignFundsDatum se actualiza correctamente con nuevo GetBack
                    -- Que el CampaignFundsDatum value cambie con los tokens extraidos
                    -- Que el CampaignDatum este en estado CsNotReached | CsFailedMilestone
                    ------------------
                    -- TODO: 
                    -- el campo sold_FT puede quedar negativo, si se hace getback todos contra un mismo CampaignFunds. Ver como afecta eso en otros lados
                    -- si el caso fuera CsFailedMilestone, hay que devolver proporcionalmente a lo vendido, a cada usuario. No se puede devolver todo.
                    -- Usar para eso un total vendido, guardado en CampaignDatum
                    ------------------
                        traceIfFalse "not isCampaignClosed" (isCampaignClosed campaignDatum_In) && 
                        traceIfFalse "not isCampaignStatus CsNotReached | CsFailedMilestone" (isCampaignStatus campaignDatum_In CampaignT.CsNotReached || isCampaignStatus campaignDatum_In CampaignT.CsFailedMilestone) && 
                        traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info) && 
                        traceIfFalse "not amount_FT > 0" (amount_FT > 0) && 
                        traceIfFalse "not avalaible_ADA >= amount_FT" (T.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In >= amount_FT) && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Updated_With_GetBack" isCorrect_Output_CampaignFundsDatum_Updated_With_GetBack && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Changed_With_GetBack" isCorrect_Output_CampaignFundsDatum_Value_Changed_With_GetBack
                    where
                        ------------------
                        !campaignFT_PriceADA  = CampaignT.cdCampaignFT_PriceADA campaignDatum_In
                        !amount_ADA = amount_FT * campaignFT_PriceADA
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Updated_With_GetBack :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Updated_With_GetBack =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_GetBackTokens
                                        campaignFundsDatum_In
                                        amount_FT amount_ADA
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Changed_With_GetBack :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Changed_With_GetBack =
                            let
                                !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                                ------------------
                                !valueOf_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount_FT
                                ------------------
                                !valueOf_totalADA = LedgerADA.lovelaceValueOf amount_ADA
                                ------------------
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> valueOf_CampaignFT <> negate valueOf_totalADA
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------
                validateCollectRedeemer :: T.ValidatorRedeemerCollectType -> CampaignT.CampaignDatumType ->   T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateCollectRedeemer (T.ValidatorRedeemerCollectType _ amount_ADA) !campaignDatum_In !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                        ---------------------
                        -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsCollect)
                        -- 2 - it runs along Campaign Funds Validator, one input, one output (ValidatorRedeemerCollect) 
                        ------------------
                        -- 1 - Que sea Campaign Admin
                        -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
                        -- 1 - Que el CampaignDatum este en estado CsReached 
                        -- 1 - Que el CampaignDatum se actualiza con el pago a los creadores de la campaña
                        -- 1 - Que el CampaignDatum value no cambie
                        -- 1 - que el redeemer de todos los Campaign Funds Validator sea correcto, mismo redeemer type, misma date, mismo amount. 
                        -- 1 - Que el monto no supera lo disponible en ese momento: el pago total acumulado hasta ahora menos lo cobrado.
                        -- 1 - El pago acumulado hasta ahora es de acuerdo al ultimo milestone aprobado. Siempre que el ultimo milestone estuvira en Creado y no en Fallado.
                        -- 1 - Cada milestone establece un porcentaje del total. Tengo el campo de total vendido ADA, y de ahi calculo los porcentajes.
                        -- 2 - que el redeemer de Campaign validator sea correcto
                        -- 2 - Que todos los CampaignFundsDatum resultantes regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
                        -- 2 - Que los CampaignFundsDatum datums se actualicen correctamente: solo el campo collected debe modificarse, sumando algun valor. 
                        -- 2 - Que la suma de todo lo que se saca de cada uno, coincida con amount
                        -- 2 - Que los CampaignFundsDatum tengan values actualizados: con menos ADA. Cada uno coincidiendo con el valor de collected del datum
                        ------------------
                        -- no hay restricciones temporales
                        ------------------
                        validateCampaignAdminAction campaignDatum_In_  &&
                        traceIfFalse "not isCorrectRedeemersCampaignDatum" isCorrectRedeemersCampaignDatum &&
                        traceIfFalse "not avalaible_ADA_to_Collect_in_Fund >= amount_ADA" (avalaible_ADA_to_Collect_in_Fund >= amount_ADA) && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Updated_With_Collect" isCorrect_Output_CampaignFundsDatum_Updated_With_Collect && 
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Collect" isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Collect
                    where
                        ------------------
                        !avalaible_ADA_to_Collect_in_Fund = T.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In
                        ------------------
                        isCorrectValidatorRedeemerFundsCollectCampaigDatum :: (LedgerApiV2.TxOutRef, LedgerApiV2.TxOut, CampaignT.CampaignDatumType) -> Bool
                        isCorrectValidatorRedeemerFundsCollectCampaigDatum input_TxOutRef_TxOut_And_CampaignDatum  =
                            let
                                !redeemerFor_CampaignDatum = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_CampaignDatum) info
                            in
                                case redeemerFor_CampaignDatum of
                                    Nothing -> traceError "Expected Campaign input with redeemer"
                                    Just redeemerFor_CampaignDatum' ->
                                        case LedgerApiV2.fromBuiltinData @CampaignT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_CampaignDatum' of
                                            Just (CampaignT.ValidatorRedeemerFundsCollect _) -> True
                                            _      -> traceError "Expected Campaign input with valid redeemer ValidatorRedeemerFundsCollect"
                        ------------------
                        isCorrectRedeemersCampaignDatum :: Bool
                        !isCorrectRedeemersCampaignDatum =
                            let
                                ------------------
                                !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info]
                                ------------------
                                !input_TxOutRef_TxOut_And_CampaignDatum =
                                    case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                                        @CampaignT.ValidatorDatum
                                        @CampaignT.CampaignDatumType
                                        ctx
                                        inputs_TxOutRefs_TxOuts
                                        campaignID_AC
                                        CampaignT.getCampaignDatumType of
                                        [x] -> x
                                        _   -> traceError "Expected one Campaign input"
                            in 
                                isCorrectValidatorRedeemerFundsCollectCampaigDatum input_TxOutRef_TxOut_And_CampaignDatum
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Updated_With_Collect :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Updated_With_Collect =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_Collect
                                        campaignFundsDatum_In
                                        amount_ADA
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Collect :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Changed_With_Collect =
                            let
                                !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                                ------------------
                                !valueOf_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount_ADA
                                ------------------
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> negate valueOf_CampaignFT
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------2
                validateUpdateMinADARedeemers :: T.CampaignFundsDatumType -> Ledger.Value -> Bool
                validateUpdateMinADARedeemers !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que sea Campaign Admin
                    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que el CampaignFundsDatum se actualiza correctamente
                    -- Que el CampaignFundsDatum value cambie con el min ADA nuevo
                    -- no hay restricciones temporales
                    ------------------
                        validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_In_  &&
                        traceIfFalse "not isCorrect_Output_CampaignFundsDatum_UpdatedMinADA" isCorrect_Output_CampaignFundsDatum_UpdatedMinADA
                        && traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_ChangedADA" isCorrect_Output_CampaignFundsDatum_Value_ChangedADA
                    where
                        ------------------
                        !newMinADA = T.cfdMinADA campaignFundsDatum_Out
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_UpdatedMinADA :: Bool
                        !isCorrect_Output_CampaignFundsDatum_UpdatedMinADA =
                            let
                                !campaignFundsDatum_Out_Control =
                                    CampaignHelpers.mkUpdated_CampaignFundsDatum_With_NewMinADA
                                        campaignFundsDatum_In
                                        newMinADA
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFundsDatum_Value_ChangedADA :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_ChangedADA =
                            let
                                -- !valueOf_CampaignFundsDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignFundsDatum
                                !valueFor_CampaignFundsDatum_Control = valueOf_CampaignFundsDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.cfdMinADA campaignFundsDatum_In)
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Control
                ----------------------------------------------------------------------------2
                validateBalanceRedeemer :: CampaignT.CampaignDatumType -> [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] ->  [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] ->   Bool
                validateBalanceRedeemer !campaignDatum_In !inputs_Own_TxOuts_And_CampaignFundsDatums !outputs_Own_TxOuts_And_CampaignFundsDatums  =
                    ---------------------
                    -- it runs alone
                    ---------------------
                    -- Que sea Protocol or Campaign Admin
                    -- Que los CampaignFundsDatum regresen a Campaign Val (se hace automaticamente al buscar outputs en same address)
                    -- Que los CampaignFundsDatum se actualicen correctamente
                    -- Que los CampaignFundsDatum value cambien correctamente
                    -- NOTE: se usa zipwith, lo que significa que depende del orden las entradas y salidas. Deben estar en el mismo orden
                    -- no hay restricciones temporales
                    ------------------
                    validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_In  &&
                    traceIfFalse "not isCorrect_Outputs_CampaignFundsDatum_Updated_Balanced" isCorrect_Outputs_CampaignFundsDatum_Updated_Balanced &&
                    traceIfFalse "not isCorrect_Outputs_CampaignFundsDatum_Values_Balanced" isCorrect_Outputs_CampaignFundsDatum_Values_Balanced
                    where
                    ------------------
                    isCorrect_Outputs_CampaignFundsDatum_Updated_Balanced :: Bool
                    !isCorrect_Outputs_CampaignFundsDatum_Updated_Balanced =
                        let
                            ------------------
                            !campaignFundsDatums_In = OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_CampaignFundsDatums
                            !campaignFundsDatums_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputs_Own_TxOuts_And_CampaignFundsDatums
                            ------------------
                            -- Use fold to sum in a single pass for efficiency
                            !inputSum_Avalaible_FT  = foldl (\acc datum' -> acc + T.cfdSubtotal_Avalaible_FT datum') 0 campaignFundsDatums_In
                            !outputSum_Avalaible_FT = foldl (\acc datum' -> acc + T.cfdSubtotal_Avalaible_FT datum') 0 campaignFundsDatums_Out
                            !inputSum_Avalaible_ADA  = foldl (\acc datum' -> acc + T.cfdSubtotal_Avalaible_ADA datum') 0 campaignFundsDatums_In
                            !outputSum_Avalaible_ADA = foldl (\acc datum' -> acc + T.cfdSubtotal_Avalaible_ADA datum') 0 campaignFundsDatums_Out
                            ------------------
                            -- Check if the non-cfdSubtotal_Avalaible_FT fields are unchanged
                            !validFieldChanges = and $ zipWith sameFieldsExceptAvailable campaignFundsDatums_In campaignFundsDatums_Out
                            ------------------
                        in
                        inputSum_Avalaible_FT == outputSum_Avalaible_FT && 
                        inputSum_Avalaible_ADA == outputSum_Avalaible_ADA && 
                        validFieldChanges
                    ------------------
                    sameFieldsExceptAvailable :: T.CampaignFundsDatumType -> T.CampaignFundsDatumType -> Bool
                    sameFieldsExceptAvailable input output =
                        T.cfdIndex input == T.cfdIndex output &&
                        T.cfdCampaignPolicy_CS input == T.cfdCampaignPolicy_CS output &&
                        T.cfdCampaignFundsPolicyID_CS input == T.cfdCampaignFundsPolicyID_CS output &&
                        T.cfdSubtotal_Sold_FT input == T.cfdSubtotal_Sold_FT output &&
                        T.cfdSubtotal_Collected_ADA input == T.cfdSubtotal_Collected_ADA output &&
                        T.cfdMinADA input == T.cfdMinADA output
                    ------------------
                    isCorrect_Outputs_CampaignFundsDatum_Values_Balanced :: Bool
                    !isCorrect_Outputs_CampaignFundsDatum_Values_Balanced =
                        let
                            -- Validate that Ledger.Value for each datum remains consistent except for the specific token change
                            !validValueChanges = and $ zipWith validateValuesMatch inputs_Own_TxOuts_And_CampaignFundsDatums outputs_Own_TxOuts_And_CampaignFundsDatums
                        in
                        validValueChanges
                    ------------------
                    validateValuesMatch :: (LedgerContextsV2.TxOut, T.CampaignFundsDatumType) -> (LedgerContextsV2.TxOut, T.CampaignFundsDatumType) -> Bool
                    validateValuesMatch (inputTxOut, _) (outputTxOut, outputDatum) =
                        let
                            ------------------
                            !campaignFT_AC = LedgerValue.AssetClass (CampaignT.cdCampaignFT_CS campaignDatum_In, CampaignT.cdCampaignFT_TN campaignDatum_In)
                            ------------------
                            -- Extract the value of the specific token in both input and output
                            ------------------
                            !input_CampaignFT_Value_Amt = OnChainHelpers.getAmt_With_AC_InValue (LedgerApiV2.txOutValue inputTxOut) campaignFT_AC
                            !output_CampaignFT_Value_Amt = OnChainHelpers.getAmt_With_AC_InValue (LedgerApiV2.txOutValue outputTxOut) campaignFT_AC
                            ------------------
                            !input_ADA_Value_Amt = OnChainHelpers.getADAfromValue (LedgerApiV2.txOutValue inputTxOut)
                            !output_ADA_Value_Amt = OnChainHelpers.getADAfromValue (LedgerApiV2.txOutValue outputTxOut)
                            ------------------
                            !input_CampaignFT_Value = LedgerValue.assetClassValue campaignFT_AC input_CampaignFT_Value_Amt
                            !output_CampaignFT_Value = LedgerValue.assetClassValue campaignFT_AC output_CampaignFT_Value_Amt
                            ------------------
                            -- Check that the other tokens remain unchanged
                            !input_Without_CampaignFT_Value  = LedgerApiV2.txOutValue inputTxOut <> negate input_CampaignFT_Value <> negate (OnChainHelpers.createADAValue input_ADA_Value_Amt)
                            !output_Without_CampaignFT_Value = LedgerApiV2.txOutValue outputTxOut <> negate output_CampaignFT_Value <> negate (OnChainHelpers.createADAValue output_ADA_Value_Amt)
                        in
                        -- Ensure the token value matches the datum's cfdSubtotal_Avalaible_FT and the rest of the value is unchanged
                        output_CampaignFT_Value_Amt == T.cfdSubtotal_Avalaible_FT outputDatum &&
                        output_ADA_Value_Amt == (T.cfdSubtotal_Avalaible_ADA outputDatum + T.cfdMinADA outputDatum) &&
                        input_Without_CampaignFT_Value == output_Without_CampaignFT_Value
                ----------------------------------------------------------------------------2
                validateRedeemerMerge :: [T.CampaignFundsDatumType]  -> [Ledger.Value] ->  T.CampaignFundsDatumType  -> Ledger.Value -> Bool
                validateRedeemerMerge !campaignFundsDatums_In !valueOf_CampaignFundsDatums_In !campaignFundsDatum_Out !valueOf_CampaignFundsDatum_Out  =
                    ------------------
                    -- Este redeemer va con Campaign Validator redeemer ValidatorRedeemerFundsMerge
                    ------------------
                    -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
                    -- 2 - it runs along Campaign Funds ID Policy (PolicyRedeemerBurnID)
                    -- 3 - it runs along Campaign Funds Validator, many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
                    ------------------
                    -- 1 - Que sea Protocol or Campaig Admin
                    -- 1 - Que se quemen CampaignFundsIDs con la correcta póliza CS indicada en CampaignDatum
                    -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
                    -- 1 - Que el CampaignDatum se actualiza con el Campaign Funds eliminados
                    -- 1 - Que el CampaignDatum value no cambie
                    -- 2 - Que se quemen CampaignFundsIDs con own póliza
                    -- 2 - Que coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
                    -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de Campaign Funds Validator (en caso de Merge hay una input que no se quema)
                    -- 2 - que el redeemer de Campaign validator sea correcto
                    -- 2 - que el redeemer de todos los Campaign Funds Validator sea correcto
                    -- 3 - Que se quemen CampaignFundsIDs con la correcta póliza CS indicada en CampaignFundDatum
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
                    ------------------
                    -- no hay restricciones temporales
                    ------------------
                    traceIfFalse "not isBurningCampaignFundsID" isBurningCampaignFundsID
                    && traceIfFalse "not isCorrect_Output_CampaignFundsDatum_SubTotal_Added" isCorrect_Output_CampaignFundsDatum_SubTotal_Added
                    && traceIfFalse "not isCorrect_Output_CampaignFundsDatum_Value_Added" isCorrect_Output_CampaignFundsDatum_Value_Added
                    ------------------
                    where
                        ---------------------
                        !minADA = sum (T.cfdMinADA <$> campaignFundsDatums_In)
                        ---------------------
                        isCorrect_Output_CampaignFundsDatum_SubTotal_Added :: Bool
                        !isCorrect_Output_CampaignFundsDatum_SubTotal_Added =
                            let
                                ---------------------
                                !avalaible_FT = sum (T.cfdSubtotal_Avalaible_FT <$> campaignFundsDatums_In)
                                !sold_FT = sum (T.cfdSubtotal_Sold_FT  <$> campaignFundsDatums_In )
                                !avalaible_ADA = sum (T.cfdSubtotal_Avalaible_ADA <$> campaignFundsDatums_In)
                                !collected_ADA = sum (T.cfdSubtotal_Collected_ADA <$> campaignFundsDatums_In)
                                ---------------------
                                !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFundsDatum_With_MergedSubtotals 
                                    campaignFundsDatum_Out avalaible_FT sold_FT avalaible_ADA collected_ADA minADA 
                            in
                                campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignDatum_Out_Control
                        ---------------------
                        isCorrect_Output_CampaignFundsDatum_Value_Added :: Bool
                        !isCorrect_Output_CampaignFundsDatum_Value_Added =
                            let
                                ------------------
                                !campaignFunds_Index = T.cfdIndex campaignFundsDatum_Out
                                !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
                                !campaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
                                ------------------
                                valueesOf_CampaignFundsDatums_In = OnChainHelpers.sumValues valueOf_CampaignFundsDatums_In
                                ---------------------n
                                !valueFor_Burning_CampaignFundsID = OnChainHelpers.getValueOfCurrencySymbol valueesOf_CampaignFundsDatums_In campaignFundsPolicyID_CS
                                !valueFor_Remaining_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC 1
                                ---------------------n
                                !valueFor_CampaignDatum_Control = valueesOf_CampaignFundsDatums_In <> valueFor_Remaining_CampaignFundsID <> valueFor_Burning_CampaignFundsID
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignDatum_Control
                        ------------------
                        isBurningCampaignFundsID :: Bool
                        !isBurningCampaignFundsID = OnChainHelpers.isToken_Burning_With_CS campaignFundsPolicyID_CS info
                --------------------------------------------------------------------------------22
                validateRedeemerDelete :: Bool
                validateRedeemerDelete =
                    ------------------
                    -- Este redeemer va con Campaign Validator redeemer ValidatorRedeemerFundsDelete
                    ------------------
                    -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
                    -- 2 - it runs along Campaign Funds ID Policy (PolicyRedeemerBurnID)
                    -- 3 - it runs along Campaign Funds Validator, many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
                    ------------------
                    -- 1 - Que sea Protocol or Campaig Admin
                    -- 1 - Que se quemen CampaignFundsIDs con la correcta póliza CS indicada en CampaignDatum
                    -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
                    -- 1 - Que el CampaignDatum se actualiza con el Campaign Funds eliminados
                    -- 1 - Que el CampaignDatum value no cambie
                    -- 2 - Que se quemen CampaignFundsIDs con own póliza
                    -- 2 - Que coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
                    -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de Campaign Funds Validator (en caso de Merge hay una input que no se quema)
                    -- 2 - que el redeemer de Campaign validator sea correcto
                    -- 2 - que el redeemer de todos los Campaign Funds Validator sea correcto
                    -- 3 - Que se quemen CampaignFundsIDs con la correcta póliza CS indicada en CampaignFundDatum
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
                    ------------------
                    -- no hay restricciones temporales
                    ------------------
                    traceIfFalse "not isBurningCampaignFundsID" isBurningCampaignFundsID
                    && traceIfFalse "not isCorrect_Input_CampaignDatum_SubTotal_Zero" isCorrect_Input_CampaignDatum_SubTotal_Zero
                    && traceIfFalse "not isCorrect_Input_CampaignDatum_Value_Zero" isCorrect_Input_CampaignDatum_Value_Zero
                    ------------------
                    where
                        ---------------------
                        isCorrect_Input_CampaignDatum_SubTotal_Zero :: Bool
                        !isCorrect_Input_CampaignDatum_SubTotal_Zero =
                            T.cfdSubtotal_Avalaible_FT campaignFundsDatum_In == 0 &&
                            T.cfdSubtotal_Sold_FT campaignFundsDatum_In == 0 && 
                            T.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In == 0 &&
                            T.cfdSubtotal_Collected_ADA campaignFundsDatum_In == 0 
                        ---------------------
                        isCorrect_Input_CampaignDatum_Value_Zero :: Bool
                        !isCorrect_Input_CampaignDatum_Value_Zero =
                            let
                                !valueFor_CampaignDatum_Control = valueOf_CampaignFundsDatum_In <> LedgerAda.lovelaceValueOf (T.cfdMinADA campaignFundsDatum_In)
                            in
                                valueOf_CampaignFundsDatum_In `OnChainHelpers.isEqValue` valueFor_CampaignDatum_Control
                        ------------------
                        isBurningCampaignFundsID :: Bool
                        !isBurningCampaignFundsID = OnChainHelpers.isToken_Burning_With_CS campaignFundsPolicyID_CS info
                --------------------------------------------------------------------------------

----------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID campaignPolicy_CS campaignFundsValidator_Hash = mkPolicyID params
    where
        params = T.PolicyParams
            { ppCampaignPolicy_CS  = PlutusTx.unsafeFromBuiltinData campaignPolicy_CS,
            ppCampaignFundsValidator_Hash  = PlutusTx.unsafeFromBuiltinData campaignFundsValidator_Hash
            }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = Plutonomy.optimizeUPLC $$( PlutusTx.compile [|| mkWrappedPolicyID ||])

----------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params


{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS  tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params = T.ValidatorParams
            {
            vpProtocolPolicyID_CS  = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS,
            vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
            }

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = Plutonomy.optimizeUPLC $$( PlutusTx.compile [|| mkWrappedValidator ||])

----------------------------------------------------------------------------2

