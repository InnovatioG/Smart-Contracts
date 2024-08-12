{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Constants                 as T
import qualified Helpers.Constants         as T
import qualified Helpers.OnChain           as OnChainHelpers
import qualified Protocol.Helpers          as ProtocolHelpers
import qualified Protocol.Types            as T
import qualified Types            as T


--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams ->   BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !protocolPolicyID_TxOutRef) _ !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && validateRedeemer
        then ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = True
        ------------------
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !protocolPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        ------------------
        !valueFor_Mint_ProtocolID = LedgerValue.assetClassValue protocolID_AC 1
        ------------------
        isMintingID :: Bool
        !isMintingID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_ProtocolID
        -----------------
        !outputs_txOuts =
            [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
            ]
        ------------------
        !_ = case outputs_txOuts of
            [] -> traceError "Expected at least one output to scripts addresses"
            _  -> ()
        ------------------
        -- 0 out is the ProtocolDatum
        ------------------
        !output_TxOut_And_ProtocolDatum =
            fromMaybe
                (traceError "Expected Protocol at output to script index 0")
                ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                    @T.ValidatorDatum
                    @T.ProtocolDatumType
                    ctx
                    (head outputs_txOuts)
                    protocolID_AC
                    Nothing
                    T.getProtocolDatumType
                )
        ------------------
        !protocolDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_ProtocolDatum
        ---------------------
        !minADA_For_ProtocolDatum = T.pdMinADA protocolDatum_Out
        !value_MinADA_For_ProtocolDatum = LedgerAda.lovelaceValueOf minADA_For_ProtocolDatum
        !valueFor_ProtocolDatum_Out_Control = valueFor_Mint_ProtocolID <> value_MinADA_For_ProtocolDatum
        ---------------------
        !protocolDatum_Out_Control =
            T.mkProtocolDatumType
                (T.pdAdmins protocolDatum_Out)
                (T.pdTokenAdminPolicy_CS protocolDatum_Out)
                minADA_For_ProtocolDatum
        ---------------------
        isCorrect_Output_ProtocolDatum :: Bool
        !isCorrect_Output_ProtocolDatum =
            protocolDatum_Out

                `OnChainHelpers.isUnsafeEqDatums` protocolDatum_Out_Control
                ------------------
        isCorrect_Output_ProtocolDatum_Value :: Bool
        !isCorrect_Output_ProtocolDatum_Value =
            let
                !valueOf_ProtocolDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_TxOut_And_ProtocolDatum
            in
                valueOf_ProtocolDatum_Out `OnChainHelpers.isEqValue` valueFor_ProtocolDatum_Out_Control
        -----------------
        validateRedeemer :: Bool
        validateRedeemer =
            -----------------
            -- Que se consuma utxo en parámetro de la póliza
            -- Que se este minteando NFT con el nombre correcto
            -----------------
            traceIfFalse "not isTxOutAnInput" (OnChainHelpers.isTxOutAnInput protocolPolicyID_TxOutRef info)
                && traceIfFalse "not isMintingID" isMintingID
                && traceIfFalse "not isCorrect_Output_ProtocolDatum" isCorrect_Output_ProtocolDatum
                && traceIfFalse "not isCorrect_Output_ProtocolDatum_Value" isCorrect_Output_ProtocolDatum_Value

------------------

--------------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
    let
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        -- Si esta el token de emergencia se saltea todos los controles
        isEmergencyRedeemer :: Bool
        !isEmergencyRedeemer =
            case redeemer of
                (T.ValidatorRedeemerEmergency _) -> True
                _                                -> False
    in
        ------------------
        case isEmergencyRedeemer of
            True ->
                let
                    !tokenEmergencyAdmin_AC = LedgerValue.AssetClass (tokenEmergencyAdminPolicy_CS, T.tokenEmergencyAdmin_TN)
                    -- search emergency admin token in output 0
                    !isEmergencyAdminTokenPresent = OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head (LedgerApiV2.txInfoOutputs info)) tokenEmergencyAdmin_AC
                in
                    if traceIfFalse "not isEmergencyAdminTokenPresent" isEmergencyAdminTokenPresent
                        then ()
                        else error ()
            False ->
                if traceIfFalse "" useThisToMakeScriptUnique
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
                    && traceIfFalse "Expected exactly one Protocol input" (length inputs_Own_TxOuts == 1)
                    && (
                         -- Si no, se valida que el que firma sea admin o que este el token admin y luego se valida el redeemer
                         validateAdminAction
                            && validateRedeemerAdmin
                       )
                    then ()
                    else error ()
                where
                    ------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !protocol_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                     ------------------
                    !inputs_Own_TxOuts = [LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info,
                        let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                        in  OnChainHelpers.isScriptAddress address && address == protocol_Validator_Address]
                    ------------------
                    !outputs_txOuts =
                        [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                        ]
                    ------------------
                    !_ = case outputs_txOuts of
                        [] -> traceError "Expected at least one output to scripts addresses"
                        _  -> ()
                    ------------------
                    -- filter only outputs at scripts addresses
                    -- 0 out is the ProtocolDatum
                    ------------------
                    !output_Own_TxOut_And_ProtocolDatum =
                        fromMaybe
                            (traceError "Expected Protocol at output to script index 0")
                            ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address @T.ValidatorDatum @T.ProtocolDatumType
                                ctx
                                (head outputs_txOuts)
                                protocolID_AC
                                (Just protocol_Validator_Address)
                                T.getProtocolDatumType
                            )
                    ------------------
                    !protocolDatum_In = T.getProtocolDatumType datum
                    ------------------
                    !valueOf_ProtocolDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                    ------------------
                    validateAdminAction :: Bool
                    !validateAdminAction =
                        -- Que este el token de admin presente en output 0
                        -- o Que sea Protocol Admin
                        traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
                        where
                            !admins = T.getAdmins protocolDatum_In
                            ------------------
                            isAdminTokenPresent :: Bool
                            isAdminTokenPresent = case LedgerApiV2.txInfoOutputs info of
                                []         -> False
                                -- search admin token in output 0
                                (output:_) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                                where
                                    !tokenAdminPolicy_CS = T.getAdminToken_CS protocolDatum_In
                                    !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.tokenAdmin_TN)
                    ------------------
                    validateRedeemerAdmin :: Bool
                    validateRedeemerAdmin =
                        case redeemer of
                            (T.ValidatorRedeemerDatumUpdate _) ->
                                ------------------
                                -- Que el ProtocolDatum regrese a Protocol Val
                                -- Que el ProtocolDatum se actualiza correctamente
                                -- Que el ProtocolDatum value no cambie
                                ------------------
                                traceIfFalse "not isCorrect_Output_ProtocolDatum_Updated" isCorrect_Output_ProtocolDatum_Updated
                                    && traceIfFalse "not isCorrect_Output_ProtocolDatum_Value_NotChanged" isCorrect_Output_ProtocolDatum_Value_NotChanged
                                where
                                    ---------------------
                                    !protocolDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                                    ---------------------
                                    !protocolDatum_Out_Control =
                                        ProtocolHelpers.mkUpdated_ProtocolDatum_With_NormalChanges
                                            protocolDatum_In
                                            (T.pdAdmins protocolDatum_Out)
                                            (T.pdTokenAdminPolicy_CS protocolDatum_Out)
                                    ---------------------
                                    isCorrect_Output_ProtocolDatum_Updated :: Bool
                                    !isCorrect_Output_ProtocolDatum_Updated =
                                        protocolDatum_Out
                                            `OnChainHelpers.isUnsafeEqDatums` protocolDatum_Out_Control
                                            ------------------
                                    isCorrect_Output_ProtocolDatum_Value_NotChanged :: Bool
                                    !isCorrect_Output_ProtocolDatum_Value_NotChanged =
                                        let
                                            !valueOf_ProtocolDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                                            !valueFor_ProtocolDatum_Control = valueOf_ProtocolDatum_In
                                        in
                                            valueOf_ProtocolDatum_Out `OnChainHelpers.isEqValue` valueFor_ProtocolDatum_Control
                            (T.ValidatorRedeemerUpdateMinADA _) ->
                                ------------------
                                -- Que el ProtocolDatum regrese a Protocol Val
                                -- Que el ProtocolDatum se actualiza correctamente
                                -- Que el ProtocolDatum value cambie con el min ADA nuevo
                                ------------------
                                traceIfFalse "not isCorrect_Output_ProtocolDatum_UpdatedMinADA" isCorrect_Output_ProtocolDatum_UpdatedMinADA
                                    && traceIfFalse "not isCorrect_Output_ProtocolDatum_Value_ChangedADA" isCorrect_Output_ProtocolDatum_Value_ChangedADA
                                where
                                    ------------------
                                    !protocolDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                                    ------------------
                                    !newMinADA = T.pdMinADA protocolDatum_Out
                                    ------------------
                                    isCorrect_Output_ProtocolDatum_UpdatedMinADA :: Bool
                                    !isCorrect_Output_ProtocolDatum_UpdatedMinADA =
                                        let
                                            !protocolDatum_Out_Control =
                                                ProtocolHelpers.mkUpdated_ProtocolDatum_With_NewMinADA
                                                    protocolDatum_In
                                                    newMinADA
                                        in
                                            protocolDatum_Out `OnChainHelpers.isUnsafeEqDatums` protocolDatum_Out_Control
                                    ------------------
                                    isCorrect_Output_ProtocolDatum_Value_ChangedADA :: Bool
                                    !isCorrect_Output_ProtocolDatum_Value_ChangedADA =
                                        let
                                            !valueOf_ProtocolDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_ProtocolDatum
                                            !valueFor_ProtocolDatum_Control = valueOf_ProtocolDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.pdMinADA protocolDatum_In)
                                        in
                                            valueOf_ProtocolDatum_Out `OnChainHelpers.isEqValue` valueFor_ProtocolDatum_Control
                            _ -> traceIfFalse "incorrect redeemer" False

------------------

--------------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID protocolPolicy_TxHash protocolPolicy_TxOutputIndex = mkPolicyID params
    where
        tid = PlutusTx.unsafeFromBuiltinData protocolPolicy_TxHash :: BuiltinByteString
        txout =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid
                , LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData protocolPolicy_TxOutputIndex
                }
        params =
            T.PolicyParams
                { ppProtocolPolicyID_TxOutRef = txout
                }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicyID||])

--------------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params =
            T.ValidatorParams
                { vpProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
                }

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedValidator||])

--------------------------------------------------------------------------------2
