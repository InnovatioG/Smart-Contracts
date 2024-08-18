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
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Campaign.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger
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

import qualified Campaign.Funds.Types      as CampaignFundsT
import qualified Campaign.Helpers          as CampaignHelpers
import qualified Campaign.Types            as T
import qualified Constants                 as T
import qualified Helpers.Constants         as T2
import qualified Helpers.OnChain    as OnChainHelpers (enumFromTo, getDatum_In_TxOut_And_Datum, getRedeemerForConsumeInput, getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS, getTxOuts_And_DatumTypes_From_TxOuts_By_AC,
                                                              getTxOuts_And_DatumTypes_From_TxOuts_By_CS, getUnsafe_Own_Input_TxOut, getValue_In_TxOut_And_Datum, isDateInRange, isDateReached, isEqValue, isNFT_Burning_With_AC, isNFT_Minting_With_CS,
                                                              isSignedByAny, isToken_Burning_With_CS_AndAmt, isToken_With_AC_InValue, isUnsafeEqDatums, isUnsafeEqDatumsCostly, isValidRange, isScriptAddress, getUnsafeOwnMintingValue, getTxOut_And_DatumType_From_TxOut_And_AC_And_Address, isTxOutAnInput, isDateNotReached)
import qualified Helpers.Types             as T
import qualified Protocol.Types            as ProtocolT
import qualified Types                     as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2

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
                traceIfFalse "not isEmergencyAdminTokenPr33esent" isEmergencyAdminTokenPresent
        ----------------------------------------------------------------------------2
        validateNonEmergencyRedeemers :: Bool
        validateNonEmergencyRedeemers =
            let
                !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                !campaignDatum_In = T.getCampaignDatumType datum
            in
                traceIfFalse "not campaignDatum_In" (campaignDatum_In `OnChainHelpers.isUnsafeEqDatums` campaignDatum_In)
        -- --------------------------------------------------------------------------------2
        -- validateNonEmergencyRedeemers ::  Bool
        -- validateNonEmergencyRedeemers  =
        --         ------------------
        --         traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        --         && validateRedeemers
        --         ------------------
        --     where
        --         ------------------
        --         !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        --         !campaign_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
        --         ------------------
        --         -- NOTE: inputsRef_TxOuts es optional, por eso no se usa el BANG !
        --         ------------------
        --         -- inputsRef_TxOuts_ =
        --         --     [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info,
        --         --             isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
        --         --     ]
        --         -- !OnChainHelpers.enumFromTo = OnChainHelpers.enumFromTo
        --         -- !OnChainHelpers.getRedeemerForConsumeInput = OnChainHelpers.getRedeemerForConsumeInput
        --         -- -- !OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS = OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
        --         -- !OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC_Campaign = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC @T.ValidatorDatum @T.CampaignDatumType
        --         -- !OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC_Protocol = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC @ProtocolT.ValidatorDatum @ProtocolT.ProtocolDatumType
        --         -- -- !OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
        --         -- !OnChainHelpers.getUnsafe_Own_Input_TxOut = OnChainHelpers.getUnsafe_Own_Input_TxOut
        --         -- !OnChainHelpers.getDatum_In_TxOut_And_Datum = OnChainHelpers.getDatum_In_TxOut_And_Datum
        --         -- !OnChainHelpers.getValue_In_TxOut_And_Datum = OnChainHelpers.getValue_In_TxOut_And_Datum
        --         -- !OnChainHelpers.isDateInRange = OnChainHelpers.isDateInRange
        --         -- !OnChainHelpers.isDateReached = OnChainHelpers.isDateReached
        --         -- !OnChainHelpers.isEqValue = OnChainHelpers.isEqValue
        --         -- !OnChainHelpers.isNFT_Burning_With_AC = OnChainHelpers.isNFT_Burning_With_AC
        --         -- !OnChainHelpers.isNFT_Minting_With_CS = OnChainHelpers.isNFT_Minting_With_CS
        --         -- !OnChainHelpers.isSignedByAny = OnChainHelpers.isSignedByAny
        --         -- !OnChainHelpers.isToken_Burning_With_CS_AndAmt = OnChainHelpers.isToken_Burning_With_CS_AndAmt
        --         -- !OnChainHelpers.isToken_With_AC_InValue = OnChainHelpers.isToken_With_AC_InValue
        --         -- !OnChainHelpers.isUnsafeEqDatums = OnChainHelpers.isUnsafeEqDatumsCostly @T.CampaignDatumType
        --         -- !OnChainHelpers.isValidRange = OnChainHelpers.isValidRange
        --         -- !OnChainHelpers.isScriptAddress = OnChainHelpers.isScriptAddress


        --         inputsRef_TxOuts_ =
        --             let
        --                 !list = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info]
        --             in
        --                 filter ( OnChainHelpers.isScriptAddress . LedgerApiV2.txOutAddress) list
        --         ------------------
        --         !inputs_TxOuts =
        --             let
        --                 !list = [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoInputs info]
        --             in
        --                 filter ( OnChainHelpers.isScriptAddress . LedgerApiV2.txOutAddress) list
        --         ------------------
        --         !inputs_Own_TxOuts = filter (\txOut -> LedgerApiV2.txOutAddress txOut == campaign_Validator_Address) inputs_TxOuts
        --         ------------------
        --         !outputs_Own_txOuts =
        --             let
        --                 !list = LedgerApiV2.txInfoOutputs info
        --                 valid txOut = 
        --                     let address = LedgerApiV2.txOutAddress txOut
        --                     in  OnChainHelpers.isScriptAddress address && address == campaign_Validator_Address
        --             in filter valid list
        --         ------------------
        --         -- NOTE: inputs_Own_TxOuts_And_CampaignFundsDatums es optional, por eso no se usa el BANG !
        --         ------------------
        --         output_Own_TxOut_And_CampaignDatum_ =
        --             fromMaybe
        --                 (traceError "Expected Campaign at own output")
        --                 ( case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
        --                     ctx
        --                     outputs_Own_txOuts
        --                     campaignID_AC
        --                     T.getCampaignDatumType of
        --                     [x] -> Just x
        --                     _   -> Nothing
        --                 )
        --         ------------------
        --         !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        --         ------------------
        --         !campaignDatum_In = T.getCampaignDatumType datum
        --         ------------------
        --         !campaignPolicy_CS = T.cdCampaignPolicy_CS campaignDatum_In
        --         !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        --         ------------------
        --         !valueOf_CampaignDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
        --         ------------------
        --         -- NOTE: campaignDatum_Out_ y valueOf_CampaignDatum_Out_ son optionales, por eso no se usa el BANG !
        --         ------------------
        --         campaignDatum_Out_ = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_CampaignDatum_
        --         valueOf_CampaignDatum_Out_ = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignDatum_
        --         --------------------------------------------------------------------------------
        --         -- NOTE: ProtocolDatum por inputRef son optionales, por eso no se usa el BANG !
        --         ------------------
        --         protocolID_AC_ = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        --         ------------------
        --         inputRef_TxOut_And_ProtocolDatum'_ =
        --             case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
        --                 ctx
        --                 inputsRef_TxOuts_
        --                 protocolID_AC_
        --                 ProtocolT.getProtocolDatumType of
        --                     [x] -> Just x
        --                     _   -> Nothing
        --         ------------------
        --         isCampaignOpen :: Bool
        --         isCampaignOpen = OnChainHelpers.isDateReached (T.cdBeginAt campaignDatum_In) info && not (OnChainHelpers.isDateReached (T.cdDeadline campaignDatum_In) info)
        --          ------------------
        --         isCampaignClosed :: Bool
        --         isCampaignClosed  = not isCampaignOpen
        --         ------------------
        --         isCampaignStatus :: T.CapaignStatus -> Bool
        --         isCampaignStatus !status = T.cdStatus campaignDatum_In == status
        --         ------------------
        --         isAdminTokenPresent :: Bool
        --         isAdminTokenPresent =
        --         ------------------
        --         -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
        --         ------------------
        --             case LedgerApiV2.txInfoOutputs info of
        --                 []         -> False
        --                 -- search admin token in output 0
        --                 (output:_) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
        --                 where
        --                     !tokenAdminPolicy_CS = T.getAdminToken_CS campaignDatum_In
        --                     !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.tokenAdmin_TN)
        --         ------------------
        --         validateCampaignAdminAction :: Bool
        --         validateCampaignAdminAction =
        --         ------------------
        --         -- NOTE: validateCampaignAdminAction es optional, por eso no se usa el BANG !
        --         ------------------
        --             -- Que este el token de admin presente
        --             -- o Que sea Campaign Admin
        --             ------------------
        --             traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
        --             where
        --                 !admins = T.getAdmins campaignDatum_In
        --         ------------------
        --         validateProtocolAdminAction :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> Bool
        --         validateProtocolAdminAction !inputRef_TxOut_And_ProtocolDatum' =
        --         ------------------
        --         -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
        --         ------------------
        --             -- Que este el token de admin presente
        --             -- o Que sea Campaign Admin
        --             -- o Que sea Protocol Admin si hay input ref protocol
        --             ------------------
        --             traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
        --             where
        --                 ------------------
        --                 !admins = case inputRef_TxOut_And_ProtocolDatum' of
        --                         Just x ->
        --                             let
        --                                 ------------------
        --                                 !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum x
        --                                 ------------------
        --                             in T.getAdmins  protocolDatum_In
        --                         _ -> traceError  "Expected Protocol at inputRef"
        --         ------------------
        --         validateProtocolOrCampaignAdminAction :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> Bool
        --         validateProtocolOrCampaignAdminAction !inputRef_TxOut_And_ProtocolDatum' =
        --         ------------------
        --         -- NOTE: isAdminTokenPresent es optional, por eso no se usa el BANG !
        --         ------------------
        --             -- Que este el token de admin presente
        --             -- o Que sea Campaign Admin
        --             -- o Que sea Protocol Admin si hay input ref protocol
        --             ------------------
        --             traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins info || isAdminTokenPresent)
        --             where
        --                 ------------------
        --                 !admins = T.getAdmins campaignDatum_In ++
        --                     case inputRef_TxOut_And_ProtocolDatum' of
        --                         Just x ->
        --                             let
        --                                 ------------------
        --                                 !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum x
        --                                 ------------------
        --                             in T.getAdmins  protocolDatum_In
        --                         _ -> []
        --         ------------------
        --         isCorrect_Output_CampaignDatum_Value_NotChanged :: Ledger.Value -> Bool
        --         isCorrect_Output_CampaignDatum_Value_NotChanged !valueOf_CampaignDatum_Out =
        --             let
        --                 !valueFor_CampaignDatum_Out_Control = valueOf_CampaignDatum_In
        --             in
        --                 valueOf_CampaignDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignDatum_Out_Control
        --         --------------------------------------------------------------------------------
        --         validateRedeemers :: Bool
        --         !validateRedeemers =
        --             case redeemer of
        --                 (T.ValidatorRedeemerDelete _) -> validate_One_Input_No_Outputs_Redeemers
        --                 _                             -> validate_One_Input_One_Output_Redeemers campaignDatum_Out_ valueOf_CampaignDatum_Out_
        --         --------------------------------------------------------------------------------
        --         -- NOTE: todos los validadores son optionales al redeemer, por eso no se usa el BANG !
        --         --------------------------------------------------------------------------------
        --         validate_One_Input_No_Outputs_Redeemers :: Bool
        --         validate_One_Input_No_Outputs_Redeemers =
        --             traceIfFalse "not length inputs_Own_TxOuts == 1" (length inputs_Own_TxOuts == 1) &&
        --             traceIfFalse "not null outputs_Own_txOuts" ( null outputs_Own_txOuts  ) &&
        --             case redeemer of
        --                 (T.ValidatorRedeemerDelete _) -> validateRedeemerDelete inputRef_TxOut_And_ProtocolDatum'_
        --                 _                             -> False
        --         --------------------------------------------------------------------------------
        --         validate_One_Input_One_Output_Redeemers :: T.CampaignDatumType -> Ledger.Value -> Bool
        --         validate_One_Input_One_Output_Redeemers !campaignDatum_Out !valueOf_CampaignDatum_Out = 
        --             let
        --                 --------------------------
        --                 caseValidateBasicUpdateRedeemers = 1
        --                 caseValidateFundsManageRedeemers = 2
        --                 caseValidateUpdateStatusCampaignRedeemers = 3
        --                 caseValidateRedeemerMilestoneManage = 4
        --                 caseValidateOthers = 5
        --                 --------------------------
        --                 redeemerType =
        --                     case redeemer of
        --                         (T.ValidatorRedeemerDatumUpdate _)        -> caseValidateBasicUpdateRedeemers
        --                         (T.ValidatorRedeemerUpdateMinADA _)       -> caseValidateBasicUpdateRedeemers
        --                         (T.ValidatorRedeemerFundsMerge _)         -> caseValidateFundsManageRedeemers
        --                         (T.ValidatorRedeemerFundsDelete _)        ->  caseValidateFundsManageRedeemers
        --                         (T.ValidatorRedeemerInitializeCampaign _) -> caseValidateUpdateStatusCampaignRedeemers
        --                         (T.ValidatorRedeemerReachedCampaign _)    -> caseValidateUpdateStatusCampaignRedeemers
        --                         (T.ValidatorRedeemerNotReachedCampaign _) ->  caseValidateUpdateStatusCampaignRedeemers
        --                         (T.ValidatorRedeemerMilestoneAprobe _)    -> caseValidateRedeemerMilestoneManage
        --                         (T.ValidatorRedeemerMilestoneReprobe _)   -> caseValidateRedeemerMilestoneManage
        --                         _                                         -> caseValidateOthers
        --                 --------------------------
        --                 validateRedeemer :: Integer -> Bool
        --                 validateRedeemer redeemerType'
        --                     | redeemerType' == caseValidateBasicUpdateRedeemers = validateBasicUpdateRedeemers inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_Out valueOf_CampaignDatum_Out
        --                     | redeemerType' == caseValidateFundsManageRedeemers = validateFundsManageRedeemers inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_Out valueOf_CampaignDatum_Out
        --                     | redeemerType' == caseValidateUpdateStatusCampaignRedeemers = validateUpdateStatusCampaignRedeemers inputsRef_TxOuts_ inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_Out valueOf_CampaignDatum_Out
        --                     | redeemerType' == caseValidateRedeemerMilestoneManage = validateRedeemerMilestoneManage inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_Out valueOf_CampaignDatum_Out
        --                     | otherwise =
        --                         case redeemer of
        --                             (T.ValidatorRedeemerFundsAdd _)             -> validateFundsAddRedeemer inputRef_TxOut_And_ProtocolDatum'_ campaignDatum_Out valueOf_CampaignDatum_Out
        --                             (T.ValidatorRedeemerFundsCollect redeemer') ->  validateFundsCollectRedeemers redeemer' campaignDatum_Out valueOf_CampaignDatum_Out
        --                             _                                           -> False
        --                 ------------------------
        --             in

        --                 traceIfFalse "not length inputs_Own_TxOuts == 1" (length inputs_Own_TxOuts == 1) &&
        --                 traceIfFalse "not length outputs_Own_txOuts == 1" (length outputs_Own_txOuts == 1) &&
        --                 validateRedeemer redeemerType
        --         ----------------------------------------------------------------------------
        --         validateRedeemerDelete :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> Bool
        --         validateRedeemerDelete !inputRef_TxOut_And_ProtocolDatum' = 
        --             ------------------
        --             -- it runs along with Campaign ID Policy  (PolicyRedeemerBurnID)
        --             ------------------
        --             -- Que sea Protocol Admin
        --             -- que el Campaign tenga CERO CampaignFunds
        --             -- Que se quemen los Campaign ID con la correcta póliza indicada en CampaignDatum siendo consumido
        --             ------------------
        --                 validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                 traceIfFalse "not isBurningCampaignID" isBurningCampaignID &&
        --                 traceIfFalse "not isZeroCampaignFunds" isZeroCampaignFunds
        --             where
        --                 ------------------
        --                 isBurningCampaignID :: Bool
        --                 !isBurningCampaignID = OnChainHelpers.isNFT_Burning_With_AC campaignID_AC info
        --                 ------------------
        --                 isZeroCampaignFunds :: Bool
        --                 !isZeroCampaignFunds = T.cdFundsCount campaignDatum_In == 0
        --         --------------------------------------------------------------------------------
        --         validateBasicUpdateRedeemers :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateBasicUpdateRedeemers !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_Out !valueOf_CampaignDatum_Out =
        --             case redeemer of
        --                 (T.ValidatorRedeemerDatumUpdate _)  -> 
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- solo es posible actualizar admin y token admin
        --                     -- Que sea Protocol or Campaign Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- no hay restricciones temporales
        --                     ---------------------
        --                         validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" isCorrect_Output_CampaignDatum_Updated
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                     where
        --                         ---------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Bool
        --                         !isCorrect_Output_CampaignDatum_Updated =
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_NormalChanges campaignDatum_In (T.cdAdmins campaignDatum_Out) (T.cdTokenAdminPolicy_CS campaignDatum_Out)
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                 ------------------
        --                 (T.ValidatorRedeemerUpdateMinADA _) -> 
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- Que sea Protocol or Campaign Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- Que el CampaignDatum se actualiza correctamente
        --                     -- Que el CampaignDatum value cambie con el min ADA nuevo
        --                     -- no hay restricciones temporales
        --                     ------------------
        --                         validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_UpdatedMinADA" isCorrect_Output_CampaignDatum_UpdatedMinADA
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_ChangedADA" isCorrect_Output_CampaignDatum_Value_ChangedADA
        --                     where
        --                         ------------------
        --                         !newMinADA = T.cdMinADA campaignDatum_Out
        --                         ------------------
        --                         isCorrect_Output_CampaignDatum_UpdatedMinADA :: Bool
        --                         !isCorrect_Output_CampaignDatum_UpdatedMinADA =
        --                             let
        --                                 !campaignDatum_Out_Control =
        --                                     CampaignHelpers.mkUpdated_CampaignDatum_With_NewMinADA
        --                                         campaignDatum_In
        --                                         newMinADA
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                         ------------------
        --                         isCorrect_Output_CampaignDatum_Value_ChangedADA :: Bool
        --                         !isCorrect_Output_CampaignDatum_Value_ChangedADA =
        --                             let
        --                                 -- !valueOf_CampaignDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignDatum
        --                                 !valueFor_CampaignDatum_Control = valueOf_CampaignDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.cdMinADA campaignDatum_In)
        --                             in
        --                                 valueOf_CampaignDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignDatum_Control
        --                 ------------------
        --                 _                                   -> False
        --         --------------------------------------------------------------------------------
        --         validateRedeemerMilestoneManage :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateRedeemerMilestoneManage !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_Out !valueOf_CampaignDatum_Out =
        --             case redeemer of
        --                 (T.ValidatorRedeemerMilestoneAprobe (T.ValidatorRedeemerMilestoneAprobeType rmaMilestoneIndex))   -> 
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- solo es posible actualizar admin y token admin
        --                     -- Que sea Protocol Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- que este en status CsReached
        --                     -- que el milestone anterior este en status MsSuccess
        --                     -- que el nuevo milestone actual este en status MsCreated
        --                     ---------------------
        --                         validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrectRedeemerMilestoneIndex" (isCorrectRedeemerMilestoneIndex rmaMilestoneIndex)
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" (isCorrect_Output_CampaignDatum_Updated rmaMilestoneIndex)
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isCampaignReached" isCampaignReached
        --                         && traceIfFalse "not isPreviusMilestoneAprobed" (isPreviusMilestoneAprobed rmaMilestoneIndex)
        --                         && traceIfFalse "not isCurrentMilestoneCreated" (isCurrentMilestoneCreated rmaMilestoneIndex)
        --                     where
        --                         ------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Integer -> Bool
        --                         isCorrect_Output_CampaignDatum_Updated milestoneIndex=
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_MilestoneAprobed campaignDatum_In milestoneIndex
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                 ------------------
        --                 (T.ValidatorRedeemerMilestoneReprobe (T.ValidatorRedeemerMilestoneReprobeType rmrMilestoneIndex)) -> 
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- solo es posible actualizar admin y token admin
        --                     -- Que sea Protocol Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- que este en status CsReached
        --                     -- que el milestone anterior este en status MsSuccess
        --                     -- que el nuevo milestone actual este en status MsCreated
        --                     ---------------------
        --                         validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrectRedeemerMilestoneIndex" (isCorrectRedeemerMilestoneIndex rmrMilestoneIndex)
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" (isCorrect_Output_CampaignDatum_Updated rmrMilestoneIndex)
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isCampaignReached" isCampaignReached
        --                         && traceIfFalse "not isPreviusMilestoneAprobed" (isPreviusMilestoneAprobed rmrMilestoneIndex)
        --                         && traceIfFalse "not isCurrentMilestoneCreated" (isCurrentMilestoneCreated rmrMilestoneIndex)
        --                     where
        --                         ------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Integer -> Bool
        --                         isCorrect_Output_CampaignDatum_Updated milestoneIndex=
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_MilestoneReprobed campaignDatum_In milestoneIndex
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                         ----------------
        --                 _                                                                                                 -> False
        --             where
        --                     isCorrectRedeemerMilestoneIndex :: Integer -> Bool
        --                     isCorrectRedeemerMilestoneIndex milestoneIndex =
        --                             milestoneIndex >= 0 && milestoneIndex < length (T.cdMilestones campaignDatum_In)
        --                     ---------------------
        --                     isCampaignReached :: Bool
        --                     !isCampaignReached = T.cdStatus campaignDatum_In == T.CsReached
        --                     ------------------
        --                     isPreviusMilestoneAprobed :: Integer -> Bool
        --                     isPreviusMilestoneAprobed milestoneIndex
        --                         | milestoneIndex == 0 = True
        --                         | otherwise =
        --                             let
        --                                 milestones = T.cdMilestones campaignDatum_In
        --                                 milestone = milestones !! (milestoneIndex - 1)
        --                             in
        --                                 T.cmStatus milestone == T.MsSuccess
        --                     ------------------
        --                     isCurrentMilestoneCreated :: Integer ->Bool
        --                     isCurrentMilestoneCreated milestoneIndex =
        --                         let
        --                             !milestones = T.cdMilestones campaignDatum_In
        --                             !milestone = milestones !! milestoneIndex
        --                         in
        --                             T.cmStatus milestone == T.MsCreated
        --         --------------------------------------------------------------------------------
        --         validateFundsAddRedeemer :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateFundsAddRedeemer !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_Out !valueOf_CampaignDatum_Out = 
        --             ------------------
        --             -- it runs along with CampaignFunds ID Policy (PolicyRedeemerMintID)
        --             ------------------
        --             -- Que sea Protocol or Campaign Admin
        --             -- Que se mintee CampaignFunds ID con la correcta póliza indicada en CampaignDatum
        --             -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --             -- Que el CampaignDatum se actualiza con nuevo Campaign Funds
        --             -- Que el CampaignDatum value no cambie
        --             -- el datum Campaign Funds y la direccion a donde va estara controlado por la poliza Campaign Funds, que ya me aseguro que se ejecuta el controlar que se este minteando CampaignFunds ID
        --             -- no hay restricciones temporales
        --             -- que la poliza de Campaign Funds controle el mint de este NFT y controle el CampaignFundsDatum
        --             ------------------
        --                 validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                 traceIfFalse "not isCorrect_Output_CampaignDatum_With_CampaignFundsAdded" isCorrect_Output_CampaignDatum_With_CampaignFundsAdded
        --                 && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                 && traceIfFalse "not isMintingCampaignFundsID" isMintingCampaignFundsID
        --             where
        --                 ------------------
        --                 isCorrect_Output_CampaignDatum_With_CampaignFundsAdded :: Bool
        --                 !isCorrect_Output_CampaignDatum_With_CampaignFundsAdded =
        --                     let
        --                         !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_CampaignFundsAdded campaignDatum_In
        --                     in
        --                         campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                 ------------------
        --                 isMintingCampaignFundsID :: Bool
        --                 !isMintingCampaignFundsID =
        --                     let
        --                         !campaignFundsPolicyID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_In
        --                     in
        --                         OnChainHelpers.isNFT_Minting_With_CS campaignFundsPolicyID_CS info
        --         --------------------------------------------------------------------------------
        --         validateFundsManageRedeemers :: Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateFundsManageRedeemers !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_Out !valueOf_CampaignDatum_Out =
        --             case redeemer of
        --                 (T.ValidatorRedeemerFundsMerge (T.ValidatorRedeemerFundsMergeType quantity))   -> 
        --                     ------------------
        --                     -- Este redeemer va con Campaign Funds Validator redeemer ValidatorRedeemerMerge
        --                     ------------------
        --                     -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
        --                     -- 2 - it runs along Campaign Funds ID Policy (PolicyRedeemerBurnID)
        --                     -- 3 - it runs along Campaign Funds Validator, many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
        --                     ------------------
        --                     -- 1 - Que sea Protocol or Campaig Admin
        --                     -- 1 y 3 - Que se quemen CampaignFundsIDs con la correcta póliza indicada en CampaignDatum o CampaignFundDatum
        --                     -- 2 - Que se quemen CampaignFundsIDs con own póliza
        --                     -- 2 - Qque coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
        --                     -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de Campaign Funds Validator (en caso de Merge hay una input que no se quema)
        --                     -- 2 - que el redeemer de Campaign validator sea correcto
        --                     -- 2 - que el redeemer de todos los Campaign Funds Validator sea correcto
        --                     -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
        --                     -- 1 - Que el CampaignDatum se actualiza con el Campaign Funds eliminados
        --                     -- 1 - Que el CampaignDatum value no cambie
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
        --                     -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
        --                     -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
        --                     -- no hay restricciones temporales
        --                     ------------------
        --                         validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted" (isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted (quantity-1))
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isBurningCampaignFundsIDs" (isBurningCampaignFundsIDs (quantity-1))
        --                 ------------------
        --                 (T.ValidatorRedeemerFundsDelete (T.ValidatorRedeemerFundsDeleteType quantity)) -> 
        --                     ------------------
        --                     -- Este redeemer va con Campaign Funds Validator redeemer ValidatorRedeemerDelete
        --                     ------------------
        --                     -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
        --                     -- 2 - it runs along Campaign Funds ID Policy (PolicyRedeemerBurnID)
        --                     -- 3 - it runs along Campaign Funds Validator, many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
        --                     ------------------
        --                     -- 1 - Que sea Protocol or Campaig Admin
        --                     -- 1 y 3 - Que se quemen CampaignFundsIDs con la correcta póliza indicada en CampaignDatum o CampaignFundDatum
        --                     -- 2 - Que se quemen CampaignFundsIDs con own póliza
        --                     -- 2 - Qque coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
        --                     -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de Campaign Funds Validator (en caso de Merge hay una input que no se quema)
        --                     -- 2 - que el redeemer de Campaign validator sea correcto
        --                     -- 2 - que el redeemer de todos los Campaign Funds Validator sea correcto
        --                     -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
        --                     -- 1 - Que el CampaignDatum se actualiza con el Campaign Funds eliminados
        --                     -- 1 - Que el CampaignDatum value no cambie
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
        --                     -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
        --                     -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
        --                     -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
        --                     -- no hay restricciones temporales
        --                     ------------------
        --                         validateProtocolOrCampaignAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted" (isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted quantity)
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isBurningCampaignFundsIDs" (isBurningCampaignFundsIDs quantity)
        --                 ------------------
        --                 _                                                                              -> False
        --             where
        --                 isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted :: Integer -> Bool
        --                 isCorrect_Output_CampaignDatum_With_CampaignFundsDeleted quantity=
        --                     let
        --                         !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_CampaignFundsDeleted campaignDatum_In quantity
        --                     in
        --                         campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                 ------------------
        --                 isBurningCampaignFundsIDs :: Integer ->  Bool
        --                 isBurningCampaignFundsIDs quantity =
        --                     let
        --                         !campaignFundsPolicyID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_In
        --                     in
        --                         OnChainHelpers.isToken_Burning_With_CS_AndAmt campaignFundsPolicyID_CS quantity info
        --         ------------------------------------------------------------------------------
        --         validateFundsCollectRedeemers :: T.ValidatorRedeemerFundsCollectType -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateFundsCollectRedeemers (T.ValidatorRedeemerFundsCollectType !date !amount_ADA) !campaignDatum_Out !valueOf_CampaignDatum_Out = 
        --                 ---------------------
        --                 -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsCollect)
        --                 -- 2 - it runs along Campaign Funds Validator, one input, one output (ValidatorRedeemerCollect)
        --                 ------------------
        --                 -- 1 - Que sea Campaign Admin
        --                 -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
        --                 -- 1 - Que el CampaignDatum este en estado CsReached
        --                 -- 1 - Que el CampaignDatum se actualiza con el pago a los creadores de la campaña
        --                 -- 1 - Que el CampaignDatum value no cambie
        --                 -- 1 - que el redeemer de todos los Campaign Funds Validator sea correcto, mismo redeemer type, misma date, mismo amount.
        --                 -- 1 - Que el monto no supera lo disponible en ese momento: el pago total acumulado hasta ahora menos lo cobrado.
        --                 -- 1 - El pago acumulado hasta ahora es de acuerdo al ultimo milestone aprobado. Siempre que el ultimo milestone estuvira en Creado y no en Fallado.
        --                 -- 1 - Cada milestone establece un porcentaje del total. Tengo el campo de total vendido ADA, y de ahi calculo los porcentajes.
        --                 -- 2 - que el redeemer de Campaign validator sea correcto
        --                 -- 2 - Que todos los CampaignFundsDatum resultantes regrese a Campaign Funds Validator (se hace automaticamente al buscar outputs en same address)
        --                 -- 2 - Que los CampaignFundsDatum datums se actualicen correctamente: solo el campo collected debe modificarse, sumando algun valor.
        --                 -- 2 - Que la suma de todo lo que se saca de cada uno, coincida con amount
        --                 -- 2 - Que los CampaignFundsDatum tengan values actualizados: con menos ADA. Cada uno coincidiendo con el valor de collected del datum
        --                 ------------------
        --                 -- no hay restricciones temporales
        --                 ------------------
        --                 validateCampaignAdminAction &&
        --                 traceIfFalse "not isCampaignClosed" isCampaignClosed &&
        --                 traceIfFalse "not isCampaignStatus CsReached" (isCampaignStatus T.CsReached) &&
        --                 traceIfFalse "not isDateInRange" (OnChainHelpers.isDateInRange date info) &&
        --                 traceIfFalse "not amount_ADA > 0" (amount_ADA > 0) &&
        --                 traceIfFalse "not avalaible_ADA_to_Collect >= amount_ADA" (avalaible_ADA_to_Collect >= amount_ADA) &&
        --                 traceIfFalse "not isCorrectRedeemersCampaignFundsDatum" isCorrectRedeemersCampaignFundsDatum &&
        --                 traceIfFalse "not isCorrect_Output_CampaignDatum_Updated_With_Collect" isCorrect_Output_CampaignDatum_Updated_With_Collect &&
        --                 traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --             where
        --                 ------------------
        --                 avalaible_ADA_to_Collect :: Integer
        --                 !avalaible_ADA_to_Collect =
        --                     let
        --                         ----------------------
        --                         !fundedADA = T.cdFundedADA campaignDatum_In
        --                         !collectedADA = T.cdCollectedADA campaignDatum_In
        --                         !milestones = T.cdMilestones campaignDatum_In
        --                         ----------------------
        --                         -- Helper function to find the current milestone index
        --                         findCurrentMilestoneIndex :: [T.CampaignMilestones] -> Integer
        --                         findCurrentMilestoneIndex ms =
        --                             let
        --                                 -- Find the index of the first milestone that is not `MsSuccess`
        --                                 searchIndex idx [] = idx  -- No more milestones, return the index
        --                                 searchIndex idx (m:_) =
        --                                     if T.cmStatus m /= T.MsSuccess then idx
        --                                     else searchIndex (idx + 1) ms
        --                             in
        --                                 searchIndex 0 ms
        --                         ----------------------
        --                         -- Calculate available ADA based on milestone status
        --                         !currentMilestoneIndex = findCurrentMilestoneIndex milestones
        --                         ----------------------
        --                         -- Calculate cumulative percentage up to the current milestone
        --                         cumulativePercentage :: Integer -> Integer
        --                         cumulativePercentage milestoneIndex =
        --                             let
        --                                 getPercentage idx =
        --                                     let
        --                                         indices = OnChainHelpers.enumFromTo 0 idx
        --                                     in
        --                                         sum [ T.cmPerncentage (milestones !! i) | i <- indices ]
        --                             in
        --                                 if milestoneIndex < length milestones - 1 then getPercentage milestoneIndex
        --                                 else 100  -- The last milestone should cover the remaining amount
        --                         ----------------------
        --                         !availableForCurrentMilestone =
        --                             if currentMilestoneIndex < length milestones then
        --                                 let milestone = milestones !! currentMilestoneIndex
        --                                 in case T.cmStatus milestone of
        --                                     T.MsCreated ->
        --                                         let percentage = cumulativePercentage currentMilestoneIndex
        --                                         in percentage * fundedADA `divide` 100
        --                                     _ -> 0
        --                             else 0
        --                     in availableForCurrentMilestone - collectedADA
        --                 ------------------
        --                 isCorrectValidatorRedeemerCollectCampaignFundsDatum :: (LedgerApiV2.TxOutRef, LedgerApiV2.TxOut, CampaignFundsT.CampaignFundsDatumType) -> Bool
        --                 isCorrectValidatorRedeemerCollectCampaignFundsDatum input_TxOutRef_TxOut_And_CampaignFundsDatum  =
        --                     let
        --                         !redeemerFor_CampaignFundsDatum = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_CampaignFundsDatum) info
        --                     in
        --                         case redeemerFor_CampaignFundsDatum of
        --                             Nothing -> traceError "Expected CampaignFunds inputs with redeemers"
        --                             Just redeemerFor_CampaignFundsDatum' ->
        --                                 case LedgerApiV2.fromBuiltinData @CampaignFundsT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_CampaignFundsDatum' of
        --                                     Just (CampaignFundsT.ValidatorRedeemerCollect (CampaignFundsT.ValidatorRedeemerCollectType date' amount_ADA')) ->
        --                                         date == date' && amount_ADA == amount_ADA'
        --                                     _      -> traceError "Expected CampaignFunds input with valid redeemer ValidatorRedeemerCollect"
        --                 ------------------
        --                 isCorrectRedeemersCampaignFundsDatum :: Bool
        --                 !isCorrectRedeemersCampaignFundsDatum =
        --                     let
        --                         !campaignFundsPolicyID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_In
        --                         ------------------
        --                         !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info]
        --                         ------------------
        --                         !inputs_TxOutRefs_TxOuts_And_CampaignFundsDatums =
        --                             case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
        --                                 @CampaignFundsT.ValidatorDatum
        --                                 @CampaignFundsT.CampaignFundsDatumType
        --                                 ctx
        --                                 inputs_TxOutRefs_TxOuts
        --                                 campaignFundsPolicyID_CS
        --                                 CampaignFundsT.getCampaignFundsDatumType of
        --                                 [] -> traceError "Expected at least one CampaignFunds input"
        --                                 x  -> x
        --                     in
        --                         all isCorrectValidatorRedeemerCollectCampaignFundsDatum inputs_TxOutRefs_TxOuts_And_CampaignFundsDatums
        --                 ------------------
        --                 isCorrect_Output_CampaignDatum_Updated_With_Collect :: Bool
        --                 !isCorrect_Output_CampaignDatum_Updated_With_Collect =
        --                     let
        --                         !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_CampaignFundsCollected campaignDatum_In amount_ADA
        --                     in
        --                         campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --         --------------------------------------------------------------------------------
        --         validateUpdateStatusCampaignRedeemers :: [LedgerContextsV2.TxOut] -> Maybe (LedgerContextsV2.TxOut, ProtocolT.ProtocolDatumType) -> T.CampaignDatumType -> Ledger.Value -> Bool
        --         validateUpdateStatusCampaignRedeemers !inputsRef_TxOuts !inputRef_TxOut_And_ProtocolDatum' !campaignDatum_Out !valueOf_CampaignDatum_Out = 
        --             case redeemer of
        --                 (T.ValidatorRedeemerInitializeCampaign _) ->
        --                     ---------------------
        --                     -- Inicializa la campaña, una vez que tiene los fondos a la venta necesarios
        --                     -- Deben haber sido minteados antes y agregados a las UTXO de fondos
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- Que sea Protocol Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- Que el CampaignDatum value no cambie
        --                     -- tiene que estar en estado created
        --                     -- tiene que tener en los CampaignFunds la cantidad de tokens necesarias para poner a la venta (el max requested)
        --                     ---------------------
        --                         validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" isCorrect_Output_CampaignDatum_Updated
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isCampaignStatusCreated" (isCampaignStatus T.CsCreated)
        --                         && traceIfFalse "not isAllCampaignFunds" isAllCampaignFunds
        --                         && traceIfFalse "not isCampaignFundsTokensForSaleReady" isCampaignFundsTokensForSaleReady
        --                     where
        --                         ---------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Bool
        --                         !isCorrect_Output_CampaignDatum_Updated =
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_NewStatus campaignDatum_In T.CsInitialized
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                         ---------------------
        --                         isCampaignFundsTokensForSaleReady :: Bool
        --                         !isCampaignFundsTokensForSaleReady =
        --                             let
        --                                 requestedMaxADA = T.cdRequestedMaxADA campaignDatum_In
        --                                 requestedTokensToSell = requestedMaxADA `divide` T.cdCampaignFT_PriceADA campaignDatum_In
        --                                 tokensAvalaibles = sum (CampaignFundsT.cfdSubtotal_Avalaible_FT . OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_CampaignFundsDatums)
        --                             in
        --                             requestedTokensToSell == tokensAvalaibles
        --                 ------------------
        --                 (T.ValidatorRedeemerReachedCampaign _) ->
        --                     ---------------------
        --                     -- Cambia el estado de campaña a alcanzada
        --                     -- Deben haber sido vendidos la cantidad minima de tokens por lo menos
        --                     -- Debe estar en estado inicalizada
        --                     -- Debe estar en fecha de deadline pasado
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- Que sea Protocol Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- Que el CampaignDatum value no cambie
        --                     -- Que se actualice la cantidad de ADA recivida (fundedADA) y el nuevo estado cSReached
        --                     -- tiene que estar en estado CsInitialized
        --                     -- tiene que tener en los CampaignFunds la cantidad de tokens necesarias vendidos, superando el minimo esperado
        --                     -- que haya terminado la campaña
        --                     ---------------------
        --                         validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" isCorrect_Output_CampaignDatum_Updated
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isCampaignStatusInitialized" (isCampaignStatus T.CsInitialized)
        --                         && traceIfFalse "not isAllCampaignFunds" isAllCampaignFunds
        --                         && traceIfFalse "not isCampaignFundsTokensSold" isCampaignFundsTokensSold
        --                         && traceIfFalse "not isCampaignFinish" isCampaignFinish
        --                     where
        --                         ---------------------
        --                         !tokensToSold = sum (CampaignFundsT.cfdSubtotal_Sold_FT . OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_CampaignFundsDatums)
        --                         ---------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Bool
        --                         !isCorrect_Output_CampaignDatum_Updated =
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_NewStatusReached campaignDatum_In tokensToSold
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                         ---------------------
        --                         isCampaignFundsTokensSold :: Bool
        --                         !isCampaignFundsTokensSold =
        --                             let
        --                                 !requestedMinADA = T.cdRequestedMinADA campaignDatum_In
        --                             in
        --                                 requestedMinADA <= tokensToSold * T.cdCampaignFT_PriceADA campaignDatum_In
        --                 ------------------
        --                 (T.ValidatorRedeemerNotReachedCampaign _) ->
        --                     ---------------------
        --                     -- Cambia el estado de campaña a no alcanzada
        --                     -- Deben haber sido vendidos menos de la cantidad minima de tokens
        --                     -- Debe estar en estado inicalizada
        --                     -- Debe estar en fecha de deadline pasado
        --                     ---------------------
        --                     -- it runs alone
        --                     ---------------------
        --                     -- Que sea Protocol Admin
        --                     -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
        --                     -- Que el CampaignDatum value no cambie
        --                     -- Que se actualice la cantidad de ADA recivida (fundedADA) y el nuevo estado cSNotReached
        --                     -- tiene que estar en estado CsInitialized
        --                     -- tiene que no tener en los CampaignFunds la cantidad de tokens minimos
        --                     -- la fecha tiene que haber pasado al deadline
        --                     ---------------------
        --                         validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&validateProtocolAdminAction inputRef_TxOut_And_ProtocolDatum' &&
        --                         traceIfFalse "not isCorrect_Output_CampaignDatum_Updated" isCorrect_Output_CampaignDatum_Updated
        --                         && traceIfFalse "not isCorrect_Output_CampaignDatum_Value_NotChanged" (isCorrect_Output_CampaignDatum_Value_NotChanged valueOf_CampaignDatum_Out)
        --                         && traceIfFalse "not isCampaignStatusInitialized" (isCampaignStatus T.CsInitialized)
        --                         && traceIfFalse "not isAllCampaignFunds" isAllCampaignFunds
        --                         && traceIfFalse "not isCampaignFundsTokensNotSold" isCampaignFundsTokensNotSold
        --                         && traceIfFalse "not isCampaignFinish" isCampaignFinish
        --                     where
        --                         ---------------------
        --                         !tokensToSold = sum (CampaignFundsT.cfdSubtotal_Sold_FT . OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_CampaignFundsDatums)
        --                         ---------------------
        --                         isCorrect_Output_CampaignDatum_Updated :: Bool
        --                         !isCorrect_Output_CampaignDatum_Updated =
        --                             let
        --                                 !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignDatum_With_NewStatusNotReached campaignDatum_In tokensToSold
        --                             in
        --                                 campaignDatum_Out `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
        --                         ---------------------
        --                         isCampaignFundsTokensNotSold :: Bool
        --                         !isCampaignFundsTokensNotSold =
        --                             let
        --                                 !requestedMinADA = T.cdRequestedMinADA campaignDatum_In
        --                             in
        --                                 requestedMinADA > tokensToSold * T.cdCampaignFT_PriceADA campaignDatum_In
        --                 ------------------
        --                 _ -> False
        --                 ------------------
        --             where
        --                 ------------------
        --                 !campaignFundsID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_In
        --                 ------------------
        --                 !inputsRef_TxOuts_And_CampaignFundsDatums =
        --                     case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
        --                         @CampaignFundsT.ValidatorDatum
        --                         @CampaignFundsT.CampaignFundsDatumType
        --                         ctx
        --                         inputsRef_TxOuts
        --                         campaignFundsID_CS
        --                         CampaignFundsT.getCampaignFundsDatumType of
        --                             [] -> traceError "Expected all CampaignFunds as inputRef"
        --                             x  -> x
        --                 ------------------
        --                 !isAllCampaignFunds = length inputsRef_TxOuts_And_CampaignFundsDatums == T.cdFundsCount campaignDatum_In
        --                 ------------------
        --                 !isCampaignFinish = OnChainHelpers.isDateReached (T.cdDeadline campaignDatum_In) info

--------------------------------------------------------------------------------


{-# INLINEABLE mkPolicy #-}
mkPolicy :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicy (T.PolicyParams !protocolPolicyID_CS !campaignPolicy_TxOutRef !campaignValidator_Hash) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && validate
        then ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !campaignPolicy_CS = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        !ownMintingValue = OnChainHelpers.getUnsafeOwnMintingValue ctx
        ------------------
        validate :: Bool
        !validate = case redeemer of
            (T.PolicyRedeemerMintID _) -> validateMintAndBurnIDRedeemers redeemer campaignPolicy_CS campaignPolicy_TxOutRef campaignValidator_Hash ownMintingValue info ctx
            (T.PolicyRedeemerBurnID _) -> validateMintAndBurnIDRedeemers redeemer campaignPolicy_CS campaignPolicy_TxOutRef campaignValidator_Hash ownMintingValue info ctx
            (T.PolicyRedeemerMintFT _) -> validateMintAndBurnFTRedeemers redeemer campaignPolicy_CS ownMintingValue info ctx
            (T.PolicyRedeemerBurnFT _) -> validateMintAndBurnFTRedeemers redeemer campaignPolicy_CS ownMintingValue info ctx

{-# INLINEABLE validateMintAndBurnIDRedeemers #-}
validateMintAndBurnIDRedeemers :: T.PolicyRedeemer -> LedgerApiV2.CurrencySymbol-> LedgerApiV2.TxOutRef -> LedgerApiV2.ValidatorHash ->  Ledger.Value -> LedgerContextsV2.TxInfo -> LedgerContextsV2.ScriptContext -> Bool
validateMintAndBurnIDRedeemers redeemer campaignPolicy_CS campaignPolicy_TxOutRef campaignValidator_Hash ownMintingValue info ctx =
    case redeemer of
        (T.PolicyRedeemerMintID _) ->
            ---------------------
            -- it runs alone
            ---------------------
            -- Que venga ProtocolDatum como ref
            -- Para identificar el correcto ProtocolDatum necesita la póliza Protocol ID que está en los parámetros de esta póliza.
            -- Que se genere salida out 0 con nuevo CampaignDatum en direcion correcta
            -- Para identificar la direccion, tengo el validator hash en los parametros de poliza
            -- Que el CampaignDatum sea correcto según límites y valores del ProtocolDatum
            -- Que se mintee Campaign ID con own póliza
            -- solo se puede ejecutar una vez por que esta parametrizado con campaignPolicy_TxOutRef
            -- Que el CampaignDatum tenga el Campaign ID
            ---------------------
            traceIfFalse "not isTxOutAnInput" (OnChainHelpers.isTxOutAnInput campaignPolicy_TxOutRef info)
                && traceIfFalse "not isMintingID" isMintingID
                && traceIfFalse "not isCorrect_Output_CampaignDatum" isCorrect_Output_CampaignDatum
                && traceIfFalse "not isCorrect_Output_CampaignDatum_Value" isCorrect_Output_CampaignDatum_Value
            where
                ---------------------
                -- !inputRef_TxOut_And_ProtocolDatum =
                --     case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                --         @ProtocolT.ValidatorDatum
                --         @ProtocolT.ProtocolDatumType
                --         ctx
                --         inputsRef_TxOuts
                --         protocolID_AC
                --         ProtocolT.getProtocolDatumType of
                --         [x] -> x
                --         _   -> traceError "Expected exactly one Protocol input ref"
                -- ------------------
                -- !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                ------------------
                !outputs_txOuts =
                    [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                    ]
                -- 0 out is the CampaignDatum
                ------------------
                !_ = if null outputs_txOuts
                    then traceError "Expected at least one output to scripts addresses"
                    else ()
                ------------------
                !output_Own_TxOut_And_CampaignDatum =
                    fromMaybe
                        (traceError "Expected Campaign at output index 0")
                        ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                            @T.ValidatorDatum
                            @T.CampaignDatumType
                            ctx
                            (head outputs_txOuts)
                            campaignID_AC
                            (Just campaignValidator_Address)
                            T.getCampaignDatumType
                        )
                ------------------
                !campaignDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_CampaignDatum
                ------------------
                !campaignFundsPolicyID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_Out
                ------------------
                !valueFor_Mint_CampaignID = LedgerValue.assetClassValue campaignID_AC 1
                ---------------------
                !minADA_For_CampaignDatum = T.cdMinADA campaignDatum_Out
                !value_MinADA_For_CampaignDatum = LedgerAda.lovelaceValueOf minADA_For_CampaignDatum
                !valueFor_CampaignDatum_Out_Control = valueFor_Mint_CampaignID <> value_MinADA_For_CampaignDatum
                ---------------------
                !admins = T.cdAdmins campaignDatum_Out
                !tokenAdminPolicy_CS = T.cdTokenAdminPolicy_CS campaignDatum_Out
                !mint_CampaignFT = T.cdMint_CampaignFT campaignDatum_Out
                !campaignFT_CS  = T.cdCampaignFT_CS campaignDatum_Out
                !campaignFT_TN = T.cdCampaignFT_TN campaignDatum_Out
                !campaignFT_PriceADA = T.cdCampaignFT_PriceADA campaignDatum_Out
                !requestedMaxADA = T.cdRequestedMaxADA campaignDatum_Out
                !requestedMinADA = T.cdRequestedMinADA campaignDatum_Out
                !fundedADA = 0
                !collectedADA = 0
                !beginAt = T.cdBeginAt campaignDatum_Out
                !deadline = T.cdDeadline campaignDatum_Out
                !status = T.CsCreated
                !milestones= T.cdMilestones campaignDatum_Out
                !fundsCount = 0
                !fundsIndex = 0
                ---------------------
                !campaignDatum_Out_Control =
                    T.mkCampaignDatumType
                        campaignPolicy_CS
                        campaignFundsPolicyID_CS
                        admins
                        tokenAdminPolicy_CS
                        mint_CampaignFT
                        campaignFT_CS
                        campaignFT_TN
                        campaignFT_PriceADA
                        requestedMaxADA
                        requestedMinADA
                        fundedADA
                        collectedADA
                        beginAt
                        deadline
                        status
                        milestones
                        fundsCount
                        fundsIndex
                        minADA_For_CampaignDatum
                ---------------------
                isMintingID :: Bool
                !isMintingID = ownMintingValue `OnChainHelpers.isEqValue` valueFor_Mint_CampaignID
                -----------------
                isCorrect_Output_CampaignDatum :: Bool
                !isCorrect_Output_CampaignDatum =
                    let
                        -----------------
                        -- Check if campaignFT_CS equals campaignPolicy_CS when minting tokens
                        validCampaignFT_CS = not mint_CampaignFT || (campaignFT_CS == campaignPolicy_CS)
                        -- Check if campaignFT_TN is not empty
                        campaignFT_TN_BS = LedgerApiV2.unTokenName campaignFT_TN
                        validCampaignFT_TN = lengthOfByteString campaignFT_TN_BS /= 0
                        -- Check if campaignFT_PriceADA is greater than zero
                        validCampaignFT_PriceADA = campaignFT_PriceADA > 0
                        -- Check if requestedMaxADA is greater than requestedMinADA, and requestedMinADA is greater than zero
                        validRequestedMinMaxADA = requestedMinADA > 0 && requestedMaxADA > requestedMinADA
                        -- Check if requestedMaxADA and requestedMinADA are divisible by campaignFT_PriceADA
                        divisibleMax = requestedMaxADA `modulo` campaignFT_PriceADA == 0
                        divisibleMin = requestedMinADA `modulo` campaignFT_PriceADA == 0
                        validRequestedPriceDivisibility = divisibleMax && divisibleMin
                        -- Check valid milestones
                        validateMilestones =
                            let
                                -- Check if there is at least one milestone
                                hasMilestones = not (null milestones)
                               -- Check if all milestones have status MsCreated
                                allCreatedMilestones = all (\m -> T.cmStatus m == T.MsCreated) milestones
                                -- Check if the total percentage of all milestones sums to 100
                                totalPercentage = sum [ T.cmPerncentage m | m <- milestones ]
                                isValidPercentage = totalPercentage == 100
                            in hasMilestones && allCreatedMilestones && isValidPercentage
                        -----------------
                    in campaignDatum_Out
                        `OnChainHelpers.isUnsafeEqDatumsCostly` campaignDatum_Out_Control
                        && traceIfFalse "not isDateReached deadline" (OnChainHelpers.isDateReached deadline info)
                        && traceIfFalse "not deadline > beginAt" (deadline > beginAt)
                        && traceIfFalse "not validateMilestones" validateMilestones
                        && traceIfFalse "not validCampaignFT_CS" validCampaignFT_CS
                        && traceIfFalse "not validCampaignFT_TN" validCampaignFT_TN
                        && traceIfFalse "not validCampaignFT_PriceADA" validCampaignFT_PriceADA
                        && traceIfFalse "not validRequestedMinMaxADA" validRequestedMinMaxADA
                        && traceIfFalse "not validRequestedPriceDivisibility" validRequestedPriceDivisibility
                ------------------
                isCorrect_Output_CampaignDatum_Value :: Bool
                !isCorrect_Output_CampaignDatum_Value =
                    let
                        !valueOf_CampaignDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignDatum
                    in
                        valueOf_CampaignDatum_Out
                            `OnChainHelpers.isEqValue` valueFor_CampaignDatum_Out_Control
                -----------------
        (T.PolicyRedeemerBurnID _) ->
            ------------------
            -- it runs along with Campaign Validator (ValidatorRedeemerDelete)
            ------------------
            -- Que se queme Campaign ID con own póliza
            -- No hay control del redeemer del Campaign Validator, por que se sobre entiende que de algun lado salen estos tokens
            -- Y solo se pueden sacar consumiendo el Campaign Datum y para eso solo el validador va a permitir con el redeemer correcto
            ---------------------
            traceIfFalse "not isBurningID" isBurningID
            where
                ------------------
                !valueFor_Burn_CampaignID = LedgerValue.assetClassValue campaignID_AC (negate 1)
                ---------------------
                isBurningID :: Bool
                !isBurningID = ownMintingValue `OnChainHelpers.isEqValue` valueFor_Burn_CampaignID
        -----------------
        _ -> False
    where
        ------------------
        -- !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        ------------------
        !campaignValidator_Address = Ledger.scriptHashAddress campaignValidator_Hash

--------------------------------------------------------------------------------2

{-# INLINEABLE validateMintAndBurnFTRedeemers #-}
validateMintAndBurnFTRedeemers :: T.PolicyRedeemer -> LedgerApiV2.CurrencySymbol-> Ledger.Value -> LedgerContextsV2.TxInfo -> LedgerContextsV2.ScriptContext -> Bool
validateMintAndBurnFTRedeemers redeemer campaignPolicy_CS ownMintingValue info ctx =
    case redeemer of
        (T.PolicyRedeemerMintFT _) ->
            ------------------
            -- it runs along with CampaignFunds Validator (ValidatorRedeemerDeposit)
            ------------------
            -- que este la campaign status sea CsCreated
            -- que la campaign este configurada para mintear los tokens que se venden
            -- que el redeemer del campaign funds sea ValidatorRedeemerDeposit
            -- que se este minetando, solamente, con AC correcta
            -- que el valor a mintear sea al determiando en el redeemer del campaign funds (lo reviso en el validar del campaign funds)
            ------------------
            traceIfFalse "not isCampaignCreated" isCampaignCreated
                && traceIfFalse "not isCampaignWithMintingFT" isCampaignWithMintingFT
                && traceIfFalse "not isCampaignFundsValidatorRedeemerDeposit" isCampaignFundsValidatorRedeemerDeposit
                && traceIfFalse "not isMintingFT" isMintingFT
            where
                ------------------
                isCampaignFundsValidatorRedeemerDeposit :: Bool
                isCampaignFundsValidatorRedeemerDeposit = case redeemerFor_CampaignFundsDatum of
                    CampaignFundsT.ValidatorRedeemerDeposit _ -> True
                    _                                         -> False
                ------------------
                !amount = case redeemerFor_CampaignFundsDatum of
                    CampaignFundsT.ValidatorRedeemerDeposit (CampaignFundsT.ValidatorRedeemerDepositType _ amt )-> amt
                    _                                                                                            -> -1
                -----------------
                !valueFor_Mint_CampaignFT = LedgerValue.assetClassValue campaignFT_AC amount
                ---------------------
                isMintingFT :: Bool
                !isMintingFT =  amount >=1 && ownMintingValue `OnChainHelpers.isEqValue` valueFor_Mint_CampaignFT
                ------------------
        -----------------
        (T.PolicyRedeemerBurnFT _) ->
            ------------------
            -- it runs along with Campaign Funds Validator (isCampaignFundsValidatorRedeemerWithdraw)
            ------------------
            -- que este la campaign status sea CsCreated, CsNotReached or CsFailedMilestone
            -- que la campaign este configurada para mintear los tokens que se venden
            -- que el redeemer del campaign funds sea Withdraw
            -- que el valor a quemar sea al determiando en el redeemer del campaign funds (lo reviso en el validar del campaign funds)
            ------------------
            traceIfFalse "not isCampaignCreated_NotReached_Or_FailedMilestone" isCampaignCreated_NotReached_Or_FailedMilestone
                && traceIfFalse "not isCampaignWithMintingFT" isCampaignWithMintingFT
                && traceIfFalse "not isCampaignFundsValidatorRedeemerWithdraw" isCampaignFundsValidatorRedeemerWithdraw
                && traceIfFalse "not isBurningFT" isBurningFT
            where
                ------------------
                isCampaignCreated_NotReached_Or_FailedMilestone :: Bool
                !isCampaignCreated_NotReached_Or_FailedMilestone =
                    isCampaignCreated || T.cdStatus campaignDatum_In == T.CsNotReached || T.cdStatus campaignDatum_In == T.CsFailedMilestone
                ------------------
                isCampaignFundsValidatorRedeemerWithdraw :: Bool
                isCampaignFundsValidatorRedeemerWithdraw = case redeemerFor_CampaignFundsDatum of
                    CampaignFundsT.ValidatorRedeemerWithdraw _ -> True
                    _                                          -> False
                ------------------
                !amount = case redeemerFor_CampaignFundsDatum of
                    CampaignFundsT.ValidatorRedeemerWithdraw (CampaignFundsT.ValidatorRedeemerWithdrawType _ amt )-> amt
                    _                                                                                              -> 1
                -----------------
                !valueFor_Burn_CampaignFT = LedgerValue.assetClassValue campaignFT_AC (negate amount)
                ---------------------
                isBurningFT :: Bool
                !isBurningFT =  amount < 0 && ownMintingValue `OnChainHelpers.isEqValue` valueFor_Burn_CampaignFT
                ------------------
        _ -> False
    where
        ------------------
        !inputsRef_TxOuts =
            [ LedgerApiV2.txInInfoResolved txInfoInput | txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
            ]
        ------------------
        !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        ------------------
        !input_TxOut_And_CampaignDatum =
            case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                @T.ValidatorDatum
                @T.CampaignDatumType
                ctx
                inputsRef_TxOuts
                campaignID_AC
                T.getCampaignDatumType of
                [x] -> x
                _   -> traceError "Expected exactly one Campaign input ref"
        ------------------
        !campaignDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_CampaignDatum
        ------------------
        !campaignFT_TN = T.cdCampaignFT_TN campaignDatum_In
        !campaignFT_AC = LedgerValue.AssetClass (campaignPolicy_CS, campaignFT_TN)
        ------------------
        isCampaignCreated :: Bool
        !isCampaignCreated = T.cdStatus campaignDatum_In == T.CsCreated
        ------------------
        isCampaignWithMintingFT :: Bool
        !isCampaignWithMintingFT = T.cdMint_CampaignFT campaignDatum_In
        ------------------
        redeemerFor_CampaignFundsDatum :: CampaignFundsT.ValidatorRedeemer
        !redeemerFor_CampaignFundsDatum =
            let
                !campaignFundsPolicyID_CS = T.cdCampaignFundsPolicyID_CS campaignDatum_In
                ------------------
                !inputs_TxOutRefs_TxOuts = [(LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info]
                ------------------
                !input_TxOutRef_TxOut_And_CampaignFundsDatum =
                    case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                        @CampaignFundsT.ValidatorDatum
                        @CampaignFundsT.CampaignFundsDatumType
                        ctx
                        inputs_TxOutRefs_TxOuts
                        campaignFundsPolicyID_CS
                        CampaignFundsT.getCampaignFundsDatumType of
                        [x] -> x
                        _   -> traceError "Expected exactly one CampaignFunds input"
                ------------------
                !redeemerFor_CampaignFundsDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_CampaignFundsDatum) info
            in
                case redeemerFor_CampaignFundsDatum' of
                    Nothing -> traceError "Expected exactly one CampaignFunds input with redeemer"
                    Just redeemerFor_CampaignFundsDatum'' ->
                        case LedgerApiV2.fromBuiltinData @CampaignFundsT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_CampaignFundsDatum'' of
                            Just x -> x
                            _      -> traceError "Expected exactly one CampaignFunds input with valid redeemer"


{-# INLINEABLE policy #-}
policy :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policy params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicy||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID protocolPolicyID_CS fundPolicy_TxHash fundPolicy_TxOutputIndex campaignValidator_Hash = mkPolicy params
    where
        tid = PlutusTx.unsafeFromBuiltinData fundPolicy_TxHash :: BuiltinByteString
        txout =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid
                , LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData fundPolicy_TxOutputIndex
                }
        params =
            T.PolicyParams
                { ppProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , ppCampaignPolicy_TxOutRef = txout
                , ppCampaignValidator_Hash = PlutusTx.unsafeFromBuiltinData campaignValidator_Hash
                }

{-# INLINEABLE policyCode #-}
policyCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicyID||])

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

{-# INLINEABLE validatorCode #-}
validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])
-- validatorCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedValidator||])

------------------------------------------------------------------------------2
