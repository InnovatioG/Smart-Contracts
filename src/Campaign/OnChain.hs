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
import qualified Helpers.OnChain           as OnChainHelpers
import           Prelude                   (Show)
import qualified Protocol.Types            as ProtocolT
import qualified Types                     as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------
-- Validator Types and Context
--------------------------------------------------------------------------------

data ValidationParams
    = ValidationParams
          { vParams        :: !T.ValidatorParams
          , vCtx           :: !LedgerContextsV2.ScriptContext
          , vInfo          :: !LedgerContextsV2.TxInfo
          , vRedeemer      :: !T.ValidatorRedeemer
          , vDatum         :: !T.CampaignDatumType
          , vOwnInput      :: !LedgerContextsV2.TxOut
          , vOwnAddress    :: !LedgerApiV2.Address
          , vInputValue    :: !Ledger.Value
          , vCampaignID_AC :: !LedgerValue.AssetClass
          , vProtocolID_AC :: !LedgerValue.AssetClass
          }
    deriving (Show)

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator !params !datumRaw !redRaw !ctxRaw =
    let !vp = mkValidationParams params datumRaw redRaw ctxRaw
    in if traceIfFalse "not unique" (checkUnique vp)
          && validate vp
       then ()
       else error ()

{-# INLINEABLE mkValidationParams #-}
mkValidationParams :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ValidationParams
mkValidationParams !params !datumRaw !redRaw !ctxRaw =
    let !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !datum = T.getCampaign_DatumType $ LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !ownInput = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !ownAddress = LedgerApiV2.txOutAddress ownInput
        !campaignPolicy_CS = T.cdCampaignPolicy_CS datum
        !protocolPolicyID = T.vpProtocolPolicyID_CS params
    in ValidationParams
        { vParams = params
        , vCtx = ctx
        , vInfo = info
        , vRedeemer = redeemer
        , vDatum = datum
        , vOwnInput = ownInput
        , vOwnAddress = ownAddress
        , vInputValue = LedgerApiV2.txOutValue ownInput
        , vCampaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        , vProtocolID_AC = LedgerValue.AssetClass (protocolPolicyID, T.protocolID_TN)
        }

--------------------------------------------------------------------------------
-- Primary Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE checkUnique #-}
checkUnique :: ValidationParams -> Bool
checkUnique !vp = T.vpProtocolPolicyID_CS (vParams vp) /= LedgerApiV2.adaSymbol

{-# INLINEABLE validate #-}
validate :: ValidationParams -> Bool
validate !vp = case vRedeemer vp of
    T.ValidatorRedeemerEmergency _ -> validateEmergency vp
    _                              -> validateNonEmergency vp

{-# INLINEABLE validateEmergency #-}
validateEmergency :: ValidationParams -> Bool
validateEmergency !vp =
    let !tokenEmergencyAdmin_AC = LedgerValue.AssetClass (T.vpTokenEmergencyAdminPolicy_CS $ vParams vp, T.tokenEmergencyAdmin_TN)
        !outputs = LedgerApiV2.txInfoOutputs $ vInfo vp
    in traceIfFalse "not isEmergencyAdminTokenPresent" $
       not (null outputs) && OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head outputs) tokenEmergencyAdmin_AC

{-# INLINEABLE validateNonEmergency #-}
validateNonEmergency :: ValidationParams -> Bool
validateNonEmergency !vp =
    traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange (vInfo vp) T.validTxTimeRange)
        && validateByInputOutput vp

--------------------------------------------------------------------------------
-- Input/Output Processing
--------------------------------------------------------------------------------

{-# INLINEABLE validateByInputOutput #-}
validateByInputOutput :: ValidationParams -> Bool
validateByInputOutput !vp =
    let !ownInputs = getOwnInputs vp
        !ownOutputs = getOwnOutputs vp
        !hasOneInput = length ownInputs == 1
        !hasNoOutputs = null ownOutputs
        !hasOneOutput = length ownOutputs == 1
    in if hasOneInput && hasNoOutputs
        then validateSingleInputNoOutput vp
        else if hasOneInput && hasOneOutput
            then validateSingleInputSingleOutput vp
            else traceError "Invalid input/output combination"

{-# INLINEABLE validateSingleInputNoOutput #-}
validateSingleInputNoOutput :: ValidationParams -> Bool
validateSingleInputNoOutput !vp =
    case vRedeemer vp of
        T.ValidatorRedeemerDelete _ -> validateDelete vp
        _                           -> False

{-# INLINEABLE validateSingleInputSingleOutput #-}
validateSingleInputSingleOutput :: ValidationParams -> Bool
validateSingleInputSingleOutput !vp =
    case getOutput_Campaign_DatumAndValue vp of
        Nothing                     -> traceError "Expected Campaign at output"
        Just (!outDatum, !outValue) -> validateRedeemer vp outDatum outValue

{-# INLINEABLE validateRedeemer #-}
validateRedeemer :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateRedeemer !vp !outDatum !outValue = case vRedeemer vp of
    T.ValidatorRedeemerDatumUpdate _          -> validateUpdate vp outDatum outValue
    T.ValidatorRedeemerUpdateMinADA _         -> validateMinADAUpdate vp outDatum outValue
    T.ValidatorRedeemerFundsAdd _             -> validateFundsAdd vp outDatum outValue
    T.ValidatorRedeemerFundsMerge _           -> validateFundsMergeAndDelete vp outDatum outValue
    T.ValidatorRedeemerFundsDelete _          -> validateFundsMergeAndDelete vp outDatum outValue
    T.ValidatorRedeemerInitializeCampaign _   -> validateStatusUpdate vp outDatum outValue
    T.ValidatorRedeemerReachedCampaign _      -> validateStatusUpdate vp outDatum outValue
    T.ValidatorRedeemerNotReachedCampaign _   -> validateStatusUpdate vp outDatum outValue
    r@(T.ValidatorRedeemerMilestoneAprobe _)  -> validateMilestone vp outDatum outValue r
    r@(T.ValidatorRedeemerMilestoneReprobe _) -> validateMilestone vp outDatum outValue r
    T.ValidatorRedeemerFundsCollect params    -> validateFundsCollect vp outDatum outValue params
    _                                         -> False


--------------------------------------------------------------------------------
--  Update Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateUpdate #-}
validateUpdate :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateUpdate !vp !outDatum !outValue =
    ---------------------
    -- it runs alone
    ---------------------
    -- solo es posible actualizar admin y token admin
    -- Que sea Protocol or Campaign Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- no hay restricciones temporales
    ---------------------
    validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_NormalChanges
                (vDatum vp)
                (T.cdAdmins outDatum)
                (T.cdTokenAdminPolicy_CS outDatum))
        && validateValueNotChanged vp outValue

--------------------------------------------------------------------------------
-- MinADA Update Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateMinADAUpdate #-}
validateMinADAUpdate :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateMinADAUpdate !vp !outDatum !outValue =
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol or Campaign Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignDatum se actualiza correctamente
    -- Que el CampaignDatum value cambie con el min ADA nuevo
    -- no hay restricciones temporales
    ------------------
    validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not min ADA > 0" (newMinADA > 0)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_UpdatedMinADA"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_MinADAChanged (vDatum vp) newMinADA)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_Changed" (outValue `OnChainHelpers.isEqValue` (vInputValue vp <> LedgerAda.lovelaceValueOf adaChange))
    where
        !newMinADA = T.cdMinADA outDatum
        !adaChange = newMinADA - T.cdMinADA (vDatum vp)


--------------------------------------------------------------------------------
-- Funds Management Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateFundsAdd #-}
validateFundsAdd :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateFundsAdd !vp !outDatum !outValue =
    ------------------
    -- it runs along with CampaignFunds ID Policy (PolicyRedeemerMintID)
    ------------------
    -- Que sea Protocol or Campaign Admin
    -- Que se mintee CampaignFunds ID con la correcta póliza indicada en CampaignDatum
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignDatum se actualiza con nuevo Campaign Funds
    -- Que el CampaignDatum value no cambie
    -- el datum Campaign Funds y la direccion a donde va estara controlado por la poliza Campaign Funds, que ya me aseguro que se ejecuta el controlar que se este minteando CampaignFunds ID
    -- no hay restricciones temporales
    -- que la poliza de Campaign Funds controle el mint de este NFT y controle el CampaignFundsDatum
    ------------------
    let !fundsPolicyCS = T.cdCampaignFundsPolicyID_CS $ vDatum vp
    in traceIfFalse "not validateProtocolOrCampaignAdminAction"
        (validateProtocolOrCampaignAdmin vp)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_With_CampaignFundsAdded"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_CampaignFundsAdded (vDatum vp))
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged"
            (validateValueNotChanged vp outValue)
        && traceIfFalse "not isMintingCampaignFundsID"
            (OnChainHelpers.isNFT_Minting_With_CS fundsPolicyCS (vInfo vp))

{-# INLINEABLE validateFundsMergeAndDelete #-}
validateFundsMergeAndDelete :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateFundsMergeAndDelete !vp !outDatum !outValue =
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
    case vRedeemer vp of
        T.ValidatorRedeemerFundsMerge (T.ValidatorRedeemerFundsMergeType !qty) ->
            validateFundsOperation (qty - 1)
        T.ValidatorRedeemerFundsDelete (T.ValidatorRedeemerFundsDeleteType !qty) ->
            validateFundsOperation qty
        _ -> False
    where
        validateFundsOperation :: Integer -> Bool
        validateFundsOperation  !qty =
            let !fundsPolicyCS = T.cdCampaignFundsPolicyID_CS $ vDatum vp
            in traceIfFalse "not validateProtocolOrCampaignAdminAction"
                (validateProtocolOrCampaignAdmin vp)
                && traceIfFalse "not isCorrect_Output_Campaign_Datum_With_CampaignFundsDeleted"
                    (outDatum `OnChainHelpers.isUnsafeEqDatums`
                    CampaignHelpers.mkUpdated_Campaign_Datum_With_CampaignFundsDeleted (vDatum vp) qty)
                && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged"
                    (validateValueNotChanged vp outValue)
                && traceIfFalse "not isBurningCampaignFundsIDs"
                    (OnChainHelpers.isToken_Burning_With_CS_AndAmt fundsPolicyCS qty (vInfo vp))

--------------------------------------------------------------------------------
-- Milestone Management Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateMilestone #-}
validateMilestone :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> T.ValidatorRedeemer -> Bool
validateMilestone !vp !outDatum !outValue !redeemer =
    ---------------------
    -- it runs alone
    ---------------------
    -- solo es posible actualizar admin y token admin
    -- Que sea Protocol Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- que este en status CsReached
    -- que el milestone anterior este en status MsSuccess
    -- que el nuevo milestone actual este en status MsCreated
    ---------------------
    let !milestoneIndex = case redeemer of
            T.ValidatorRedeemerMilestoneAprobe (T.ValidatorRedeemerMilestoneAprobeType idx)   -> idx
            T.ValidatorRedeemerMilestoneReprobe (T.ValidatorRedeemerMilestoneReprobeType idx) -> idx
            _                                                                                 -> traceError "Invalid milestone redeemer"
        !protocolDatum = getProtocol_Datum vp
    in validateProtocolAdminAction vp protocolDatum
        && traceIfFalse "not isCorrectRedeemerMilestoneIndex" (isValidMilestoneIndex vp milestoneIndex)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated"
            (validateMilestoneDatumUpdate vp outDatum milestoneIndex redeemer)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged"
            (validateValueNotChanged vp outValue)
        && traceIfFalse "not isCampaignReached"
            (T.cdStatus (vDatum vp) == T.CsReached)
        && traceIfFalse "not isPreviusMilestoneAprobed"
            (validatePreviousMilestone vp milestoneIndex)
        && traceIfFalse "not isCurrentMilestoneCreated"
            (validateCurrentMilestone vp milestoneIndex)

{-# INLINEABLE isValidMilestoneIndex #-}
isValidMilestoneIndex :: ValidationParams -> Integer -> Bool
isValidMilestoneIndex !vp !idx =
    let !milestones = T.cdMilestones $ vDatum vp
        !len = length milestones
    in idx >= 0 && idx < len

{-# INLINEABLE validateMilestoneDatumUpdate #-}
validateMilestoneDatumUpdate :: ValidationParams -> T.CampaignDatumType -> Integer -> T.ValidatorRedeemer -> Bool
validateMilestoneDatumUpdate !vp !outDatum !idx !redeemer =
    case redeemer of
        T.ValidatorRedeemerMilestoneAprobe _ ->
            outDatum `OnChainHelpers.isUnsafeEqDatums`
            CampaignHelpers.mkUpdated_Campaign_Datum_With_MilestoneAprobed (vDatum vp) idx
        T.ValidatorRedeemerMilestoneReprobe _ ->
            outDatum `OnChainHelpers.isUnsafeEqDatums`
            CampaignHelpers.mkUpdated_Campaign_Datum_With_MilestoneReprobed (vDatum vp) idx
        _ -> False

{-# INLINEABLE validatePreviousMilestone #-}
validatePreviousMilestone :: ValidationParams -> Integer -> Bool
validatePreviousMilestone !vp !idx
    | idx == 0 = True
    | otherwise =
        let !milestones = T.cdMilestones $ vDatum vp
            !prevMilestone = milestones !! (idx - 1)
        in T.cmStatus prevMilestone == T.MsSuccess

{-# INLINEABLE validateCurrentMilestone #-}
validateCurrentMilestone :: ValidationParams -> Integer -> Bool
validateCurrentMilestone !vp !idx =
    let !milestones = T.cdMilestones $ vDatum vp
        !milestone = milestones !! idx
    in T.cmStatus milestone == T.MsCreated

--------------------------------------------------------------------------------
-- Collection Management Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateFundsCollect #-}
validateFundsCollect :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> T.ValidatorRedeemerFundsCollectType -> Bool
validateFundsCollect !vp !outDatum !outValue (T.ValidatorRedeemerFundsCollectType !amount) =
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
    traceIfFalse "not validateCampaignAdminAction" (validateCampaignAdminAction vp)
        && traceIfFalse "not isCampaignStatus CsReached" (isCampaignStatus vp T.CsReached)
        && traceIfFalse "not amount_ADA > 0" (amount > 0)
        && traceIfFalse "not avalaible_ADA_to_Collect >= amount_ADA" (getAvailableADA vp >= amount)
        && traceIfFalse "not isCorrectRedeemersCampaignFundsDatum" (validateCampaignFundsRedeemers vp amount)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated_With_Collect"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_CampaignFundsCollected (vDatum vp) amount)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged"
            (validateValueNotChanged vp outValue)

{-# INLINEABLE getAvailableADA #-}
getAvailableADA :: ValidationParams -> Integer
getAvailableADA !vp =
    let 
        findCurrentMilestoneIndex :: Integer
        findCurrentMilestoneIndex =
            let !searchIndex = go 0 milestones
                go !idx [] = idx
                go !idx (m:ms)
                    | T.cmStatus m /= T.MsSuccess = idx
                    | otherwise = go (idx + 1) ms
            in searchIndex
        
        calculateAvailableAmount :: Integer -> Integer
        calculateAvailableAmount !currentIdx
            | currentIdx >= length milestones = 0
            | otherwise =
                let !milestone = milestones !! currentIdx
                in case T.cmStatus milestone of
                    T.MsCreated ->
                        let !percentage = calculatePercentage currentIdx
                        in percentage * fundedADA `divide` 100
                    _ -> 0

        calculatePercentage :: Integer -> Integer
        calculatePercentage !idx
            | idx >= length milestones - 1 = 100
            | otherwise = go 0 0
            where
                go !acc !i
                    | i > idx = acc
                    | otherwise = go (acc + T.cmPerncentage (milestones !! i)) (i + 1)

        !datum = vDatum vp
        !fundedADA = T.cdFundedADA datum
        !collectedADA = T.cdCollectedADA datum
        !milestones = T.cdMilestones datum
        !currentMilestoneIndex = findCurrentMilestoneIndex
        !availableAmount = calculateAvailableAmount currentMilestoneIndex

    in availableAmount - collectedADA

{-# INLINEABLE validateCampaignFundsRedeemers #-}
validateCampaignFundsRedeemers :: ValidationParams -> Integer -> Bool
validateCampaignFundsRedeemers !vp !amount =
    let !fundsPolicyCS = T.cdCampaignFundsPolicyID_CS $ vDatum vp
        !inputs = [(LedgerApiV2.txInInfoOutRef txIn, LedgerApiV2.txInInfoResolved txIn)
                | txIn <- LedgerApiV2.txInfoInputs $ vInfo vp]
        !campaignFundsInput = case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
            @CampaignFundsT.ValidatorDatum
            @CampaignFundsT.CampaignFundsDatumType
            (vCtx vp)
            inputs
            fundsPolicyCS
            CampaignFundsT.getCampaignFunds_DatumType of
                [x] -> x
                _   -> traceError "Expected one CampaignFunds input"
    in  case OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) campaignFundsInput) (vInfo vp) of
        Nothing -> traceError "Expected CampaignFunds input with redeemers"
        Just !r -> case LedgerApiV2.fromBuiltinData @CampaignFundsT.ValidatorRedeemer $ LedgerApiV2.getRedeemer r of
            Just (CampaignFundsT.ValidatorRedeemerCollect (CampaignFundsT.ValidatorRedeemerCollectType !amount')) ->
                amount == amount'
            _ -> traceError "Expected CampaignFunds input with valid redeemer ValidatorRedeemerCollect"

--------------------------------------------------------------------------------
-- Status Update Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateStatusUpdate #-}
validateStatusUpdate :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Bool
validateStatusUpdate !vp !outDatum !outValue =
    let !protocolDatum = getProtocol_Datum vp
        !campaignFundsDatum = snd <$> getCampaignFunds vp
    in case vRedeemer vp of 
        T.ValidatorRedeemerInitializeCampaign _ -> validateInitialize vp outDatum outValue protocolDatum campaignFundsDatum
        T.ValidatorRedeemerReachedCampaign _    -> validateReached vp outDatum outValue protocolDatum campaignFundsDatum
        T.ValidatorRedeemerNotReachedCampaign _ -> validateNotReached vp outDatum outValue protocolDatum campaignFundsDatum
        _                                       -> False

{-# INLINEABLE validateInitialize #-}
validateInitialize :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Maybe ProtocolT.ProtocolDatumType -> [CampaignFundsT.CampaignFundsDatumType] -> Bool
validateInitialize !vp !outDatum !outValue !mProtocolDatum !campaignFundsDatum =
     ---------------------
    -- Inicializa la campaña, una vez que tiene los fondos a la venta necesarios
    -- Deben haber sido minteados antes y agregados a las UTXO de fondos
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignDatum value no cambie
    -- tiene que estar en estado created
    -- tiene que tener en los CampaignFunds la cantidad de tokens necesarias para poner a la venta (el max requested)
    ---------------------
    validateProtocolAdminAction vp mProtocolDatum
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatus (vDatum vp) T.CsInitialized)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged" (validateValueNotChanged vp outValue)
        && traceIfFalse "not isCampaignStatusCreated" (isCampaignStatus vp T.CsCreated)
        && traceIfFalse "not isAllCampaignFunds" (length campaignFundsDatum == T.cdFundsCount (vDatum vp))
        && traceIfFalse "not isCampaignTokensAvalaible" isCampaignTokensAvalaible
    where
        isCampaignTokensAvalaible :: Bool
        !isCampaignTokensAvalaible =
            let
                requestedMaxADA = T.cdRequestedMaxADA (vDatum vp)
                requestedTokensToSell = requestedMaxADA `divide` T.cdCampaignToken_PriceADA (vDatum vp)
                tokensAvalaibles = sum (CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken <$> campaignFundsDatum)
            in
                requestedTokensToSell == tokensAvalaibles

{-# INLINEABLE validateReached #-}
validateReached :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Maybe ProtocolT.ProtocolDatumType -> [CampaignFundsT.CampaignFundsDatumType] -> Bool
validateReached !vp !outDatum !outValue !mProtocolDatum !campaignFundsDatum =
    ---------------------
    -- Cambia el estado de campaña a alcanzada
    -- Deben haber sido vendidos la cantidad minima de tokens por lo menos
    -- Debe estar en estado inicalizada
    -- Debe estar en fecha de deadline pasado
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignDatum value no cambie
    -- Que se actualice la cantidad de ADA recivida (fundedADA) y el nuevo estado cSReached
    -- tiene que estar en estado CsInitialized
    -- tiene que tener en los CampaignFunds la cantidad de tokens necesarias vendidos, superando el minimo esperado
    -- que haya terminado la campaña
    ---------------------
        validateProtocolAdminAction vp mProtocolDatum
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatusReached (vDatum vp) tokensToSold)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged" (validateValueNotChanged vp outValue)
        && traceIfFalse "not isCampaignStatusInitialized" (isCampaignStatus vp T.CsInitialized)
        && traceIfFalse "not isAllCampaignFunds" (length campaignFundsDatum == T.cdFundsCount (vDatum vp))
        && traceIfFalse "not isCampaignFundsTokensSold" isCampaignFundsTokensSold
        && traceIfFalse "not isCampaignFinish" (isCampaignFinish vp)
    where
         ---------------------
        !tokensToSold = sum (CampaignFundsT.cfdSubtotal_Sold_CampaignToken <$> campaignFundsDatum)
        ---------------------
        isCampaignFundsTokensSold :: Bool
        !isCampaignFundsTokensSold =
            let
                !requestedMinADA = T.cdRequestedMinADA (vDatum vp)
            in
                requestedMinADA <= tokensToSold * T.cdCampaignToken_PriceADA (vDatum vp)


{-# INLINEABLE validateNotReached #-}
validateNotReached :: ValidationParams -> T.CampaignDatumType -> Ledger.Value -> Maybe ProtocolT.ProtocolDatumType -> [CampaignFundsT.CampaignFundsDatumType] -> Bool
validateNotReached !vp !outDatum !outValue !mProtocolDatum !campaignFundsDatum =
    ---------------------
    -- Cambia el estado de campaña a no alcanzada
    -- Deben haber sido vendidos menos de la cantidad minima de tokens
    -- Debe estar en estado inicalizada
    -- Debe estar en fecha de deadline pasado
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol Admin
    -- Que el CampaignDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignDatum value no cambie
    -- Que se actualice la cantidad de ADA recivida (fundedADA) y el nuevo estado cSNotReached
    -- tiene que estar en estado CsInitialized
    -- tiene que no tener en los CampaignFunds la cantidad de tokens minimos
    -- la fecha tiene que haber pasado al deadline
    ---------------------
        validateProtocolAdminAction vp mProtocolDatum
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Updated"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
             CampaignHelpers.mkUpdated_Campaign_Datum_With_NewStatusNotReached (vDatum vp) tokensToSold)
        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged" (validateValueNotChanged vp outValue)
        && traceIfFalse "not isCampaignStatusInitialized" (isCampaignStatus vp T.CsInitialized)
        && traceIfFalse "not isAllCampaignFunds" (length campaignFundsDatum == T.cdFundsCount (vDatum vp))
        && traceIfFalse "not isCampaignFundsTokensNotSold" isCampaignFundsTokensNotSold
        && traceIfFalse "not isCampaignFinish" (isCampaignFinish vp)
    where
        ---------------------
        !tokensToSold = sum (CampaignFundsT.cfdSubtotal_Sold_CampaignToken <$> campaignFundsDatum)
        ---------------------
        isCampaignFundsTokensNotSold :: Bool
        !isCampaignFundsTokensNotSold =
            let
                !requestedMinADA = T.cdRequestedMinADA (vDatum vp)
            in
                requestedMinADA > tokensToSold * T.cdCampaignToken_PriceADA (vDatum vp)

--------------------------------------------------------------------------------
-- Delete Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateDelete #-}
validateDelete :: ValidationParams -> Bool
validateDelete !vp =
     ------------------
    -- it runs along with Campaign ID Policy  (PolicyRedeemerBurnID)
    ------------------
    -- Que sea Protocol Admin
    -- que el Campaign tenga CERO CampaignFunds
    -- Que se quemen los Campaign ID con la correcta póliza indicada en CampaignDatum siendo consumido
    ------------------
    let !protocolDatum = getProtocol_Datum vp
    in validateProtocolAdminAction vp protocolDatum
        && traceIfFalse "not isBurningCampaignID" (OnChainHelpers.isNFT_Burning_With_AC (vCampaignID_AC vp) (vInfo vp))
        && traceIfFalse "not isZeroCampaignFunds" (T.cdFundsCount (vDatum vp) == 0)


--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

{-# INLINEABLE getOwnInputs #-}
getOwnInputs :: ValidationParams -> [LedgerContextsV2.TxOut]
getOwnInputs !vp  =
    [ LedgerApiV2.txInInfoResolved txInfoInput
    | txInfoInput <- LedgerApiV2.txInfoInputs $ vInfo vp
    , let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
    , OnChainHelpers.isScriptAddress address && address == vOwnAddress vp
    ]

{-# INLINEABLE getOwnOutputs #-}
getOwnOutputs :: ValidationParams -> [LedgerContextsV2.TxOut]
getOwnOutputs !vp =
    [ output
    | output <- LedgerApiV2.txInfoOutputs $ vInfo vp
     , let address = LedgerApiV2.txOutAddress output
    , OnChainHelpers.isScriptAddress address && LedgerApiV2.txOutAddress output == vOwnAddress vp
    ]

{-# INLINEABLE getRefInputs #-}
getRefInputs :: ValidationParams -> [LedgerContextsV2.TxOut]
getRefInputs !vp =
    [ LedgerApiV2.txInInfoResolved txInfoInput
    | txInfoInput <- LedgerApiV2.txInfoReferenceInputs $ vInfo vp
    , OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
    ]

{-# INLINEABLE getProtocol_Datum #-}
getProtocol_Datum :: ValidationParams -> Maybe ProtocolT.ProtocolDatumType
getProtocol_Datum !vp =
    let !refInputs = getRefInputs vp
        !protocolDatums = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
            (vCtx vp)
            refInputs
            (vProtocolID_AC vp)
            ProtocolT.getProtocol_DatumType
    in case protocolDatums of
        [(_,x)] -> Just x
        _   -> Nothing

{-# INLINEABLE getCampaignFunds #-}
getCampaignFunds :: ValidationParams -> [(LedgerContextsV2.TxOut, CampaignFundsT.CampaignFundsDatumType)] 
getCampaignFunds !vp  =
    let !refInputs = getRefInputs vp
        !campaignFundsID_CS = T.cdCampaignFundsPolicyID_CS $ vDatum vp
        !fundsDatums = OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
            @CampaignFundsT.ValidatorDatum
            @CampaignFundsT.CampaignFundsDatumType
            (vCtx vp)
            refInputs
            campaignFundsID_CS
            CampaignFundsT.getCampaignFunds_DatumType 
    in case fundsDatums of
        [] -> traceError "Expected all CampaignFunds as inputRef"
        x  -> x

{-# INLINEABLE getOutput_Campaign_DatumAndValue #-}
getOutput_Campaign_DatumAndValue :: ValidationParams -> Maybe (T.CampaignDatumType, Ledger.Value)
getOutput_Campaign_DatumAndValue !vp =
    let !outputs = getOwnOutputs vp
    in case outputs of
        [output] ->
            let !maybeTxOut = OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                    @T.ValidatorDatum
                    @T.CampaignDatumType
                    (vCtx vp)
                    output
                    (vCampaignID_AC vp)
                    (Just $ vOwnAddress vp)
                    T.getCampaign_DatumType
            in case maybeTxOut of
                Just txOut -> Just (OnChainHelpers.getDatum_In_TxOut_And_Datum txOut, LedgerApiV2.txOutValue output)
                Nothing    -> Nothing
        _ -> Nothing

{-# INLINEABLE isCampaignOpen #-}
isCampaignOpen :: ValidationParams -> Bool
isCampaignOpen !vp =
    let !datum = vDatum vp
        !info = vInfo vp
    in OnChainHelpers.isDateReached (T.cdBeginAt datum) info
       && not (OnChainHelpers.isDateReached (T.cdDeadline datum) info)

{-# INLINEABLE isCampaignFinish #-}
isCampaignFinish :: ValidationParams -> Bool
isCampaignFinish = not . isCampaignOpen

{-# INLINEABLE isCampaignStatus #-}
isCampaignStatus :: ValidationParams -> T.CapaignStatus -> Bool
isCampaignStatus !vp !status = T.cdStatus (vDatum vp) == status

{-# INLINEABLE validateValueNotChanged #-}
validateValueNotChanged :: ValidationParams -> Ledger.Value -> Bool
validateValueNotChanged !vp !outValue =
    traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged" $
    outValue `OnChainHelpers.isEqValue` vInputValue vp

--------------------------------------------------------------------------------
-- Admin Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE isAdminTokenPresent #-}
isAdminTokenPresent :: ValidationParams -> Bool
isAdminTokenPresent !vp =
    case LedgerApiV2.txInfoOutputs $ vInfo vp of
        []           -> False
        (output : _) ->
            let !tokenAdmin_AC = LedgerValue.AssetClass (T.getAdminToken_CS $ vDatum vp, T.tokenAdmin_TN)
            in OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC

{-# INLINEABLE validateCampaignAdminAction #-}
validateCampaignAdminAction :: ValidationParams -> Bool
validateCampaignAdminAction !vp =
    ------------------
    -- Que este el token de admin presente
    -- o Que sea Campaign Admin
    ------------------
    traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent"
        (OnChainHelpers.isSignedByAny ( T.getAdmins $ vDatum vp) (vInfo vp) || isAdminTokenPresent vp)

{-# INLINEABLE validateProtocolAdminAction #-}
validateProtocolAdminAction :: ValidationParams -> Maybe ProtocolT.ProtocolDatumType -> Bool
validateProtocolAdminAction !vp !mprot =
    let !admins = case mprot of
            Just protDatum -> T.getAdmins protDatum
            _                   -> traceError "Expected Protocol at inputRef"
    in traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent"
        (OnChainHelpers.isSignedByAny admins (vInfo vp) || isAdminTokenPresent vp)

{-# INLINEABLE validateProtocolOrCampaignAdmin #-}
validateProtocolOrCampaignAdmin :: ValidationParams -> Bool
validateProtocolOrCampaignAdmin !vp =
    ------------------
    -- Que este el token de admin presente
    -- o Que sea Campaign Admin
    -- o Que sea Protocol Admin si hay input ref protocol
    ------------------
    let !protocolDatum = getProtocol_Datum vp
        !admins = T.getAdmins (vDatum vp) ++ maybe [] T.getAdmins protocolDatum
    in traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent"
        (OnChainHelpers.isSignedByAny admins (vInfo vp) || isAdminTokenPresent vp)


--------------------------------------------------------------------------------

{-# INLINEABLE mkPolicy #-}
mkPolicy :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicy (T.PolicyParams !protocolPolicyID_CS !campaignPolicy_TxOutRef !campaignValidator_Hash) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && validatePolciy
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
        validatePolciy :: Bool
        !validatePolciy = case redeemer of
            (T.PolicyRedeemerMintID _)            -> validateMintAndBurnIDRedeemers
            (T.PolicyRedeemerBurnID _)            -> validateMintAndBurnIDRedeemers
            (T.PolicyRedeemerMintCampaignToken _) -> validateMintAndBurnCampaignTokenRedeemers
            (T.PolicyRedeemerBurnCampaignToken _) -> validateMintAndBurnCampaignTokenRedeemers
        ------------------
        validateMintAndBurnIDRedeemers :: Bool
        validateMintAndBurnIDRedeemers =
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
                    -- TODO: falta validar que sea protocol admin
                    ---------------------
                    traceIfFalse "not isTxOutAnInput" (OnChainHelpers.isTxOutAnInput campaignPolicy_TxOutRef info)
                        && traceIfFalse "not isMintingID" isMintingID
                        && traceIfFalse "not isCorrect_Output_Campaign_Datum" isCorrect_Output_Campaign_Datum
                        && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value" isCorrect_Output_Campaign_Datum_Value
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
                        !_ =
                            if null outputs_txOuts
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
                                    T.getCampaign_DatumType
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
                        !mint_CampaignToken = T.cdMint_CampaignToken campaignDatum_Out
                        !campaignToken_CS = T.cdCampaignToken_CS campaignDatum_Out
                        !campaignToken_TN = T.cdCampaignToken_TN campaignDatum_Out
                        !campaignToken_PriceADA = T.cdCampaignToken_PriceADA campaignDatum_Out
                        !requestedMaxADA = T.cdRequestedMaxADA campaignDatum_Out
                        !requestedMinADA = T.cdRequestedMinADA campaignDatum_Out
                        !fundedADA = 0
                        !collectedADA = 0
                        !beginAt = T.cdBeginAt campaignDatum_Out
                        !deadline = T.cdDeadline campaignDatum_Out
                        !status = T.CsCreated
                        !milestones = T.cdMilestones campaignDatum_Out
                        !fundsCount = 0
                        !fundsIndex = 0
                        ---------------------
                        !campaignDatum_Out_Control =
                            T.mkCampaign_DatumType
                                campaignPolicy_CS
                                campaignFundsPolicyID_CS
                                admins
                                tokenAdminPolicy_CS
                                mint_CampaignToken
                                campaignToken_CS
                                campaignToken_TN
                                campaignToken_PriceADA
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
                        isCorrect_Output_Campaign_Datum :: Bool
                        !isCorrect_Output_Campaign_Datum =
                            let
                                -----------------
                                -- Check if campaignToken_CS equals campaignPolicy_CS when minting tokens
                                validCampaignToken_CS = not mint_CampaignToken || (campaignToken_CS == campaignPolicy_CS)
                                -- Check if campaignToken_TN is not empty
                                campaignToken_TN_BS = LedgerApiV2.unTokenName campaignToken_TN
                                validCampaignToken_TN = lengthOfByteString campaignToken_TN_BS /= 0
                                -- Check if campaignToken_PriceADA is greater than zero
                                validCampaignToken_PriceADA = campaignToken_PriceADA > 0
                                -- Check if requestedMaxADA is greater than requestedMinADA, and requestedMinADA is greater than zero
                                validRequestedMinMaxADA = requestedMinADA > 0 && requestedMaxADA >= requestedMinADA
                                -- Check if requestedMaxADA and requestedMinADA are divisible by campaignToken_PriceADA
                                divisibleMax = requestedMaxADA `modulo` campaignToken_PriceADA == 0
                                -- divisibleMin = requestedMinADA `modulo` campaignToken_PriceADA == 0
                                validRequestedPriceDivisibility = divisibleMax 
                                -- Check valid milestones
                                validateMilestones =
                                    let
                                        -- Check if there is at least one milestone
                                        hasMilestones = not (null milestones)
                                        -- Check if all milestones have status MsCreated
                                        allCreatedMilestones = all (\m -> T.cmStatus m == T.MsCreated) milestones
                                        -- Check if the total percentage of all milestones sums to 100
                                        totalPercentage = sum [T.cmPerncentage m | m <- milestones]
                                        isValidPercentage = totalPercentage == 100
                                    in
                                        hasMilestones && allCreatedMilestones && isValidPercentage
                            in
                                -----------------
                                traceIfFalse "not isDateReached deadline" (OnChainHelpers.isDateNotReached deadline info)
                                    && traceIfFalse "not deadline > beginAt" (deadline > beginAt)
                                    && traceIfFalse "not validateMilestones" validateMilestones
                                    && traceIfFalse "not validCampaignToken_CS" validCampaignToken_CS
                                    && traceIfFalse "not validCampaignToken_TN" validCampaignToken_TN
                                    && traceIfFalse "not validCampaignToken_PriceADA" validCampaignToken_PriceADA
                                    && traceIfFalse "not validRequestedMinAndMaxADA" validRequestedMinMaxADA
                                    && traceIfFalse "not validRequestedPriceDivisibility" validRequestedPriceDivisibility
                                    && campaignDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignDatum_Out_Control

                        ------------------
                        isCorrect_Output_Campaign_Datum_Value :: Bool
                        !isCorrect_Output_Campaign_Datum_Value =
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
        validateMintAndBurnCampaignTokenRedeemers :: Bool
        validateMintAndBurnCampaignTokenRedeemers  =
            case redeemer of
                (T.PolicyRedeemerMintCampaignToken _) ->
                    ------------------
                    -- it runs along with CampaignFunds Validator (ValidatorRedeemerDeposit)
                    ------------------
                    -- que este la campaign status sea CsCreated
                    -- que la campaign este configurada para mintear los tokens que se venden
                    -- que el redeemer del CampaignFunds sea ValidatorRedeemerDeposit
                    -- que se este minetando, solamente, con AC correcta
                    -- que el valor a mintear sea al determiando en el redeemer del CampaignFunds (lo reviso en el validar del CampaignFunds)
                    ------------------
                    traceIfFalse "not isCampaignCreated" isCampaignCreated
                        && traceIfFalse "not isCampaignWithMintingCampaignToken" isCampaignWithMintingCampaignToken
                        && traceIfFalse "not isCampaignFundsValidatorRedeemerDeposit" isCampaignFundsValidatorRedeemerDeposit
                        && traceIfFalse "not isMintingCampaignToken" isMintingCampaignToken
                    where
                        ------------------
                        isCampaignFundsValidatorRedeemerDeposit :: Bool
                        isCampaignFundsValidatorRedeemerDeposit = case redeemerFor_CampaignFundsDatum of
                            CampaignFundsT.ValidatorRedeemerDeposit _ -> True
                            _                                         -> False
                        ------------------
                        !amount = case redeemerFor_CampaignFundsDatum of
                            CampaignFundsT.ValidatorRedeemerDeposit (CampaignFundsT.ValidatorRedeemerDepositType amt) -> amt
                            _                                                                                         -> -1
                        -----------------
                        !valueFor_Mint_CampaignToken = LedgerValue.assetClassValue campaignToken_AC amount
                        ---------------------
                        isMintingCampaignToken :: Bool
                        !isMintingCampaignToken = amount >= 1 && ownMintingValue `OnChainHelpers.isEqValue` valueFor_Mint_CampaignToken
                ------------------
                -----------------
                (T.PolicyRedeemerBurnCampaignToken _) ->
                    ------------------
                    -- it runs along with CampaignFunds Validator (isCampaignFundsValidatorRedeemerWithdraw)
                    ------------------
                    -- que este la campaign status sea CsCreated, CsNotReached or CsFailedMilestone
                    -- que la campaign este configurada para mintear los tokens que se venden
                    -- que el redeemer del CampaignFunds sea Withdraw
                    -- que el valor a quemar sea al determiando en el redeemer del CampaignFunds (lo reviso en el validar del CampaignFunds)
                    ------------------
                    traceIfFalse "not isCampaignCreated_NotReached_Or_FailedMilestone" isCampaignCreated_NotReached_Or_FailedMilestone
                        && traceIfFalse "not isCampaignWithMintingCampaignToken" isCampaignWithMintingCampaignToken
                        && traceIfFalse "not isCampaignFundsValidatorRedeemerWithdraw" isCampaignFundsValidatorRedeemerWithdraw
                        && traceIfFalse "not isBurningCampaignToken" isBurningCampaignToken
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
                            CampaignFundsT.ValidatorRedeemerWithdraw (CampaignFundsT.ValidatorRedeemerWithdrawType amt) -> amt
                            _                                                                                           -> 1
                        -----------------
                        !valueFor_Burn_CampaignToken = LedgerValue.assetClassValue campaignToken_AC (negate amount)
                        ---------------------
                        isBurningCampaignToken :: Bool
                        !isBurningCampaignToken = amount < 0 && ownMintingValue `OnChainHelpers.isEqValue` valueFor_Burn_CampaignToken
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
                        T.getCampaign_DatumType of
                        [x] -> x
                        _   -> traceError "Expected exactly one Campaign input ref"
                ------------------
                !campaignDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_CampaignDatum
                ------------------
                !campaignToken_TN = T.cdCampaignToken_TN campaignDatum_In
                !campaignToken_AC = LedgerValue.AssetClass (campaignPolicy_CS, campaignToken_TN)
                ------------------
                isCampaignCreated :: Bool
                !isCampaignCreated = T.cdStatus campaignDatum_In == T.CsCreated
                ------------------
                isCampaignWithMintingCampaignToken :: Bool
                !isCampaignWithMintingCampaignToken = T.cdMint_CampaignToken campaignDatum_In
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
                                CampaignFundsT.getCampaignFunds_DatumType of
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

--------------------------------------------------------------------------------2

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
mkWrappedPolicyID protocolPolicyID_CS campaignPolicy_TxHash campaignPolicy_TxOutputIndex campaignValidator_Hash = mkPolicy params
    where
        tid = PlutusTx.unsafeFromBuiltinData campaignPolicy_TxHash :: BuiltinByteString
        txout =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = LedgerApiV2.TxId tid
                , LedgerApiV2.txOutRefIdx = PlutusTx.unsafeFromBuiltinData campaignPolicy_TxOutputIndex
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
-- validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])
validatorCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedValidator||])

------------------------------------------------------------------------------2
