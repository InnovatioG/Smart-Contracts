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

import qualified Ledger
import qualified Ledger.Ada                as LedgerADA
import qualified Ledger.Ada                as LedgerAda
import qualified Ledger.Value              as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude

----------------------------------------------------------------------------2
-- Import Internos
----------------------------------------------------------------------------2

import qualified Campaign.Funds.Types      as T
import qualified Campaign.Helpers          as CampaignHelpers
import qualified Campaign.Types            as CampaignT
import qualified Constants                 as T
import qualified Helpers.OnChain           as OnChainHelpers
import           Prelude                   (Show)
import qualified Protocol.Types            as ProtocolT
import qualified Types                     as T
import qualified PlutusTx.Ratio as TxRatio

----------------------------------------------------------------------------2
-- Modulo
----------------------------------------------------------------------------2

--------------------------------------------------------------------------------
-- Validator Types and Context
--------------------------------------------------------------------------------

data ValidationParams
    = ValidationParams
          { vParams                   :: !T.ValidatorParams
          , vCtx                      :: !LedgerContextsV2.ScriptContext
          , vInfo                     :: !LedgerContextsV2.TxInfo
          , vRedeemer                 :: !T.ValidatorRedeemer
          , vDatum                    :: !T.CampaignFundsDatumType
          , vOwnInput                 :: !LedgerContextsV2.TxOut
          , vOwnAddress               :: !LedgerApiV2.Address
          , vInputValue               :: !Ledger.Value
          , vCampaignID_AC            :: !LedgerValue.AssetClass
          , vProtocolID_AC            :: !LedgerValue.AssetClass
          , vCampaignFundsID_AC       :: !LedgerValue.AssetClass
          , vCampaignFundsPolicyID_CS :: !LedgerApiV2.CurrencySymbol
          , vOwnInputs                :: ![LedgerContextsV2.TxOut]
          , vOwnOutputs               :: ![LedgerContextsV2.TxOut]
          , vInputsLength             :: !Integer
          , vOutputsLength            :: !Integer
          , vCampaignDatum            :: !CampaignT.CampaignDatumType
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
        !datum = T.getCampaignFunds_DatumType $ LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
        !ownInput = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
        !address = LedgerApiV2.txOutAddress ownInput
        !campaignPolicy_CS = T.cfdCampaignPolicy_CS datum
        !campaignFundsPolicyID_CS = T.cfdCampaignFundsPolicyID_CS datum
        !protocolPolicyID_CS = T.vpProtocolPolicyID_CS params
        !ownInputs = getOwnInputs info address
        !ownOutputs = getOwnOutputs info address
        !useNormalInput = case redeemer of
            T.ValidatorRedeemerDelete _  -> True
            T.ValidatorRedeemerCollect _ -> True
            T.ValidatorRedeemerMerge _   -> True
            _                            -> False
        !campaignDatum = getCampaign_Datum ctx (LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)) useNormalInput
        !campaignFunds_Index = T.cfdIndex datum
        !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
    in ValidationParams
        { vParams = params
        , vCtx = ctx
        , vInfo = info
        , vRedeemer = redeemer
        , vDatum = datum
        , vOwnInput = ownInput
        , vOwnAddress = address
        , vInputValue = LedgerApiV2.txOutValue ownInput
        , vCampaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
        , vProtocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        , vCampaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
        , vCampaignFundsPolicyID_CS = campaignFundsPolicyID_CS
        , vOwnInputs = ownInputs
        , vOwnOutputs = ownOutputs
        , vInputsLength = length ownInputs
        , vOutputsLength = length ownOutputs
        , vCampaignDatum = campaignDatum
        }

--------------------------------------------------------------------------------
-- Primary Validation Functions
--------------------------------------------------------------------------------

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
validateByInputOutput !vp = case vRedeemer vp of
    T.ValidatorRedeemerDelete _        -> validateInputsNoOutput vp
    T.ValidatorRedeemerBalanceAssets _ -> validateManyInputsSameOutputs vp
    _                                  -> validateInputsOneOutput vp

{-# INLINEABLE validateInputsNoOutput #-}
validateInputsNoOutput :: ValidationParams -> Bool
validateInputsNoOutput !vp =
    traceIfFalse "not null outputs_Own_txOuts" (vOutputsLength vp == 0)
        && validateDelete vp

{-# INLINEABLE validateManyInputsSameOutputs #-}
validateManyInputsSameOutputs :: ValidationParams -> Bool
validateManyInputsSameOutputs !vp =
    traceIfFalse "not isSameOutputsAsInputs" (vInputsLength vp == vOutputsLength vp)
        && traceIfFalse "not length inputs_Own_TxOuts > 1" (vInputsLength vp > 1)
        && validateBalance vp

{-# INLINEABLE validateInputsOneOutput #-}
validateInputsOneOutput :: ValidationParams -> Bool
validateInputsOneOutput !vp =
    case getOutputCampaignFundsDatums vp of
        [(output, outDatum)] ->
            let outValue = LedgerApiV2.txOutValue output
            in if vInputsLength vp == 1
                  then validateSingleInputRedeemer vp outDatum outValue
                  else validateMultipleInputsRedeemer vp outDatum outValue
        _ -> traceError "Expected exactly one CampaignFunds output"

{-# INLINEABLE validateSingleInputRedeemer #-}
validateSingleInputRedeemer :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
validateSingleInputRedeemer !vp !outDatum !outValue = case vRedeemer vp of
    T.ValidatorRedeemerUpdateMinADA _ -> validateMinADAUpdate vp outDatum outValue
    T.ValidatorRedeemerDeposit r      -> validateDeposit vp outDatum outValue r
    T.ValidatorRedeemerWithdraw r     -> validateWithdraw vp outDatum outValue r
    T.ValidatorRedeemerCollect r      -> validateCollect vp outDatum outValue r
    T.ValidatorRedeemerSell r         -> validateSell vp outDatum outValue r
    T.ValidatorRedeemerGetBack r      -> validateGetBack vp outDatum outValue r
    _                                 -> False

{-# INLINEABLE validateMultipleInputsRedeemer #-}
validateMultipleInputsRedeemer :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
validateMultipleInputsRedeemer !vp !outDatum !outValue = case vRedeemer vp of
    T.ValidatorRedeemerMerge _ -> validateMerge vp outDatum outValue
    _                          -> False


--------------------------------------------------------------------------------
-- MinADA Update Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateMinADAUpdate #-}
validateMinADAUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
validateMinADAUpdate !vp !outDatum !outValue =
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Campaign Admin
    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignFundsDatum se actualiza correctamente
    -- Que el CampaignFundsDatum value cambie con el min ADA nuevo
    -- no hay restricciones temporales
    ------------------
    validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not min ADA > 0" (T.cfdMinADA outDatum > 0)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_UpdatedMinADA"
            (outDatum `OnChainHelpers.isUnsafeEqDatums`
            CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_MinADAChanged (vDatum vp) newMinADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_ChangedADA"
            (outValue `OnChainHelpers.isEqValue` (vInputValue vp <> LedgerAda.lovelaceValueOf adaChange))
     where
        !newMinADA = T.cfdMinADA outDatum
        !adaChange = newMinADA - T.cfdMinADA (vDatum vp)

--------------------------------------------------------------------------------
-- Deposit Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateDeposit #-}
validateDeposit :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> T.ValidatorRedeemerDepositType -> Bool
validateDeposit !vp !outDatum !outValue (T.ValidatorRedeemerDepositType !amount) =
    ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol or Campaign Admin
    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos depositos
    -- Que el CampaignFundsDatum value cambie con los tokens depositados
    -- Que el CampaignDatum este en estado CsCreated
    ------------------
    validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not isCampaignStatus CsCreated"
            (isCampaignStatus vp CampaignT.CsCreated)
        && traceIfFalse "not amount > 0"
            (amount > 0)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Updated_With_Deposits"
            (validateDepositDatumUpdate vp outDatum amount)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Changed_With_Deposits"
            (validateDepositValueChange vp outValue amount)

{-# INLINEABLE validateDepositDatumUpdate #-}
validateDepositDatumUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Integer -> Bool
validateDepositDatumUpdate !vp !outDatum !amount =
    let !campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Deposits
            (vDatum vp)
            amount
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control

{-# INLINEABLE validateDepositValueChange #-}
validateDepositValueChange :: ValidationParams -> Ledger.Value -> Integer -> Bool
validateDepositValueChange !vp !outValue !amount =
    let !campaignToken_AC = getCampaignToken_AC vp
        !valueOf_CampaignToken = LedgerValue.assetClassValue campaignToken_AC amount
        !valueFor_Control = vInputValue vp <> valueOf_CampaignToken
    in outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- Withdraw Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateWithdraw #-}
validateWithdraw :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> T.ValidatorRedeemerWithdrawType -> Bool
validateWithdraw !vp !outDatum !outValue (T.ValidatorRedeemerWithdrawType !amount) =
     ---------------------
    -- it runs alone
    ---------------------
    -- Que sea Protocol or Campaign Admin
    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos withdraw
    -- Que el CampaignFundsDatum value cambie con los tokens withdraw
    -- Que el CampaignDatum este en estado CsCreated
    ------------------
    validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not isCampaignStatus CsCreated"
            (isCampaignStatus vp CampaignT.CsCreated)
        && traceIfFalse "not amount > 0"
            (amount > 0)
        && traceIfFalse "not avalaible_CampaignToken >= amount"
            (T.cfdSubtotal_Avalaible_CampaignToken (vDatum vp) >= amount)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Updated_With_Withdraw"
            (validateWithdrawDatumUpdate vp outDatum amount)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Changed_With_Withdraw"
            (validateWithdrawValueChange vp outValue amount)

{-# INLINEABLE validateWithdrawDatumUpdate #-}
validateWithdrawDatumUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Integer -> Bool
validateWithdrawDatumUpdate !vp !outDatum !amount =
    let !campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Withdraw
            (vDatum vp)
            amount
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control

{-# INLINEABLE validateWithdrawValueChange #-}
validateWithdrawValueChange :: ValidationParams -> Ledger.Value -> Integer -> Bool
validateWithdrawValueChange !vp !outValue !amount =
    let !campaignToken_AC = getCampaignToken_AC vp
        !valueOf_CampaignToken = LedgerValue.assetClassValue campaignToken_AC amount
        !valueFor_Control = vInputValue vp <> negate valueOf_CampaignToken
    in outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- Sell Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateSell #-}
validateSell :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> T.ValidatorRedeemerSellType -> Bool
validateSell !vp !outDatum !outValue (T.ValidatorRedeemerSellType !amount_CampaignToken) =
    ---------------------
    -- it runs alone
    ---------------------
    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignFundsDatum se actualiza correctamente con nuevos vendidos
    -- Que el CampaignFundsDatum value cambie con el nuevo ADA y menos los tokens vendidos
    -- Que el CampaignDatum este en estado CsInitialized
    -- Que la fecha sea entre Begin y Deadline
    ------------------
    traceIfFalse "not isCampaignOpen"
        (isCampaignOpen vp)
        && traceIfFalse "not isCampaignStatus CsInitialized"
            (isCampaignStatus vp CampaignT.CsInitialized)
        && traceIfFalse "not amount_CampaignToken > 0"
            (amount_CampaignToken > 0)
        && traceIfFalse "not avalaible_CampaignToken >= amount_CampaignToken"
            (T.cfdSubtotal_Avalaible_CampaignToken (vDatum vp) >= amount_CampaignToken)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Updated_With_SoldTokens"
            (validateSellDatumUpdate vp outDatum amount_CampaignToken amount_ADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Changed_WithSoldTokens"
            (validateSellValueChange vp outValue amount_CampaignToken amount_ADA)
    where
        !amount_ADA = amount_CampaignToken * CampaignT.cdCampaignToken_PriceADA (vCampaignDatum vp)
{-# INLINEABLE validateSellDatumUpdate #-}
validateSellDatumUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Integer -> Integer -> Bool
validateSellDatumUpdate !vp !outDatum !amount_CampaignToken !amount_ADA =
    let
        !campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_SoldTokens
            (vDatum vp)
            amount_CampaignToken
            amount_ADA
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control

{-# INLINEABLE validateSellValueChange #-}
validateSellValueChange :: ValidationParams -> Ledger.Value -> Integer -> Integer -> Bool
validateSellValueChange !vp !outValue !amount_CampaignToken !amount_ADA=
    let !campaignToken_AC = getCampaignToken_AC vp
        !valueOf_CampaignToken = LedgerValue.assetClassValue campaignToken_AC amount_CampaignToken
        !valueOf_ADA = LedgerADA.lovelaceValueOf amount_ADA
        !valueFor_Control = vInputValue vp <> negate valueOf_CampaignToken <> valueOf_ADA
    in outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- GetBack Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateGetBack #-}
validateGetBack :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> T.ValidatorRedeemerGetBackType -> Bool
validateGetBack !vp !outDatum !outValue (T.ValidatorRedeemerGetBackType !amount_CampaignToken) =
    ---------------------
    -- it runs alone
    ---------------------
    -- Que el CampaignFundsDatum regrese a Campaign Val (se hace automaticamente al buscar outputs en same address)
    -- Que el CampaignFundsDatum se actualiza correctamente con nuevo GetBack
    -- Que el CampaignFundsDatum value cambie con los tokens extraidos
    -- Que el CampaignDatum este en estado CsNotReached | CsFailedMilestone
    ------------------
    -- TODO:
    -- si el caso fuera CsFailedMilestone, hay que devolver proporcionalmente a lo vendido, a cada usuario. No se puede devolver todo.
    -- Usar para eso un total vendido, guardado en CampaignDatum que se actualiza en redeemer ValidatorRedeemerNotReachedCampaign
    ------------------
        traceIfFalse "not isCampaignStatus CsNotReached | CsFailedMilestone"
            (isCampaignStatus vp CampaignT.CsNotReached || isCampaignStatus vp CampaignT.CsFailedMilestone)
        && traceIfFalse "not amount_CampaignToken > 0"
            (amount_CampaignToken > 0)
        && traceIfFalse "not avalaible_ADA >= amount_ADA"
            (T.cfdSubtotal_Avalaible_ADA (vDatum vp) >= amount_ADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Updated_With_GetBack"
            (validateGetBackDatumUpdate vp outDatum amount_CampaignToken amount_ADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Changed_With_GetBack"
            (validateGetBackValueChange vp outValue amount_CampaignToken amount_ADA)
        where
            !amount_ADA = calculateGetBackAmountADA vp amount_CampaignToken

{-# INLINEABLE calculateGetBackAmountADA #-}
calculateGetBackAmountADA :: ValidationParams -> Integer -> Integer
calculateGetBackAmountADA !vp !amount_CampaignToken =
    let
        !datum = vCampaignDatum vp
        !priceADA = CampaignT.cdCampaignToken_PriceADA datum
        !funded = CampaignT.cdFundedADA datum
        !collected = CampaignT.cdCollectedADA datum

        !valuation = if funded == 0 then TxRatio.fromInteger 1 else TxRatio.fromInteger 1 - TxRatio.unsafeRatio collected funded
        !priceRational = TxRatio.fromInteger priceADA
        !finalPrice = valuation * priceRational

        !totalRational = TxRatio.fromInteger amount_CampaignToken * finalPrice
        !result = TxRatio.truncate totalRational
    in result

{-# INLINEABLE validateGetBackDatumUpdate #-}
validateGetBackDatumUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Integer -> Integer -> Bool
validateGetBackDatumUpdate !vp !outDatum !amount_CampaignToken !amount_ADA =
    let !campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_GetBackTokens
            (vDatum vp)
            amount_CampaignToken
            amount_ADA
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control

{-# INLINEABLE validateGetBackValueChange #-}
validateGetBackValueChange :: ValidationParams -> Ledger.Value -> Integer -> Integer -> Bool
validateGetBackValueChange !vp !outValue !amount_CampaignToken !amount_ADA  =
    let !campaignToken_AC = getCampaignToken_AC vp
        !valueOf_CampaignToken = LedgerValue.assetClassValue campaignToken_AC amount_CampaignToken
        !valueOf_ADA = LedgerADA.lovelaceValueOf amount_ADA
        !valueFor_Control = vInputValue vp <> valueOf_CampaignToken <> negate valueOf_ADA
    in outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- Collect Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateCollect #-}
validateCollect :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> T.ValidatorRedeemerCollectType -> Bool
validateCollect !vp !outDatum !outValue (T.ValidatorRedeemerCollectType !amount_ADA) =
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
    validateCampaignAdminAction vp
        && traceIfFalse "not isCorrectRedeemersCampaignDatum"
            (validateCollectCampaignRedeemer vp)
        && traceIfFalse "not avalaible_ADA_to_Collect_in_Fund >= amount_ADA"
            (T.cfdSubtotal_Avalaible_ADA (vDatum vp) >= amount_ADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Updated_With_Collect"
            (validateCollectDatumUpdate vp outDatum amount_ADA)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Changed_With_Collect"
            (validateCollectValueChange vp outValue amount_ADA)

{-# INLINEABLE validateCollectCampaignRedeemer #-}
validateCollectCampaignRedeemer :: ValidationParams -> Bool
validateCollectCampaignRedeemer !vp =
    let !inputs = [(LedgerApiV2.txInInfoOutRef txIn, LedgerApiV2.txInInfoResolved txIn)
                 | txIn <- LedgerApiV2.txInfoInputs $ vInfo vp]
        !campaignInput = case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
            @CampaignT.ValidatorDatum
            @CampaignT.CampaignDatumType
            (vCtx vp)
            inputs
            (vCampaignID_AC vp)
            CampaignT.getCampaign_DatumType of
                [x] -> x
                _   -> traceError "Expected one Campaign input"

    in  case OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) campaignInput) (vInfo vp) of
            Nothing -> traceError "Expected Campaign input with redeemer"
            Just r -> case LedgerApiV2.fromBuiltinData @CampaignT.ValidatorRedeemer $ LedgerApiV2.getRedeemer r of
                Just (CampaignT.ValidatorRedeemerFundsCollect _) -> True
                _                                                -> traceError "Expected Campaign input with valid redeemer ValidatorRedeemerFundsCollect"

{-# INLINEABLE validateCollectDatumUpdate #-}
validateCollectDatumUpdate :: ValidationParams -> T.CampaignFundsDatumType -> Integer -> Bool
validateCollectDatumUpdate !vp !outDatum !amount_ADA =
    let !campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_Collect
            (vDatum vp)
            amount_ADA
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control

{-# INLINEABLE validateCollectValueChange #-}
validateCollectValueChange :: ValidationParams -> Ledger.Value -> Integer -> Bool
validateCollectValueChange !vp !outValue !amount_ADA =
    let
        valueOf_ADA = LedgerADA.lovelaceValueOf amount_ADA
        valueFor_Control = vInputValue vp <> negate valueOf_ADA
    in outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- Balance Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateBalance #-}
validateBalance :: ValidationParams -> Bool
validateBalance !vp =
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
    let !inputDatums = getInputCampaignFundsDatums vp
        !outputDatums = getOutputCampaignFundsDatums vp
    in validateProtocolOrCampaignAdmin vp
        && traceIfFalse "not isCorrect_Outputs_CampaignFunds_Datums_Updated_Balanced" (validateBalancedDatums inputDatums outputDatums)
        && traceIfFalse "not isCorrect_Outputs_CampaignFunds_Datums_Values_Balanced" (validateBalancedValues inputDatums outputDatums vp)

{-# INLINEABLE validateBalancedDatums #-}
validateBalancedDatums :: [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] -> [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] -> Bool
validateBalancedDatums !inputs !outputs =
    let !inDatums = map snd inputs
        !outDatums = map snd outputs
        !inputSum_Avalaible_CampaignToken = sum $ map T.cfdSubtotal_Avalaible_CampaignToken inDatums
        !outputSum_Avalaible_CampaignToken = sum $ map T.cfdSubtotal_Avalaible_CampaignToken outDatums
        !inputSum_Avalaible_ADA = sum $ map T.cfdSubtotal_Avalaible_ADA inDatums
        !outputSum_Avalaible_ADA = sum $ map T.cfdSubtotal_Avalaible_ADA outDatums
        !validFieldChanges = and $ zipWith sameFieldsExceptAvailable inDatums outDatums
    in inputSum_Avalaible_CampaignToken == outputSum_Avalaible_CampaignToken
        && inputSum_Avalaible_ADA == outputSum_Avalaible_ADA
        && validFieldChanges

{-# INLINEABLE sameFieldsExceptAvailable #-}
sameFieldsExceptAvailable :: T.CampaignFundsDatumType -> T.CampaignFundsDatumType -> Bool
sameFieldsExceptAvailable !input !output =
    T.cfdIndex input == T.cfdIndex output
        && T.cfdCampaignPolicy_CS input == T.cfdCampaignPolicy_CS output
        && T.cfdCampaignFundsPolicyID_CS input == T.cfdCampaignFundsPolicyID_CS output
        && T.cfdSubtotal_Sold_CampaignToken input == T.cfdSubtotal_Sold_CampaignToken output
        && T.cfdSubtotal_Collected_ADA input == T.cfdSubtotal_Collected_ADA output
        && T.cfdMinADA input == T.cfdMinADA output

{-# INLINEABLE validateBalancedValues #-}
validateBalancedValues :: [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] -> [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)] -> ValidationParams -> Bool
validateBalancedValues !inputs !outputs !vp =
    and $ zipWith (validateValueMatch vp) inputs outputs

{-# INLINEABLE validateValueMatch #-}
validateValueMatch :: ValidationParams -> (LedgerContextsV2.TxOut, T.CampaignFundsDatumType) -> (LedgerContextsV2.TxOut, T.CampaignFundsDatumType) -> Bool
validateValueMatch !vp (!inTxOut, !_) (!outTxOut, !outDatum) =
    let !campaignToken_AC = LedgerValue.AssetClass (CampaignT.cdCampaignToken_CS $ vCampaignDatum vp, CampaignT.cdCampaignToken_TN $ vCampaignDatum vp)
        !inCampaignToken_Amount = OnChainHelpers.getAmt_With_AC_InValue (LedgerApiV2.txOutValue inTxOut) campaignToken_AC
        !outCampaignToken_Amount = OnChainHelpers.getAmt_With_AC_InValue (LedgerApiV2.txOutValue outTxOut) campaignToken_AC
        !inADA_Amount = OnChainHelpers.getADAfromValue (LedgerApiV2.txOutValue inTxOut)
        !outADA_Amount = OnChainHelpers.getADAfromValue (LedgerApiV2.txOutValue outTxOut)
        !inCampaignToken_Value = LedgerValue.assetClassValue campaignToken_AC inCampaignToken_Amount
        !outCampaignToken_Value = LedgerValue.assetClassValue campaignToken_AC outCampaignToken_Amount
        !inRest = LedgerApiV2.txOutValue inTxOut
            <> negate inCampaignToken_Value
            <> negate (OnChainHelpers.createADAValue inADA_Amount)
        !outRest = LedgerApiV2.txOutValue outTxOut
            <> negate outCampaignToken_Value
            <> negate (OnChainHelpers.createADAValue outADA_Amount)
    in outCampaignToken_Amount == T.cfdSubtotal_Avalaible_CampaignToken outDatum
        && outADA_Amount == (T.cfdSubtotal_Avalaible_ADA outDatum + T.cfdMinADA outDatum)
        && inRest == outRest

--------------------------------------------------------------------------------
-- Merge Implementation
--------------------------------------------------------------------------------

{-# INLINEABLE validateMerge #-}
validateMerge :: ValidationParams -> T.CampaignFundsDatumType -> Ledger.Value -> Bool
validateMerge !vp !outDatum !outValue =
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
    let !inputTxOutsAndDatums = getInputCampaignFundsDatums vp
        !inputDatums = map snd inputTxOutsAndDatums
        !inputValues = map (LedgerApiV2.txOutValue . fst) inputTxOutsAndDatums
    in traceIfFalse "not isBurningCampaignFundsIDs2"
        ( OnChainHelpers.isToken_Burning_With_CS (vCampaignFundsPolicyID_CS vp) (vInfo vp))
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_SubTotal_Added"
            (validateMergeDatumUpdate outDatum inputDatums)
        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value_Added"
            (validateMergeValueChange vp outValue inputValues)

{-# INLINEABLE validateMergeDatumUpdate #-}
validateMergeDatumUpdate ::T.CampaignFundsDatumType -> [T.CampaignFundsDatumType] -> Bool
validateMergeDatumUpdate !outDatum !inputDatums =
    let !minADA = sum (map T.cfdMinADA inputDatums)
        !avalaible_CampaignToken = sum (map T.cfdSubtotal_Avalaible_CampaignToken inputDatums)
        !sold_CampaignToken = sum (map T.cfdSubtotal_Sold_CampaignToken inputDatums)
        !avalaible_ADA = sum (map T.cfdSubtotal_Avalaible_ADA inputDatums)
        !collected_ADA = sum (map T.cfdSubtotal_Collected_ADA inputDatums)
        !campaignDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_MergedSubtotals
            outDatum
            avalaible_CampaignToken
            sold_CampaignToken
            avalaible_ADA
            collected_ADA
            minADA
    in outDatum `OnChainHelpers.isUnsafeEqDatums` campaignDatum_Out_Control

{-# INLINEABLE validateMergeValueChange #-}
validateMergeValueChange :: ValidationParams -> Ledger.Value -> [Ledger.Value] -> Bool
validateMergeValueChange !vp !outValue !inputValues =
    let
        !totalValue = OnChainHelpers.sumValues inputValues
        !mintingValue = LedgerApiV2.txInfoMint (vInfo vp)
        !burningID = OnChainHelpers.getValueOfCurrencySymbol mintingValue (vCampaignFundsPolicyID_CS vp)
        !valueFor_Control = totalValue <> burningID
    in
        outValue `OnChainHelpers.isEqValue` valueFor_Control

--------------------------------------------------------------------------------
-- Delete Validation Functions
--------------------------------------------------------------------------------

{-# INLINEABLE validateDelete #-}
validateDelete :: ValidationParams -> Bool
validateDelete !vp =
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
    --TODO: agregar control, si la campaña está en estado alguno que sea activo, solo se borra si tiene zero subtotals y minADA only
    -- pero si esta todo terminado y es para limpiar, deberia permitir, con min ada only, pero sin imporatrn si collected o sold son distintos de zero
    ------------------
    traceIfFalse "not isBurningCampaignFundsIDs3" ( OnChainHelpers.isToken_Burning_With_CS (vCampaignFundsPolicyID_CS vp) (vInfo vp))
        && traceIfFalse "not isZeroSubtotals" (isZeroSubtotals $ vDatum vp)
        && traceIfFalse "not isMinADAAndIDOnlyValue" (vInputValue vp `OnChainHelpers.isEqValue` (LedgerAda.lovelaceValueOf (T.cfdMinADA $ vDatum vp) <> LedgerValue.assetClassValue (vCampaignFundsID_AC vp) 1))
    where
        isZeroSubtotals :: T.CampaignFundsDatumType -> Bool
        isZeroSubtotals !datum =
            T.cfdSubtotal_Avalaible_CampaignToken datum == 0
                && T.cfdSubtotal_Sold_CampaignToken datum == 0
                && T.cfdSubtotal_Avalaible_ADA datum == 0
                && T.cfdSubtotal_Collected_ADA datum == 0

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

{-# INLINEABLE getOwnInputs #-}
getOwnInputs :: LedgerApiV2.TxInfo -> LedgerApiV2.Address -> [LedgerContextsV2.TxOut]
getOwnInputs !info !add =
    [ LedgerApiV2.txInInfoResolved txInfoInput
    | txInfoInput <- LedgerApiV2.txInfoInputs info
    , let address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
    , OnChainHelpers.isScriptAddress address && address == add
    ]

{-# INLINEABLE getOwnOutputs #-}
getOwnOutputs :: LedgerApiV2.TxInfo -> LedgerApiV2.Address ->[LedgerContextsV2.TxOut]
getOwnOutputs !info !add =
    [ output
    | output <- LedgerApiV2.txInfoOutputs info
     , let address = LedgerApiV2.txOutAddress output
    , OnChainHelpers.isScriptAddress address && address == add
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
        _       -> Nothing

{-# INLINEABLE getCampaign_Datum #-}
getCampaign_Datum :: LedgerContextsV2.ScriptContext -> LedgerValue.AssetClass -> Bool -> CampaignT.CampaignDatumType
getCampaign_Datum !ctx !campaignID_AC !useNormalInput =
    let inputs = if useNormalInput
            then [(LedgerApiV2.txInInfoOutRef txIn, LedgerApiV2.txInInfoResolved txIn)
                | txIn <- LedgerApiV2.txInfoInputs $ LedgerContextsV2.scriptContextTxInfo ctx]
            else [(LedgerApiV2.txInInfoOutRef txIn, LedgerApiV2.txInInfoResolved txIn)
                | txIn <- LedgerApiV2.txInfoReferenceInputs $ LedgerContextsV2.scriptContextTxInfo ctx]
        campaignDatums = OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
            @CampaignT.ValidatorDatum
            @CampaignT.CampaignDatumType
            ctx
            inputs
            campaignID_AC
            CampaignT.getCampaign_DatumType
    in case campaignDatums of
        [(_, _, datum)] -> datum
        _ -> traceError $ if useNormalInput
             then "Expected exactly one Campaign normal input"
             else "Expected exactly one Campaign reference input"

{-# INLINEABLE getOutput_CampaignFunds_DatumAndValue #-}
getOutput_CampaignFunds_DatumAndValue :: ValidationParams -> Maybe (T.CampaignFundsDatumType, Ledger.Value)
getOutput_CampaignFunds_DatumAndValue !vp =
    case vOwnOutputs vp of
        [output] ->
            let !maybeTxOut = OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                    @T.ValidatorDatum
                    @T.CampaignFundsDatumType
                    (vCtx vp)
                    output
                    (vCampaignFundsID_AC vp)
                    (Just $ vOwnAddress vp)
                    T.getCampaignFunds_DatumType
            in case maybeTxOut of
                Just txOut -> Just (OnChainHelpers.getDatum_In_TxOut_And_Datum txOut, LedgerApiV2.txOutValue output)
                Nothing    -> Nothing
        _ -> Nothing

{-# INLINEABLE getInputCampaignFundsDatums #-}
getInputCampaignFundsDatums :: ValidationParams -> [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)]
getInputCampaignFundsDatums !vp =
    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
        @T.ValidatorDatum
        @T.CampaignFundsDatumType
        (vCtx vp)
        (vOwnInputs vp)
        (vCampaignFundsPolicyID_CS vp)
        T.getCampaignFunds_DatumType

{-# INLINEABLE getOutputCampaignFundsDatums #-}
getOutputCampaignFundsDatums :: ValidationParams -> [(LedgerContextsV2.TxOut, T.CampaignFundsDatumType)]
getOutputCampaignFundsDatums !vp =
    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
        @T.ValidatorDatum
        @T.CampaignFundsDatumType
        (vCtx vp)
        (vOwnOutputs vp)
        (vCampaignFundsPolicyID_CS vp)
        T.getCampaignFunds_DatumType


{-# INLINEABLE getCampaignToken_AC #-}
getCampaignToken_AC :: ValidationParams -> LedgerValue.AssetClass
getCampaignToken_AC !vp =
    LedgerValue.AssetClass
        (CampaignT.cdCampaignToken_CS $ vCampaignDatum vp,
         CampaignT.cdCampaignToken_TN $ vCampaignDatum vp)

{-# INLINEABLE isCampaignOpen #-}
isCampaignOpen :: ValidationParams -> Bool
isCampaignOpen !vp =
    let !datum = vCampaignDatum vp
        !info = vInfo vp
    in OnChainHelpers.isDateReached (CampaignT.cdBeginAt datum) info
       && not (OnChainHelpers.isDateReached (CampaignT.cdDeadline datum) info)

{-# INLINEABLE isCampaignFinish #-}
isCampaignFinish :: ValidationParams -> Bool
isCampaignFinish = not . isCampaignOpen

{-# INLINEABLE isCampaignStatus #-}
isCampaignStatus :: ValidationParams -> CampaignT.CapaignStatus -> Bool
isCampaignStatus !vp !status = CampaignT.cdStatus (vCampaignDatum vp) == status



--------------------------------------------------------------------------------

{-# INLINEABLE checkUnique #-}
checkUnique :: ValidationParams -> Bool
checkUnique !vp = T.vpProtocolPolicyID_CS (vParams vp) /= LedgerApiV2.adaSymbol

--------------------------------------------------------------------------------
-- Authorization Functions
--------------------------------------------------------------------------------

{-# INLINEABLE isAdminTokenPresent #-}
isAdminTokenPresent :: ValidationParams -> Bool
isAdminTokenPresent !vp =
    case LedgerApiV2.txInfoOutputs $ vInfo vp of
        []           -> False
        (output : _) ->
            let !tokenAdmin_AC = LedgerValue.AssetClass (T.getAdminToken_CS $ vCampaignDatum vp, T.tokenAdmin_TN)
            in OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC

{-# INLINEABLE validateCampaignAdminAction #-}
validateCampaignAdminAction :: ValidationParams -> Bool
validateCampaignAdminAction !vp =
    ------------------
    -- Que este el token de admin presente
    -- o Que sea Campaign Admin
    ------------------
    traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent"
        (OnChainHelpers.isSignedByAny (T.getAdmins $ vCampaignDatum vp) (vInfo vp) || isAdminTokenPresent vp)

{-# INLINEABLE validateProtocolOrCampaignAdmin #-}
validateProtocolOrCampaignAdmin :: ValidationParams -> Bool
validateProtocolOrCampaignAdmin !vp =
    ------------------
    -- Que este el token de admin presente
    -- o Que sea Campaign Admin
    -- o Que sea Protocol Admin si hay input ref protocol
    ------------------
    let !protocolDatum = getProtocol_Datum vp
        !admins = T.getAdmins (vCampaignDatum vp) ++ maybe [] T.getAdmins protocolDatum
    in traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent"
        (OnChainHelpers.isSignedByAny admins (vInfo vp) || isAdminTokenPresent vp)

----------------------------------------------------------------------------

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !campaignPolicy_CS !campaignFundsValidator_Hash) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && validateMintAndBurnIDRedeemers
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
        validateMintAndBurnIDRedeemers :: Bool
        validateMintAndBurnIDRedeemers =
            case redeemer of
                (T.PolicyRedeemerMintID _) ->
                    ------------------
                    -- it runs along with Campaign Validator (ValidatorRedeemerFundsAdd)
                    -- validateAdminAction
                    -- traceIfFalse "not isCorrect_Output_Campaign_Datum_With_CampaignFundsAdded" isCorrect_Output_Campaign_Datum_With_CampaignFundsAdded
                    -- && traceIfFalse "not isCorrect_Output_Campaign_Datum_Value_NotChanged" isCorrect_Output_Campaign_Datum_Value_NotChanged
                    -- && traceIfFalse "not isMintingCampaignFundsID" isMintingCampaignFundsID
                    -- && traceIfFalse "not isCampaignOpen" isCampaignOpen
                    ------------------
                    -- Que se consuma CampaignDatum con redeemer correcto (ValidatorRedeemerFundsAdd)
                    -- Para identificar el correcto CampaignDatum necesita la póliza Campaign ID que está en los parámetros de esta póliza.
                    -- Que se genere salida con nuevo CampaignFunds Datum en CampaignFunds Validator (CampaignFunds Validator está indicada en CampaignDatum)
                    -- Que el CampaignFunds Datum sea correcto
                    -- Que se mintee CampaignFunds ID con own póliza
                    -- Que el CampaignFunds Datum tenga el CampaignFunds ID
                    ------------------
                    traceIfFalse "not isMintingCampaignFundsID" isMintingCampaignFundsID
                        && traceIfFalse "not isCorrect_Redeemer_CampaignDatum" (isCorrect_Redeemer_CampaignDatum isCampaignValidatorRedeemerFundsAdd)
                        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum" isCorrect_Output_CampaignFunds_Datum
                        && traceIfFalse "not isCorrect_Output_CampaignFunds_Datum_Value" isCorrect_Output_CampaignFunds_Datum_Value
                    where
                        ------------------
                        !outputs_txOuts =
                            [ txOut | txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                            ]
                        ------------------
                        !_ =
                            if length outputs_txOuts < 2
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
                        !output_Own_TxOut_And_CampaignFundsDatum =
                            fromMaybe
                                (traceError "Expected CampaignFunds at output index 1")
                                ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                    @T.ValidatorDatum
                                    @T.CampaignFundsDatumType
                                    ctx
                                    (outputs_txOuts !! 1)
                                    campaignFundsID_AC
                                    (Just campaignFundsValidator_Address)
                                    T.getCampaignFunds_DatumType
                                )
                        ------------------
                        !campaignFundsDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_CampaignFundsDatum
                        ------------------
                        !valueFor_Mint_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC 1
                        ------------------
                        !minADA_For_CampaignFundsDatum_Out = T.cfdMinADA campaignFundsDatum_Out
                        !value_MinADA_For_CampaignFundsDatum_Out = LedgerAda.lovelaceValueOf minADA_For_CampaignFundsDatum_Out
                        !valueFor_CampaignFundsDatum_Out_Control = valueFor_Mint_CampaignFundsID <> value_MinADA_For_CampaignFundsDatum_Out
                        ------------------
                        !campaignFundsDatum_Out_Control =
                            T.mkCampaignFunds_DatumType
                                campaignFunds_Index
                                campaignPolicy_CS
                                campaignFundsPolicyID_CS
                                0
                                0
                                0
                                0
                                minADA_For_CampaignFundsDatum_Out
                        ------------------
                        isCampaignValidatorRedeemerFundsAdd :: CampaignT.ValidatorRedeemer -> Bool
                        isCampaignValidatorRedeemerFundsAdd redemeerToCheck = case redemeerToCheck of
                            CampaignT.ValidatorRedeemerFundsAdd _ -> True
                            _                                     -> False
                        ------------------
                        isMintingCampaignFundsID :: Bool
                        isMintingCampaignFundsID = ownMintingValue `OnChainHelpers.isEqValue` valueFor_Mint_CampaignFundsID
                        -----------------
                        isCorrect_Output_CampaignFunds_Datum :: Bool
                        isCorrect_Output_CampaignFunds_Datum =
                            campaignFundsDatum_Out `OnChainHelpers.isUnsafeEqDatums` campaignFundsDatum_Out_Control
                        ------------------
                        isCorrect_Output_CampaignFunds_Datum_Value :: Bool
                        isCorrect_Output_CampaignFunds_Datum_Value =
                            let
                                !valueOf_CampaignFundsDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_CampaignFundsDatum
                            in
                                valueOf_CampaignFundsDatum_Out `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_Out_Control
                ------------------
                (T.PolicyRedeemerBurnID _) ->
                    ------------------
                    -- 1 - it runs along Campaign Validator, one input, one output (ValidatorRedeemerFundsDelete or ValidatorRedeemerFundsMerge)
                    -- 2 - it runs along CampaignFunds ID Policy (PolicyRedeemerBurnID)
                    -- 3 - it runs along CampaignFunds Validator ,  many inputs-one output (ValidatorRedeemerMerge) or many inputs-zero outputs (ValidatorRedeemerDelete)
                    ------------------
                    -- 1 - Que sea Protocol or Campaig Admin
                    -- 1 y 3 - Que se quemen CampaignFundsIDs con la correcta póliza indicada en CampaignDatum o CampaignFunds Datum
                    -- 2 - Que se quemen CampaignFundsIDs con own póliza
                    -- 2 - Qque coincida exactamente el total minted con el quantity del redeemer de Campaign Validator (en caso de Merge quantity menos uno)
                    -- 2 - Que coincida exactamente el total minted con la cantidad de inputs de CampaignFunds Validator (en caso de Merge hay una input que no se quema)
                    -- 2 - que el redeemer de Campaign validator sea correcto
                    -- 2 - que el redeemer de todos los CampaignFunds Validator sea correcto
                    -- 1 - Que el CampaignDatum regrese a Campaign Validator (se hace automaticamente al buscar outputs en same address)
                    -- 1 - Que el CampaignDatum se actualiza con el CampaignFunds eliminados
                    -- 1 - Que el CampaignDatum value no cambie
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante regrese a CampaignFunds Validator (se hace automaticamente al buscar outputs en same address)
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga el value de todos acumulados
                    -- 3 - En el caso de Merge: Que el CampaignFundsDatum resultante tenga en el datum los subtotales acumulados
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum no tengan tokens en value
                    -- 3 - En el caso de Delete: que los CampaignFundsDatum tengan zero subtotales
                    -- no hay restricciones temporales
                    ------------------
                    traceIfFalse "not isBurningCampaignFundsIDs4" isBurningCampaignFundsIDs
                        && traceIfFalse "not isCorrect_Redeemer_CampaignDatum" (isCorrect_Redeemer_CampaignDatum isCampaignValidatorRedeemerFundsMergeOrDelete )
                        -- && traceIfFalse "not isZeroAssets" isZeroAssets
                    where
                        ------------------
                        -- !input_TxOutRef_TxOut_And_CampaignFundsDatum =
                        --     case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                        --         @T.ValidatorDatum
                        --         @T.CampaignFundsDatumType
                        --         ctx
                        --         inputs_TxOutRefs_TxOuts
                        --         campaignFundsPolicyID_CS
                        --         T.getCampaignFunds_DatumType of
                        --         [x] -> x
                        --         _   -> traceError "Expected exactly one CampaignFunds input"
                        -- ------------------
                        -- !input_TxOut_And_CampaignFundsDatum = (\(_, txOut, datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_CampaignFundsDatum
                        -- ------------------
                        -- -- !campaignFundsDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_CampaignFundsDatum
                        -- -- ------------------
                        -- -- !campaignFunds_Index = T.cfdIndex campaignFundsDatum_In
                        -- -- !campaignFundsID_TN = LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS campaignFunds_Index
                        -- -- !campaignFundsID_AC = LedgerValue.AssetClass (campaignFundsPolicyID_CS, campaignFundsID_TN)
                        -- -- ------------------
                        -- -- !valueFor_Mint_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC 1
                        -- !valueFor_Burn_CampaignFundsID = LedgerValue.assetClassValue campaignFundsID_AC (negate 1)
                        ------------------
                        isBurningCampaignFundsIDs :: Bool
                        isBurningCampaignFundsIDs = OnChainHelpers.isToken_Burning_With_CS campaignFundsPolicyID_CS info && ownMintingValue `OnChainHelpers.isEqValue` OnChainHelpers.getValueOfCurrencySymbol ownMintingValue campaignFundsPolicyID_CS
                        -----------------
                        isCampaignValidatorRedeemerFundsMergeOrDelete :: CampaignT.ValidatorRedeemer -> Bool
                        isCampaignValidatorRedeemerFundsMergeOrDelete redemeerToCheck = case redemeerToCheck of
                            CampaignT.ValidatorRedeemerFundsDelete _ -> True
                            CampaignT.ValidatorRedeemerFundsMerge _  -> True
                            _                                        -> False
                        ------------------
                        -- isZeroAssets :: Bool
                        -- !isZeroAssets =
                        --     let
                        --         ------------------
                        --         !minADA_For_CampaignFundsDatum_In = T.cfdMinADA campaignFundsDatum_In
                        --         !value_MinADA_For_CampaignFundsDatum_In = LedgerAda.lovelaceValueOf minADA_For_CampaignFundsDatum_In
                        --         !valueFor_CampaignFundsDatum_In_Control = valueFor_Mint_CampaignFundsID <> value_MinADA_For_CampaignFundsDatum_In
                        --         ------------------
                        --         !valueOf_CampaignFundsDatum_In = OnChainHelpers.getValue_In_TxOut_And_Datum input_TxOut_And_CampaignFundsDatum
                        --     in
                        --         valueOf_CampaignFundsDatum_In `OnChainHelpers.isEqValue` valueFor_CampaignFundsDatum_In_Control
            where
                ------------------
                !campaignID_AC = LedgerValue.AssetClass (campaignPolicy_CS, T.campaignID_TN)
                ------------------
                !inputs_TxOutRefs_TxOuts =
                    [ (LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | txInfoInput <- LedgerApiV2.txInfoInputs info, OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                    ]
                ------------------
                !input_TxOutRef_TxOut_And_CampaignDatum =
                    case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                        @CampaignT.ValidatorDatum
                        @CampaignT.CampaignDatumType
                        ctx
                        inputs_TxOutRefs_TxOuts
                        campaignID_AC
                        CampaignT.getCampaign_DatumType of
                        [x] -> x
                        _   -> traceError "Expected exactly one Campaign input"
                ------------------
                get_Redeemer_CampaignDatum :: Maybe CampaignT.ValidatorRedeemer
                get_Redeemer_CampaignDatum  =
                    let
                        !redeemerFor_CampaignDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_CampaignDatum) info
                    in
                        case redeemerFor_CampaignDatum' of
                            Nothing                        -> Nothing
                            Just redeemerFor_CampaignDatum -> LedgerApiV2.fromBuiltinData @CampaignT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_CampaignDatum
                ------------------
                isCorrect_Redeemer_CampaignDatum :: (CampaignT.ValidatorRedeemer -> Bool) -> Bool
                isCorrect_Redeemer_CampaignDatum isRedeemerType =
                    case get_Redeemer_CampaignDatum of
                                Just x -> isRedeemerType x
                                _      -> False

----------------------------------------------------------------------------2

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
mkWrappedPolicyID campaignPolicy_CS campaignFundsValidator_Hash = mkPolicyID params
    where
        params =
            T.PolicyParams
                { ppCampaignPolicy_CS = PlutusTx.unsafeFromBuiltinData campaignPolicy_CS
                , ppCampaignFundsValidator_Hash = PlutusTx.unsafeFromBuiltinData campaignFundsValidator_Hash
                }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = Plutonomy.optimizeUPLC $$(PlutusTx.compile [||mkWrappedPolicyID||])

----------------------------------------------------------------------------2

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

----------------------------------------------------------------------------2
