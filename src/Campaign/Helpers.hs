{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Campaign.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude     hiding (unless)

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Campaign.Funds.Types as CampaignFundsT
import qualified Campaign.Types       as T
import qualified Helpers.OnChain      as OnChainHelpers
import qualified Helpers.Types        as T
import qualified Constants as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_NormalChanges #-}
mkUpdated_Campaign_Datum_With_NormalChanges :: T.CampaignDatumType -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_NormalChanges !campaignDatum_In !admins !tokenAdminPolicy_CS =
    campaignDatum_In { T.cdAdmins = admins, T.cdTokenAdminPolicy_CS = tokenAdminPolicy_CS }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_MinADAChanged #-}
mkUpdated_Campaign_Datum_With_MinADAChanged :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_MinADAChanged !campaignDatum_In !newMinADA = campaignDatum_In { T.cdMinADA = newMinADA }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_CampaignFundsAdded #-}
mkUpdated_Campaign_Datum_With_CampaignFundsAdded :: T.CampaignDatumType -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_CampaignFundsAdded !campaignDatum_In =
    campaignDatum_In { T.cdFundsCount = T.cdFundsCount campaignDatum_In + 1, T.cdFundsIndex = T.cdFundsIndex campaignDatum_In + 1 }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_CampaignFundsDeleted #-}
mkUpdated_Campaign_Datum_With_CampaignFundsDeleted :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_CampaignFundsDeleted !campaignDatum_In quantity =
    campaignDatum_In { T.cdFundsCount = T.cdFundsCount campaignDatum_In - quantity }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_NewStatus #-}
mkUpdated_Campaign_Datum_With_NewStatus :: T.CampaignDatumType -> T.CapaignStatus -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_NewStatus !campaignDatum_In !status = campaignDatum_In { T.cdStatus = status }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Campaign_Datum_With_CampaignFundsCollected #-}
mkUpdated_Campaign_Datum_With_CampaignFundsCollected :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_CampaignFundsCollected !campaignDatum_In !amount =
    campaignDatum_In { T.cdCollectedADA = T.cdCollectedADA campaignDatum_In + amount}

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_Campaign_Datum_With_NewStatusReached #-}
mkUpdated_Campaign_Datum_With_NewStatusReached :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_NewStatusReached !campaignDatum_In !fundedADA = campaignDatum_In { T.cdStatus = T.CsReached, T.cdFundedADA = fundedADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_Campaign_Datum_With_NewStatusNotReached #-}
mkUpdated_Campaign_Datum_With_NewStatusNotReached :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_NewStatusNotReached !campaignDatum_In !fundedADA = campaignDatum_In { T.cdStatus = T.CsNotReached, T.cdFundedADA = fundedADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_Campaign_Datum_With_MilestoneApproved #-}
mkUpdated_Campaign_Datum_With_MilestoneApproved :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_MilestoneApproved !campaignDatum_In milestoneIndex =
    let
        updatedMilestones = updateMilestones 0 (T.cdMilestones campaignDatum_In)
        
        updateMilestones :: Integer -> [T.CampaignMilestones] -> [T.CampaignMilestones]
        updateMilestones _ [] = []
        updateMilestones i (m : ms)
            | i == milestoneIndex = m { T.cmStatus = T.MsSuccess } : updateMilestones (i + 1) ms
            | otherwise            = m : updateMilestones (i + 1) ms
    in campaignDatum_In { T.cdMilestones = updatedMilestones }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_Campaign_Datum_With_MilestoneFailed #-}
mkUpdated_Campaign_Datum_With_MilestoneFailed :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_MilestoneFailed !campaignDatum_In milestoneIndex =
    let
        updatedMilestones = updateMilestones 0 (T.cdMilestones campaignDatum_In)
        
        updateMilestones :: Integer -> [T.CampaignMilestones] -> [T.CampaignMilestones]
        updateMilestones _ [] = []
        updateMilestones i (m : ms)
            | i == milestoneIndex = m { T.cmStatus = T.MsFailed } : updateMilestones (i + 1) ms
            | otherwise            = m : updateMilestones (i + 1) ms
    in campaignDatum_In { T.cdMilestones = updatedMilestones, T.cdStatus = T.CsFailedMilestone }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_Campaign_Datum_With_Collect #-}
mkUpdated_Campaign_Datum_With_Collect :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_Campaign_Datum_With_Collect !campaignDatum_In !amount =
    campaignDatum_In {
        T.cdCollectedADA = T.cdCollectedADA campaignDatum_In + amount
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_MergedSubtotals #-}
mkUpdated_CampaignFunds_Datum_With_MergedSubtotals :: CampaignFundsT.CampaignFundsDatumType -> Integer ->  Integer -> Integer ->  Integer ->  Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_MergedSubtotals !campaignFundsDatum_Out !avalaible_CampaignToken !sold_CampaignToken !avalaible_ADA !collected_ADA !minADA  =
    campaignFundsDatum_Out {
        CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken = avalaible_CampaignToken ,
        CampaignFundsT.cfdSubtotal_Sold_CampaignToken = sold_CampaignToken,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = avalaible_ADA ,
        CampaignFundsT.cfdSubtotal_Collected_ADA = collected_ADA ,
        CampaignFundsT.cfdMinADA = minADA}

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_MinADAChanged #-}
mkUpdated_CampaignFunds_Datum_With_MinADAChanged :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_MinADAChanged !campaignFundsDatum_In !newMinADA = campaignFundsDatum_In { CampaignFundsT.cfdMinADA = newMinADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_Deposits #-}
mkUpdated_CampaignFunds_Datum_With_Deposits :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_Deposits !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken = CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken campaignFundsDatum_In + amount
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_Withdraw #-}
mkUpdated_CampaignFunds_Datum_With_Withdraw :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_Withdraw !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken = CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken campaignFundsDatum_In - amount
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_SoldTokens #-}
mkUpdated_CampaignFunds_Datum_With_SoldTokens :: CampaignFundsT.CampaignFundsDatumType -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_SoldTokens !campaignFundsDatum_In  !amount_CampaignToken !amount_ADA =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Sold_CampaignToken = CampaignFundsT.cfdSubtotal_Sold_CampaignToken campaignFundsDatum_In + amount_CampaignToken,
        CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken = CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken campaignFundsDatum_In - amount_CampaignToken ,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In + amount_ADA
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_GetBackTokens #-}
mkUpdated_CampaignFunds_Datum_With_GetBackTokens :: CampaignFundsT.CampaignFundsDatumType -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_GetBackTokens !campaignFundsDatum_In !amount_CampaignToken !amount_ADA =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Sold_CampaignToken = CampaignFundsT.cfdSubtotal_Sold_CampaignToken campaignFundsDatum_In - amount_CampaignToken,
        CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken = CampaignFundsT.cfdSubtotal_Avalaible_CampaignToken campaignFundsDatum_In + amount_CampaignToken ,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In - amount_ADA
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFunds_Datum_With_Collect #-}
mkUpdated_CampaignFunds_Datum_With_Collect :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFunds_Datum_With_Collect !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In - amount
        , CampaignFundsT.cfdSubtotal_Collected_ADA = CampaignFundsT.cfdSubtotal_Collected_ADA campaignFundsDatum_In + amount
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkCampaignFundsID_TN #-}
mkCampaignFundsID_TN :: Integer -> LedgerApiV2.TokenName
mkCampaignFundsID_TN index =
    LedgerApiV2.TokenName $ T.campaignFundsID_TN_basename <> OnChainHelpers.intToBBS index

--------------------------------------------------------------------------------

