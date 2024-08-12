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

import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import           PlutusTx.Prelude            hiding (unless)
import qualified PlutusTx.Ratio              as TxRatio

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.OnChain      as OnChainHelpers
import qualified Helpers.Types               as T
import qualified Campaign.Funds.Types as CampaignFundsT
import qualified Campaign.Types         as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_NormalChanges #-}
mkUpdated_CampaignDatum_With_NormalChanges :: T.CampaignDatumType -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_NormalChanges !campaignDatum_In !admins !tokenAdminPolicy_CS =
    campaignDatum_In { T.cdAdmins = admins, T.cdTokenAdminPolicy_CS = tokenAdminPolicy_CS }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_NewMinADA #-}
mkUpdated_CampaignDatum_With_NewMinADA :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_NewMinADA !campaignDatum_In !newMinADA = campaignDatum_In { T.cdMinADA = newMinADA }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_CampaignFundsAdded #-}
mkUpdated_CampaignDatum_With_CampaignFundsAdded :: T.CampaignDatumType -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_CampaignFundsAdded !campaignDatum_In =
    campaignDatum_In { T.cdFundsCount = T.cdFundsCount campaignDatum_In + 1, T.cdFundsIndex = T.cdFundsIndex campaignDatum_In + 1 }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_CampaignFundsDeleted #-}
mkUpdated_CampaignDatum_With_CampaignFundsDeleted :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_CampaignFundsDeleted !campaignDatum_In quantity =
    campaignDatum_In { T.cdFundsCount = T.cdFundsCount campaignDatum_In - quantity }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_NewStatus #-}
mkUpdated_CampaignDatum_With_NewStatus :: T.CampaignDatumType -> T.CapaignStatus -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_NewStatus !campaignDatum_In !status = campaignDatum_In { T.cdStatus = status }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_CampaignFundsCollected #-}
mkUpdated_CampaignDatum_With_CampaignFundsCollected :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_CampaignFundsCollected !campaignDatum_In !amount =
    campaignDatum_In { T.cdCollectedADA = T.cdCollectedADA campaignDatum_In + amount}

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignDatum_With_NewStatusReached #-}
mkUpdated_CampaignDatum_With_NewStatusReached :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_NewStatusReached !campaignDatum_In !fundedADA = campaignDatum_In { T.cdStatus = T.CsReached, T.cdFundedADA = fundedADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignDatum_With_NewStatusNotReached #-}
mkUpdated_CampaignDatum_With_NewStatusNotReached :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_NewStatusNotReached !campaignDatum_In !fundedADA = campaignDatum_In { T.cdStatus = T.CsNotReached, T.cdFundedADA = fundedADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignDatum_With_MilestoneAprobed #-}
mkUpdated_CampaignDatum_With_MilestoneAprobed :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_MilestoneAprobed !campaignDatum_In milestoneIndex =
    let
        indices = OnChainHelpers.enumFromTo 0 (length (T.cdMilestones campaignDatum_In) - 1)
        updatedMilestones = zipWith (curry updateMilestone) indices (T.cdMilestones campaignDatum_In)
        updateMilestone (i, milestone) = if i == milestoneIndex
                                         then milestone { T.cmStatus = T.MsSuccess }
                                         else milestone
    in campaignDatum_In { T.cdMilestones = updatedMilestones }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignDatum_With_MilestoneReprobed #-}
mkUpdated_CampaignDatum_With_MilestoneReprobed :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_MilestoneReprobed !campaignDatum_In milestoneIndex =
    let 
        indices = OnChainHelpers.enumFromTo 0 (length (T.cdMilestones campaignDatum_In) - 1)
        updatedMilestones = zipWith (curry updateMilestone) indices (T.cdMilestones campaignDatum_In)
        updateMilestone (i, milestone) = if i == milestoneIndex 
                                         then milestone { T.cmStatus = T.MsFailed } 
                                         else milestone
    in campaignDatum_In { T.cdMilestones = updatedMilestones, T.cdStatus = T.CsFailedMilestone }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignDatum_With_Collect #-}
mkUpdated_CampaignDatum_With_Collect :: T.CampaignDatumType -> Integer -> T.CampaignDatumType
mkUpdated_CampaignDatum_With_Collect !campaignDatum_In !amount =
    campaignDatum_In {
        T.cdCollectedADA = T.cdCollectedADA campaignDatum_In + amount
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_MergedSubtotals #-}
mkUpdated_CampaignFundsDatum_With_MergedSubtotals :: CampaignFundsT.CampaignFundsDatumType -> Integer ->  Integer -> Integer ->  Integer ->  Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_MergedSubtotals !campaignFundsDatum_Out !avalaible_FT !sold_FT !avalaible_ADA !collected_ADA !minADA  =
    campaignFundsDatum_Out {
        CampaignFundsT.cfdSubtotal_Avalaible_FT = avalaible_FT ,
        CampaignFundsT.cfdSubtotal_Sold_FT = sold_FT,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = avalaible_ADA ,
        CampaignFundsT.cfdSubtotal_Collected_ADA = collected_ADA ,
        CampaignFundsT.cfdMinADA = minADA}

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_NewMinADA #-}
mkUpdated_CampaignFundsDatum_With_NewMinADA :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_NewMinADA !campaignFundsDatum_In !newMinADA = campaignFundsDatum_In { CampaignFundsT.cfdMinADA = newMinADA }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_Deposits #-}
mkUpdated_CampaignFundsDatum_With_Deposits :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_Deposits !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_FT = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In + amount
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_Withdraw #-}
mkUpdated_CampaignFundsDatum_With_Withdraw :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_Withdraw !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_FT = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In - amount
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_SoldTokens #-}
mkUpdated_CampaignFundsDatum_With_SoldTokens :: CampaignFundsT.CampaignFundsDatumType -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_SoldTokens !campaignFundsDatum_In !amount !totalADA =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Sold_FT = CampaignFundsT.cfdSubtotal_Sold_FT campaignFundsDatum_In + amount,
        CampaignFundsT.cfdSubtotal_Avalaible_FT = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In - amount ,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In + totalADA
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_GetBackTokens #-}
mkUpdated_CampaignFundsDatum_With_GetBackTokens :: CampaignFundsT.CampaignFundsDatumType -> Integer -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_GetBackTokens !campaignFundsDatum_In !amount !totalADA =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Sold_FT = CampaignFundsT.cfdSubtotal_Sold_FT campaignFundsDatum_In - amount,
        CampaignFundsT.cfdSubtotal_Avalaible_FT = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In + amount ,
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_FT campaignFundsDatum_In - totalADA
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_CampaignFundsDatum_With_Collect #-}
mkUpdated_CampaignFundsDatum_With_Collect :: CampaignFundsT.CampaignFundsDatumType -> Integer -> CampaignFundsT.CampaignFundsDatumType
mkUpdated_CampaignFundsDatum_With_Collect !campaignFundsDatum_In !amount =
    campaignFundsDatum_In {
        CampaignFundsT.cfdSubtotal_Avalaible_ADA = CampaignFundsT.cfdSubtotal_Avalaible_ADA campaignFundsDatum_In - amount
        , CampaignFundsT.cfdSubtotal_Collected_ADA = CampaignFundsT.cfdSubtotal_Collected_ADA campaignFundsDatum_In + amount
        }

--------------------------------------------------------------------------------2
