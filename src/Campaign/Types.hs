{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Campaign.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Constants            as T
import qualified Helpers.Types        as T
import qualified Types                as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams
    = PolicyParams
          { ppProtocolPolicyID_CS     :: LedgerApiV2.CurrencySymbol
          , ppCampaignPolicy_TxOutRef :: LedgerApiV2.TxOutRef
          , ppCampaignValidator_Hash  :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_CS pp1 == ppProtocolPolicyID_CS pp2
            && ppCampaignPolicy_TxOutRef pp1 == ppCampaignPolicy_TxOutRef pp2
            && ppCampaignValidator_Hash pp1 == ppCampaignValidator_Hash pp2

PlutusTx.makeLift ''PolicyParams

PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: LedgerApiV2.CurrencySymbol
          , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

-- instance Schema.ToSchema ValidatorParams where
--     toSchema = Schema.FormSchemaUnit

instance Eq ValidatorParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
        && vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams

PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data CapaignStatus = CsCreated | CsInitialized | CsReached | CsNotReached | CsFailedMilestone deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq CapaignStatus where
    {-# INLINABLE (==) #-}
    (==) :: CapaignStatus -> CapaignStatus -> Bool
    CsCreated == CsCreated                 = True
    CsInitialized == CsInitialized         = True
    CsReached == CsReached                 = True
    CsNotReached == CsNotReached           = True
    CsFailedMilestone == CsFailedMilestone = True
    _ == _                                 = False

PlutusTx.makeIsDataIndexed ''CapaignStatus [('CsCreated, 1), ('CsInitialized, 2), ('CsReached, 3), ('CsNotReached, 4), ('CsFailedMilestone, 5)]

data MilestoneStatus = MsCreated | MsSuccess | MsFailed deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq MilestoneStatus where
    {-# INLINABLE (==) #-}
    (==) :: MilestoneStatus -> MilestoneStatus -> Bool
    MsCreated == MsCreated = True
    MsSuccess == MsSuccess = True
    MsFailed == MsFailed   = True
    _ == _                 = False

PlutusTx.makeIsDataIndexed ''MilestoneStatus [('MsCreated, 1), ('MsSuccess, 2), ('MsFailed, 3)]

data CampaignMilestones
    = CampaignMilestones
          { cmEstimatedDeliveryDate :: LedgerApiV2.POSIXTime
          , cmPerncentage           :: Integer
          , cmStatus                :: MilestoneStatus
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq CampaignMilestones where
  {-# INLINABLE (==) #-}
  (==) :: CampaignMilestones -> CampaignMilestones -> Bool
  cm1 == cm2 =
    cmEstimatedDeliveryDate cm1 == cmEstimatedDeliveryDate cm2
        && cmPerncentage cm1 == cmPerncentage cm2
        && cmStatus cm1 == cmStatus cm2

PlutusTx.makeIsDataIndexed ''CampaignMilestones [('CampaignMilestones, 0)]

data CampaignDatumType
    = CampaignDatumType
          { cdCampaignVersion          :: Integer
          , cdCampaignPolicy_CS        :: T.CS
          , cdCampaignFundsPolicyID_CS :: T.CS
          , cdAdmins                   :: [T.WalletPaymentPKH]
          , cdTokenAdminPolicy_CS      :: LedgerApiV2.CurrencySymbol
          , cdMint_CampaignFT          :: Bool
          , cdCampaignFT_CS            :: T.CS
          , cdCampaignFT_TN            :: T.TN
          , cdCampaignFT_PriceADA      :: Integer
          , cdRequestedMaxADA          :: Integer
          , cdRequestedMinADA          :: Integer
          , cdFundedADA                :: Integer
          , cdCollectedADA             :: Integer
          , cdBeginAt                  :: LedgerApiV2.POSIXTime
          , cdDeadline                 :: LedgerApiV2.POSIXTime
          , cdStatus                   :: CapaignStatus
          , cdMilestones               :: [CampaignMilestones]
          , cdFundsCount               :: Integer
          , cdFundsIndex               :: Integer
          , cdMinADA                   :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq CampaignDatumType where
  {-# INLINABLE (==) #-}
  ps1 == ps2 =
    cdCampaignVersion ps1 == cdCampaignVersion ps2
        && cdCampaignPolicy_CS ps1 == cdCampaignPolicy_CS ps2
        && cdCampaignFundsPolicyID_CS ps1 == cdCampaignFundsPolicyID_CS ps2
        && cdAdmins ps1 == cdAdmins ps2
        && cdTokenAdminPolicy_CS ps1 == cdTokenAdminPolicy_CS ps2
        && cdMint_CampaignFT ps1 == cdMint_CampaignFT ps2
        && cdCampaignFT_CS ps1 == cdCampaignFT_CS ps2
        && cdCampaignFT_TN ps1 == cdCampaignFT_TN ps2
        && cdCampaignFT_PriceADA ps1 == cdCampaignFT_PriceADA ps2
        && cdRequestedMaxADA ps1 == cdRequestedMaxADA ps2
        && cdRequestedMinADA ps1 == cdRequestedMinADA ps2
        && cdFundedADA ps1 == cdFundedADA ps2
        && cdCollectedADA ps1 == cdCollectedADA ps2
        && cdBeginAt ps1 == cdBeginAt ps2
        && cdDeadline ps1 == cdDeadline ps2
        && cdStatus ps1 == cdStatus ps2
        && cdMilestones ps1 == cdMilestones ps2
        && cdFundsCount ps1 == cdFundsCount ps2
        && cdFundsIndex ps1 == cdFundsIndex ps2
        && cdMinADA ps1 == cdMinADA ps2

instance T.HasAdmins CampaignDatumType where
    {-# INLINABLE getAdmins #-}
    getAdmins = cdAdmins

instance T.HasAdminToken CampaignDatumType where
    {-# INLINABLE getAdminToken_CS #-}
    getAdminToken_CS = cdTokenAdminPolicy_CS

PlutusTx.makeIsDataIndexed ''CampaignDatumType [('CampaignDatumType, 0)]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = CampaignDatum CampaignDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    CampaignDatum mps1 == CampaignDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('CampaignDatum, 0)]

{-# INLINABLE getCampaignDatumType #-}
getCampaignDatumType :: ValidatorDatum -> CampaignDatumType
getCampaignDatumType (CampaignDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
  showCborAsDatumType cbor =
    case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINABLE mkCampaignDatumType #-}
mkCampaignDatumType ::
        T.CS
    -> T.CS
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Bool
    -> T.CS
    -> T.TN
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> LedgerApiV2.POSIXTime
    -> CapaignStatus
    -> [CampaignMilestones]
    -> Integer
    -> Integer
    -> Integer
    -> CampaignDatumType
mkCampaignDatumType
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
    minADA
    =
    let !adminsOrdered = sort admins
    in  CampaignDatumType
            {
            cdCampaignVersion = T.campaignVersion
            , cdCampaignPolicy_CS          =  campaignPolicy_CS
            , cdCampaignFundsPolicyID_CS   =  campaignFundsPolicyID_CS
            , cdAdmins                     =  adminsOrdered
            , cdTokenAdminPolicy_CS        =  tokenAdminPolicy_CS
            , cdMint_CampaignFT            =  mint_CampaignFT
            , cdCampaignFT_CS              =  campaignFT_CS
            , cdCampaignFT_TN              =  campaignFT_TN
            , cdCampaignFT_PriceADA        =  campaignFT_PriceADA
            , cdRequestedMaxADA            =  requestedMaxADA
            , cdRequestedMinADA            =  requestedMinADA
            , cdFundedADA                   =  fundedADA
            , cdCollectedADA                =  collectedADA
            , cdBeginAt                    =  beginAt
            , cdDeadline                   =  deadline
            , cdStatus                     =  status
            , cdMilestones                 =  milestones
            , cdFundsCount                 =  fundsCount
            , cdFundsIndex                 =  fundsIndex
            , cdMinADA                     =  minADA
            }

{-# INLINABLE mkFundDatum #-}
mkFundDatum ::
        T.CS
    -> T.CS
    -> [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Bool
    -> T.CS
    -> T.TN
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> Integer
    -> LedgerApiV2.POSIXTime
    -> LedgerApiV2.POSIXTime
    -> CapaignStatus
    -> [CampaignMilestones]
    -> Integer
    -> Integer
    -> Integer
    -> ValidatorDatum
mkFundDatum
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
    minADA =
  CampaignDatum
    $ mkCampaignDatumType
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
        minADA

mkDatum :: CampaignDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . CampaignDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]

data PolicyRedeemerMintFTType = PolicyRedeemerMintFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintFTType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintFTType
    [('PolicyRedeemerMintFTType, 0)]

data PolicyRedeemerBurnFTType = PolicyRedeemerBurnFTType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnFTType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnFTType
    [('PolicyRedeemerBurnFTType, 0)]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    | PolicyRedeemerMintFT PolicyRedeemerMintFTType
    | PolicyRedeemerBurnFT PolicyRedeemerBurnFTType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerMintFT rmtx1 == PolicyRedeemerMintFT rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnFT rmtx1 == PolicyRedeemerBurnFT rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    , ('PolicyRedeemerMintFT, 2)
    , ('PolicyRedeemerBurnFT, 3)
    ]

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2
data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDatumUpdateType
    [('ValidatorRedeemerDatumUpdateType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerUpdateMinADAType
    [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerFundsAddType = ValidatorRedeemerFundsAddType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundsAddType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundsAddType
    [('ValidatorRedeemerFundsAddType, 0)]

--------------------------------------------------------------------------------2
newtype ValidatorRedeemerFundsMergeType
    = ValidatorRedeemerFundsMergeType { rfmQuantity :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundsMergeType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundsMergeType
    [('ValidatorRedeemerFundsMergeType, 0)]

--------------------------------------------------------------------------------2
newtype ValidatorRedeemerFundsDeleteType
    = ValidatorRedeemerFundsDeleteType { rfdQuantity :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundsDeleteType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundsDeleteType
    [('ValidatorRedeemerFundsDeleteType, 0)]


--------------------------------------------------------------------------------2

data ValidatorRedeemerFundsCollectType
    = ValidatorRedeemerFundsCollectType
          { rfcDate   :: LedgerApiV2.POSIXTime
          , rfcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerFundsCollectType where
    {-# INLINABLE (==) #-}
    r1 == r2 =
        rfcDate r1 == rfcDate r2 && rfcAmount r1 == rfcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundsCollectType
    [('ValidatorRedeemerFundsCollectType, 0)]


--------------------------------------------------------------------------------2
data ValidatorRedeemerInitializeCampaignType = ValidatorRedeemerInitializeCampaignType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerInitializeCampaignType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerInitializeCampaignType
    [('ValidatorRedeemerInitializeCampaignType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerReachedCampaignType = ValidatorRedeemerReachedCampaignType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerReachedCampaignType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReachedCampaignType
    [('ValidatorRedeemerReachedCampaignType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerNotReachedCampaignType = ValidatorRedeemerNotReachedCampaignType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerNotReachedCampaignType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerNotReachedCampaignType
    [('ValidatorRedeemerNotReachedCampaignType, 0)]

--------------------------------------------------------------------------------222

newtype ValidatorRedeemerMilestoneAprobeType
    = ValidatorRedeemerMilestoneAprobeType { rmaMilestoneIndex :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerMilestoneAprobeType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerMilestoneAprobeType
    [('ValidatorRedeemerMilestoneAprobeType, 0)]
--------------------------------------------------------------------------------2
newtype ValidatorRedeemerMilestoneReprobeType
    = ValidatorRedeemerMilestoneReprobeType { rmrMilestoneIndex :: Integer }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerMilestoneReprobeType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerMilestoneReprobeType
    [('ValidatorRedeemerMilestoneReprobeType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [('ValidatorRedeemerDeleteType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerFundsAdd ValidatorRedeemerFundsAddType
    | ValidatorRedeemerFundsMerge ValidatorRedeemerFundsMergeType
    | ValidatorRedeemerFundsDelete ValidatorRedeemerFundsDeleteType
    | ValidatorRedeemerFundsCollect ValidatorRedeemerFundsCollectType
    | ValidatorRedeemerInitializeCampaign ValidatorRedeemerInitializeCampaignType
    | ValidatorRedeemerReachedCampaign ValidatorRedeemerReachedCampaignType
    | ValidatorRedeemerNotReachedCampaign ValidatorRedeemerNotReachedCampaignType
    | ValidatorRedeemerMilestoneAprobe ValidatorRedeemerMilestoneAprobeType
    | ValidatorRedeemerMilestoneReprobe ValidatorRedeemerMilestoneReprobeType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    (==) :: ValidatorRedeemer -> ValidatorRedeemer -> Bool
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerFundsAdd rmcp1 == ValidatorRedeemerFundsAdd rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerFundsMerge rmcp1 == ValidatorRedeemerFundsMerge rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerFundsDelete rmcp1 == ValidatorRedeemerFundsDelete rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerInitializeCampaign rmcp1 == ValidatorRedeemerInitializeCampaign rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerMilestoneAprobe rmcp1 == ValidatorRedeemerMilestoneAprobe rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerMilestoneReprobe rmcp1 == ValidatorRedeemerMilestoneReprobe rmcp2 =
        rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2                 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2 =
        rmcp1 == rmcp2
    _ == _ = False

PlutusTx.makeIsDataIndexed
  ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerFundsAdd, 2)
    , ('ValidatorRedeemerFundsMerge, 4)
    , ('ValidatorRedeemerFundsDelete, 3)
    , ('ValidatorRedeemerFundsCollect, 12)
    , ('ValidatorRedeemerInitializeCampaign, 9)
    , ('ValidatorRedeemerReachedCampaign, 10)
    , ('ValidatorRedeemerNotReachedCampaign, 11)
    , ('ValidatorRedeemerMilestoneAprobe, 5)
    , ('ValidatorRedeemerMilestoneReprobe, 6)
    , ('ValidatorRedeemerEmergency, 7)
    , ('ValidatorRedeemerDelete, 8)
    ]
--------------------------------------------------------------------------------2

mkMintFTRedeemer :: LedgerApiV2.Redeemer
mkMintFTRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerMintFT PolicyRedeemerMintFTType

mkBurnFTRedeemer :: LedgerApiV2.Redeemer
mkBurnFTRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerBurnFT PolicyRedeemerBurnFTType

mkMintIDRedeemer :: LedgerApiV2.Redeemer
mkMintIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerMintID PolicyRedeemerMintIDType

mkBurnIDRedeemer :: LedgerApiV2.Redeemer
mkBurnIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerBurnID PolicyRedeemerBurnIDType

--------------------------------------------------------------------------------2

mkDatumUpdateRedeemer :: LedgerApiV2.Redeemer
mkDatumUpdateRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkCampaignFundsAddRedeemer :: LedgerApiV2.Redeemer
mkCampaignFundsAddRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundsAdd ValidatorRedeemerFundsAddType

mkCampaignFundsMergeRedeemer :: Integer -> LedgerApiV2.Redeemer
mkCampaignFundsMergeRedeemer quantity =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundsMerge $ ValidatorRedeemerFundsMergeType quantity

mkCampaignFundsDeleteRedeemer :: Integer ->  LedgerApiV2.Redeemer
mkCampaignFundsDeleteRedeemer quantity =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundsDelete $ ValidatorRedeemerFundsDeleteType quantity

mkCampaignFundsCollectRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkCampaignFundsCollectRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerFundsCollect $ ValidatorRedeemerFundsCollectType date' amount'

mkInitializeCampaignRedeemer :: LedgerApiV2.Redeemer
mkInitializeCampaignRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerInitializeCampaign ValidatorRedeemerInitializeCampaignType

mkReachedCampaignRedeemer :: LedgerApiV2.Redeemer
mkReachedCampaignRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerReachedCampaign ValidatorRedeemerReachedCampaignType

mkNotReachedCampaignRedeemer :: LedgerApiV2.Redeemer
mkNotReachedCampaignRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerNotReachedCampaign ValidatorRedeemerNotReachedCampaignType

mkMilestoneAprobeRedeemer :: Integer -> LedgerApiV2.Redeemer
mkMilestoneAprobeRedeemer milestoneIndex =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerMilestoneAprobe $ ValidatorRedeemerMilestoneAprobeType milestoneIndex

mkMilestoneReprobeRedeemer :: Integer -> LedgerApiV2.Redeemer
mkMilestoneReprobeRedeemer milestoneIndex =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerMilestoneReprobe $ ValidatorRedeemerMilestoneReprobeType milestoneIndex

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

--------------------------------------------------------------------------------2
