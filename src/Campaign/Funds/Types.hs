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

module Campaign.Funds.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema  as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics         as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types        as T
import qualified Types       as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams
    = PolicyParams
          { ppCampaignPolicy_CS           :: LedgerApiV2.CurrencySymbol
          , ppCampaignFundsValidator_Hash :: LedgerApiV2.ValidatorHash
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        ppCampaignPolicy_CS pp1 == ppCampaignPolicy_CS pp2 &&
        ppCampaignFundsValidator_Hash pp1 == ppCampaignFundsValidator_Hash pp2


PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed
    ''PolicyParams
    [ ('PolicyParams, 0)
    ]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: LedgerApiV2.CurrencySymbol
          , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

-- instance Schema.ToSchema ValidatorParams where
--     toSchema = Schema.FormSchemaUnit

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2 &&
        vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed
    ''ValidatorParams
    [ ('ValidatorParams, 0)
    ]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data CampaignFundsDatumType
    = CampaignFundsDatumType
          { cfdIndex                    :: Integer
          , cfdCampaignPolicy_CS        :: T.CS
          , cfdCampaignFundsPolicyID_CS :: T.CS
          , cfdSubtotal_Avalaible_FT       :: Integer
          , cfdSubtotal_Sold_FT            :: Integer
          , cfdSubtotal_Avalaible_ADA            :: Integer
          , cfdSubtotal_Collected_ADA            :: Integer
          , cfdMinADA                   :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq CampaignFundsDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        cfdIndex ps1 == cfdIndex ps2
            && cfdCampaignPolicy_CS ps1 == cfdCampaignPolicy_CS ps2
            && cfdCampaignFundsPolicyID_CS ps1 == cfdCampaignFundsPolicyID_CS ps2
            && cfdSubtotal_Avalaible_FT ps1 == cfdSubtotal_Avalaible_FT ps2
            && cfdSubtotal_Sold_FT  ps1 == cfdSubtotal_Sold_FT  ps2
            && cfdSubtotal_Avalaible_ADA  ps1 == cfdSubtotal_Avalaible_ADA  ps2
            && cfdSubtotal_Collected_ADA  ps1 == cfdSubtotal_Collected_ADA  ps2
            && cfdMinADA ps1 == cfdMinADA ps2

instance Ord CampaignFundsDatumType where
    {-# INLINEABLE compare #-}
    compare :: CampaignFundsDatumType -> CampaignFundsDatumType -> Ordering
    compare a b = compare (cfdIndex a) (cfdIndex b)

PlutusTx.makeIsDataIndexed
    ''CampaignFundsDatumType
    [ ('CampaignFundsDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = CampaignFundsDatum CampaignFundsDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    CampaignFundsDatum mps1 == CampaignFundsDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('CampaignFundsDatum, 0)
    ]

{-# INLINEABLE getCampaignFundsDatumType #-}
getCampaignFundsDatumType :: ValidatorDatum -> CampaignFundsDatumType
getCampaignFundsDatumType (CampaignFundsDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkCampaignFundsDatumType #-}
mkCampaignFundsDatumType :: Integer -> T.CS -> T.CS -> Integer -> Integer -> Integer -> Integer -> Integer -> CampaignFundsDatumType
mkCampaignFundsDatumType = CampaignFundsDatumType

{-# INLINEABLE mkCampaignFundsDatum #-}
mkCampaignFundsDatum :: Integer ->  T.CS -> T.CS -> Integer -> Integer -> Integer -> Integer -> Integer ->  ValidatorDatum
mkCampaignFundsDatum
    index
    campaignPolicy_CS
    campaignFundsPolicyID_CS
    subtotal_Avalaible_FT
    subtotal_Sold_FT
    subtotal_Avalaible_ADA 
    subtotal_Collected_ADA 
    minADA
     =
        CampaignFundsDatum $
            mkCampaignFundsDatumType
                index
                campaignPolicy_CS
                campaignFundsPolicyID_CS
                subtotal_Avalaible_FT
                subtotal_Sold_FT
                subtotal_Avalaible_ADA 
                subtotal_Collected_ADA 
                minADA

mkDatum :: CampaignFundsDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . CampaignFundsDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintIDType [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [ ('PolicyRedeemerBurnIDType, 0)
    ]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _                                                   = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    ]

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDepositType
    = ValidatorRedeemerDepositType
          { rdDate   :: LedgerApiV2.POSIXTime
          , rdAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rdDate r1 == rdDate r2
            && rdAmount r1 == rdAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDepositType
    [ ('ValidatorRedeemerDepositType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerWithdrawType
    = ValidatorRedeemerWithdrawType
          { rwDate   :: LedgerApiV2.POSIXTime
          , rwAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwDate r1 == rwDate r2
            && rwAmount r1 == rwAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerWithdrawType
    [ ('ValidatorRedeemerWithdrawType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerSellType
    = ValidatorRedeemerSellType
          { rcpcDate   :: LedgerApiV2.POSIXTime
          , rcpcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerSellType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcpcDate r1 == rcpcDate r2 && rcpcAmount r1 == rcpcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerSellType
    [ ('ValidatorRedeemerSellType, 0)
    ]

--------------------------------------------------------------------------------2
data ValidatorRedeemerGetBackType
    = ValidatorRedeemerGetBackType
          { rcmcDate   :: LedgerApiV2.POSIXTime
          , rcmcAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerGetBackType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcmcDate r1 == rcmcDate r2 && rcmcAmount r1 == rcmcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerGetBackType
    [ ('ValidatorRedeemerGetBackType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollectType
    = ValidatorRedeemerCollectType
          { rcacDate   :: LedgerApiV2.POSIXTime
          , rcacAmount :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollectType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcacDate r1 == rcacDate r2 && rcacAmount r1 == rcacAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollectType
    [ ('ValidatorRedeemerCollectType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [ ('ValidatorRedeemerDeleteType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerBalanceAssetsType = ValidatorRedeemerBalanceAssetsType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerBalanceAssetsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerBalanceAssetsType
    [ ('ValidatorRedeemerBalanceAssetsType, 0)
    ]

--------------------------------------------------------------------------------2
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerSell ValidatorRedeemerSellType
    | ValidatorRedeemerGetBack ValidatorRedeemerGetBackType
    | ValidatorRedeemerCollect ValidatorRedeemerCollectType
    | ValidatorRedeemerMerge ValidatorRedeemerDeleteType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2     = rmf1 == rmf2
    ValidatorRedeemerDeposit rmf1 == ValidatorRedeemerDeposit rmf2               = rmf1 == rmf2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2           = rmcp1 == rmcp2
    ValidatorRedeemerSell rmcp1 == ValidatorRedeemerSell rmcp2                   = rmcp1 == rmcp2
    ValidatorRedeemerGetBack rmcp1 == ValidatorRedeemerGetBack rmcp2             = rmcp1 == rmcp2
    ValidatorRedeemerCollect rmcp1 == ValidatorRedeemerCollect rmcp2             = rmcp1 == rmcp2
    ValidatorRedeemerMerge rmcp1 == ValidatorRedeemerMerge rmcp2                 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2               = rmcp1 == rmcp2
    ValidatorRedeemerBalanceAssets rmcp1 == ValidatorRedeemerBalanceAssets rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2         = rmcp1 == rmcp2
    _ == _                                                                       = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateMinADA, 0)
    , ('ValidatorRedeemerDeposit, 1)
    , ('ValidatorRedeemerWithdraw, 2)
    , ('ValidatorRedeemerSell, 3)
    , ('ValidatorRedeemerGetBack, 4)
    , ('ValidatorRedeemerCollect, 5)
    , ('ValidatorRedeemerMerge, 6)
    , ('ValidatorRedeemerDelete, 7)
    , ('ValidatorRedeemerBalanceAssets, 8)
    , ('ValidatorRedeemerEmergency, 9)
    ]

--------------------------------------------------------------------------------2

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

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =

    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkDepositRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkDepositRedeemer date' deposit' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDeposit $
                ValidatorRedeemerDepositType date' deposit'

mkWithdrawRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkWithdrawRedeemer date' withdraw' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerWithdraw $
                ValidatorRedeemerWithdrawType date' withdraw'

mkSellRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkSellRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerSell $
                ValidatorRedeemerSellType date' amount'

mkGetBackRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkGetBackRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerGetBack $
                ValidatorRedeemerGetBackType date' amount'

mkCollectRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkCollectRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerCollect $
                ValidatorRedeemerCollectType date' amount'

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType


mkMergeRedeemer :: LedgerApiV2.Redeemer
mkMergeRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerMerge ValidatorRedeemerDeleteType

mkBalanceAssetsRedeemer :: LedgerApiV2.Redeemer
mkBalanceAssetsRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

--------------------------------------------------------------------------------2
