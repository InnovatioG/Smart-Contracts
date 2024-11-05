{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
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

module Protocol.Types where

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

import qualified Constants            as T
import qualified Helpers.Types        as T
import qualified Types                as T
import qualified Helpers.OnChain as OnChainHelpers

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams
    = PolicyParams { ppProtocolPolicyID_TxOutRef :: LedgerApiV2.TxOutRef }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINABLE (==) #-}
    pp1 == pp2 =
        ppProtocolPolicyID_TxOutRef pp1 == ppProtocolPolicyID_TxOutRef pp2

PlutusTx.makeLift ''PolicyParams

PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS          :: T.CS
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

data MinMaxDef a
    = MinMaxDef
          { mmdMin :: a
          , mmdMax :: a
          , mmdDef :: a
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance (Eq a) => Eq (MinMaxDef a) where
    {-# INLINABLE (==) #-}
    mi1 == mi2 =
        mmdMin mi1 == mmdMin mi2
            && mmdMax mi1 == mmdMax mi2
            && mmdDef mi1 == mmdDef mi2

-- Method to check the range
isInRange :: (Ord a) => MinMaxDef a -> a -> Bool
isInRange mmd value = mmdMin mmd <= value && value <= mmdMax mmd

isValidMinMaxDef :: (Ord a) => MinMaxDef a -> Bool
isValidMinMaxDef mmd = mmdMin mmd <= mmdMax mmd && mmdMin mmd <= mmdDef mmd && mmdDef mmd <= mmdMax mmd

instance DataOpenApiSchema.ToSchema (MinMaxDef Integer)

instance DataOpenApiSchema.ToSchema (MinMaxDef LedgerApiV2.POSIXTime)

PlutusTx.makeIsDataIndexed ''MinMaxDef [('MinMaxDef, 0)]

{-# INLINABLE mkMinMaxDef #-}
mkMinMaxDef :: a -> a -> a -> MinMaxDef a
mkMinMaxDef min' max' def' =
    MinMaxDef {mmdMin = min', mmdMax = max', mmdDef = def'}

--------------------------------------------------------------------------------2

data ProtocolDatumType
    = ProtocolDatumType
          { pdProtocolVersion     :: Integer
          , pdAdmins              :: [T.WalletPaymentPKH]
          , pdTokenAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
          , pdMinADA              :: Integer
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ProtocolDatumType where
    {-# INLINABLE (==) #-}
    ps1 == ps2 =
        pdProtocolVersion ps1 == pdProtocolVersion ps2
            && pdAdmins ps1 == pdAdmins ps2
            && pdTokenAdminPolicy_CS ps1 == pdTokenAdminPolicy_CS ps2
            && pdMinADA ps1 == pdMinADA ps2

instance T.HasAdmins ProtocolDatumType where
    {-# INLINABLE getAdmins #-}
    getAdmins = pdAdmins

instance T.HasAdminToken ProtocolDatumType where
    {-# INLINABLE getAdminToken_CS #-}
    getAdminToken_CS = pdTokenAdminPolicy_CS

PlutusTx.makeIsDataIndexed ''ProtocolDatumType [('ProtocolDatumType, 0)]

newtype ValidatorDatum
    = ProtocolDatum ProtocolDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINABLE (==) #-}
    ProtocolDatum mps1 == ProtocolDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ProtocolDatum, 0)]

{-# INLINABLE getProtocol_DatumType #-}
getProtocol_DatumType :: ValidatorDatum -> ProtocolDatumType
getProtocol_DatumType (ProtocolDatum sdType) = sdType

{-# INLINEABLE getProtocol_DatumType_From_UTxO #-}
getProtocol_DatumType_From_UTxO :: LedgerApiV2.TxOut -> ProtocolDatumType
getProtocol_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
                    Nothing     -> P.error "No Protocol Datum found"
                    Just datum' -> getProtocol_DatumType datum'
                    
instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor =
        case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
            Nothing -> Nothing
            Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2
{-# INLINABLE mkProtocol_DatumType #-}
mkProtocol_DatumType ::
    [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Integer
    -> ProtocolDatumType
mkProtocol_DatumType
    admins
    tokenAdminPolicy_CS
    minADA =
        let !adminsOrdered = sort admins
        in ProtocolDatumType
                { pdProtocolVersion = T.protocolVersion
                , pdAdmins = adminsOrdered
                , pdTokenAdminPolicy_CS = tokenAdminPolicy_CS
                , pdMinADA = minADA
                }

{-# INLINABLE mkProtocol_Datum #-}
mkProtocol_Datum ::
        [T.WalletPaymentPKH]
    -> LedgerApiV2.CurrencySymbol
    -> Integer
    -> ValidatorDatum
mkProtocol_Datum
    admins
    tokenAdminPolicy_CS
    minADA =
        ProtocolDatum
            $ mkProtocol_DatumType
                    admins
                    tokenAdminPolicy_CS
                    minADA

mkDatum :: ProtocolDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . ProtocolDatum

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDatumUpdateType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

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

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINABLE (==) #-}
    r1 == r2 =  r1 ==  r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2
data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINABLE (==) #-}
    (==) :: ValidatorRedeemer -> ValidatorRedeemer -> Bool
    ValidatorRedeemerDatumUpdate rmf1 == ValidatorRedeemerDatumUpdate rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 =
        rmf1 == rmf2
    ValidatorRedeemerEmergency rmf1 == ValidatorRedeemerEmergency rmf2 =
        rmf1 == rmf2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
        [ ('ValidatorRedeemerDatumUpdate, 0)
        , ('ValidatorRedeemerUpdateMinADA, 1)
        , ('ValidatorRedeemerEmergency, 2)
        ]

--------------------------------------------------------------------------------2


getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType))   = Just "DatumUpdate"
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType))       = Just "Emergency"
getValidatorRedeemerName _                                                                        = Nothing

--------------------------------------------------------------------------------2

mkMintIDRedeemer :: LedgerApiV2.Redeemer
mkMintIDRedeemer = LedgerApiV2.Redeemer $ LedgerApiV2.toBuiltinData (0 :: Integer)

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

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

--------------------------------------------------------------------------------2
