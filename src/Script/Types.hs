{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
--------------------------------------------------------------------------------2

module Script.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson           as DataAeson
import qualified Data.OpenApi.Schema  as DataOpenApiSchema
import qualified GHC.Generics         as GHCGenerics
import qualified Ledger.Value         as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as P
import qualified Schema

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Helpers.Types        as T
import qualified Types                as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams
    = PolicyParams { ppProtocolPolicyID_CS :: T.CS }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 = ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams
    = ValidatorParams
          { vpProtocolPolicyID_CS :: T.CS
          , vpScriptPolicyID_CS   :: T.CS
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2  &&
        vpScriptPolicyID_CS p1 == vpScriptPolicyID_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data ScriptDatumType
    = ScriptDatumType
          { sdAdminPaymentPKH :: T.WalletPaymentPKH
          , sdAdminStakePKH   :: Maybe T.WalletPaymentPKH
          , sdScriptHash      :: BuiltinByteString
          }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ScriptDatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
            sdAdminPaymentPKH sd1 == sdAdminPaymentPKH sd2
            && sdAdminStakePKH sd1 == sdAdminStakePKH sd2
            && sdScriptHash sd1 == sdScriptHash sd2

PlutusTx.makeIsDataIndexed ''ScriptDatumType [('ScriptDatumType, 0)]

newtype ValidatorDatum
    = ScriptDatum ScriptDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    ScriptDatum sd1 == ScriptDatum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ScriptDatum, 0)]

{-# INLINEABLE getScriptDatumType #-}
getScriptDatumType :: ValidatorDatum -> ScriptDatumType
getScriptDatumType (ScriptDatum sdType) = sdType

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d  -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkScriptDatumType #-}
mkScriptDatumType ::T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> BuiltinByteString -> ScriptDatumType
mkScriptDatumType = ScriptDatumType

{-# INLINEABLE mkScriptDatum #-}
mkScriptDatum :: T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> BuiltinByteString -> ValidatorDatum
mkScriptDatum
    adminPaymentPKH
    adminStakePKH
    scriptHash =
        ScriptDatum $
            mkScriptDatumType
                adminPaymentPKH
                adminStakePKH
                scriptHash

mkDatum :: ScriptDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . ScriptDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerMintIDType

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        r1 == r2

PlutusTx.unstableMakeIsData ''PolicyRedeemerBurnIDType

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
    [ ('PolicyRedeemerMintID, 1),
      ('PolicyRedeemerBurnID, 2)
    ]

--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName _                                                      = Nothing


--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerDelete = ValidatorRedeemerDelete deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDelete where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDelete [('ValidatorRedeemerDelete, 0)]

type ValidatorRedeemer = ValidatorRedeemerDelete

------------------------------------------------------------------------------

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just ValidatorRedeemerDelete) = Just "Delete"
getValidatorRedeemerName _                              = Nothing

------------------------------------------------------------------------------

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

mkScriptDeleteRedeemer :: LedgerApiV2.Redeemer
mkScriptDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData
            ValidatorRedeemerDelete

--------------------------------------------------------------------------------2
