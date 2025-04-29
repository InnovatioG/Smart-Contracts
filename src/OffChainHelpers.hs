
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module OffChainHelpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Campaign.Funds.Types as CampaignFundT
import qualified Campaign.Types       as CampaignT
import qualified Helpers.OffChain     as OffChainHelpers
import qualified Protocol.Types       as ProtocolT
import qualified Script.Types         as ScriptT

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

-- cabal repl
-- import OffChainHelpers


-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f030405ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff0000008000ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f040506ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff0000009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff00ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581c786139173b79832ace8b3a55c04d8e43586724fcd497cd286bf27345581c3afcf1924a5ee2cfe4c11c642dcdcf97ed7436648f431175ba3664a100008080d8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f019fd8799f01581ce1492c3e12463b0039b36513ce5f50e391cb6b7773905897f9ff74f3581c4371d5ba8cbd3e330f2417d2d04cf44b258d9cff207e90188c91c291581c3a888d65f16790950a72daee1f63aa05add6d268434107cfa5b67712ffff581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f19115c1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f1b0000040ad427de4a1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"
-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f1b0000040ad427de4a1a004c4b401a05f5e100ffd8799f1915b31a004c4b401a05f5e100ffffd8799f000000ff0000d8799f00001901bcffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ca44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581cf44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f011a004c4b401a05f5e100ffd8799f021a004c4b401a05f5e100ffd8799f19386b1a004c4b401a05f5e100ffd8799f1a2067346f1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"


-- readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f04581c3cb3175e23c6dd93e0d4695c840da618b32c36a48c0d60258f89fa05581c02dd31957912b5c33d5c1528610eea14995fba430f4d9fe16174e05a58200973b805951df6bf13832868c8846bd6bf753234bf33e988ad64f61619eec2031a000493e09f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff80409fd8799f001a3b9aca001b000000174876e800ffffd8799f1b00000001cf7c58001b00000016071384001b000000072957f000ffd8799f581cd0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06444d41595aff1a05f5e1001a05f5e100d8799f001a000f42401a0001d4c0ffd8799f001a0007a1201a000186a0ffd8799f001a0007a1201a000186a0ff1a000509101a000509101a000530201b00038d7ea4c67fff00ffff\"}"

readStringDecodedAsProtocolValidatorDatum :: P.String -> P.IO ProtocolT.ValidatorDatum
readStringDecodedAsProtocolValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ProtocolT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d8799fd8799fd8799fd8799f581c47d39eec62a0069c145d691757d2dec20d879a949e0f603b9f3035a346506f6f6c4944ff9f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16eff1b000001835d3453f09fd8799f185a01ffd8799f18b402ffd8799f19270f03ffff1a02faf080581c14f792644b7ce8e294ef44826e6017aee704f9d5f2bfa8b83ce9d38effd8799fd8799f581cabfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16ed8799fd8799f582025d3b9007a5ab81ef517aea2448dccddd6b494d8404ff000cd23c508e8762c79ff00ffffffffff\"}"
-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d87980\"}"
-- readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d87a9fd8799f01ffff\"}"



readStringDecodedAsProtocolValidatorRedeemer :: P.String -> P.IO ProtocolT.ValidatorRedeemer
readStringDecodedAsProtocolValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ProtocolT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result





--------------------------------------------------------------------------------

-- readStringDecodedAsScriptValidatorDatum "{\"getDatum\":\"d8799fd8799fd87a80581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabd87a80581c3a996e7ae6bb0444d0fddad841e0b843c6fb819788853670dd9a417affff\"}"

readStringDecodedAsScriptValidatorDatum :: P.String -> P.IO ScriptT.ValidatorDatum
readStringDecodedAsScriptValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ScriptT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsScriptPolicyRedeemer "{\"getRedeemer\":\"d87980\"}"

readStringDecodedAsScriptPolicyRedeemer :: P.String -> P.IO ScriptT.PolicyRedeemer
readStringDecodedAsScriptPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @ScriptT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

--------------------------------------------------------------------------------

-- cabal repl
-- import Protocol.OffChainHelpers

-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c31bfa53cf3cd05ddb7e3e2230d78bbdbd6735c4018a1deb6b022a60c581c71818ecb72f7f5ac542566360ae318a3dd25a2541db9791414b09a2e581cf1e013aee32c3c7b5e318d8e24e50a4e1e1a03430c0a2212096c185e9f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b0000018971a313b81b00000189ad3dddb8d87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c9eca19af35e1e13892a7ea1215a163b32fa68db748863521c067e26b581c7287baa38b03be764476101673c803b4d6d85640e37addce20ad2d68581cb1cb50589461b2f66955ca45707eb80a7a5a289600953ec5ef96843a9f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897ab39aa31b00000189b64e64a3d87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c7a1cea9e91d69cee0aee8cb3a61e3cf1c815ce9b1793c83d212d1614581c7ab790f20e31972f1b1e84c0e7468932018cdae8010aac890ed9a65f581c1a5595a496b3cc8161a61bff8109bdab3ddb30f1ff3d9ddb07ffda069f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897ae5973d1b00000189b680613dd87a8000000080001a001ef015ffff\"}"
-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f0100581c7a1cea9e91d69cee0aee8cb3a61e3cf1c815ce9b1793c83d212d1614581c7ab790f20e31972f1b1e84c0e7468932018cdae8010aac890ed9a65f581c1a5595a496b3cc8161a61bff8109bdab3ddb30f1ff3d9ddb07ffda069f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001897af078151b00000189b68b4216d87a8000000080001a001f76c7ffff\"}"
-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f02581cb7eb625c4289e5c6caacd4c631d7290f83d3d7fc8032d3ddc287a87b581c0fee00f9d38692d31c707a74fc4296c6d81ddd3853981c4c93115a43581c113a210c5942a82f65bfb879b91a728183966419959afef6a59b7e23581c047d3f4f14e687440923f6f20cf16f9e83b8887f64c3073268429d6c581c5458e7503d967d7b19a5b595552b5d4fcb1e9681e332b9d9a40688949f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff011b000001898581977a1b00000189c11c617ad87a8000000080001a00218417ffff\"}"
-- readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d8799fd8799f03581c5deb66ced32cec9d3f67f65c7a28f48381a8878ea5100a80df5e718b424654581c299717576079e74bf47adb28acee81428732a2facdf55cc3fcfbfea7581c3c6ddaa70112e8d198365a53cb73b5474486acb148b1a6d26742edd1581c3571ab43f462a780a6e4a12338b0dd9c3b0763db2406c7ee2d18ffa1581c64216646e75f98779951f5207a660fafc29fafeb5acfa7aefd17af419f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab011b0000018cd4f58be81b0000018d2234efe8d87a801a0001d4c09f1a000f42401a000f3e58ff00001a00218417ffff\"}"

readStringDecodedAsCampaignValidatorDatum :: P.String -> P.IO CampaignT.ValidatorDatum
readStringDecodedAsCampaignValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsCampaignPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"
-- readStringDecodedAsCampaignPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsCampaignPolicyRedeemer :: P.String -> P.IO CampaignT.PolicyRedeemer
readStringDecodedAsCampaignPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsCampaignValidatorRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsCampaignValidatorRedeemer :: P.String -> P.IO CampaignT.ValidatorRedeemer
readStringDecodedAsCampaignValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result


--------------------------------------------------------------------------------



readStringDecodedAsCampaignFundValidatorDatum :: P.String -> P.IO CampaignFundT.ValidatorDatum
readStringDecodedAsCampaignFundValidatorDatum encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsDatum encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignFundT.ValidatorDatum (LedgerApiV2.getDatum raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsCampaignFundPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"
-- readStringDecodedAsCampaignFundPolicyRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsCampaignFundPolicyRedeemer :: P.String -> P.IO CampaignFundT.PolicyRedeemer
readStringDecodedAsCampaignFundPolicyRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignFundT.PolicyRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

-- readStringDecodedAsCampaignFundValidatorRedeemer "{\"getRedeemer\":\"d8799fd87980ff\"}"

readStringDecodedAsCampaignFundValidatorRedeemer :: P.String -> P.IO CampaignFundT.ValidatorRedeemer
readStringDecodedAsCampaignFundValidatorRedeemer encoded = do
    !raw <- OffChainHelpers.readStringDecodedAsRedeemer encoded
    P.putStrLn $ "Raw: " ++ P.show raw
    let !result = LedgerApiV2.unsafeFromBuiltinData @CampaignFundT.ValidatorRedeemer (LedgerApiV2.getRedeemer raw)
    P.putStrLn $ "Result: " ++ P.show result
    return result

