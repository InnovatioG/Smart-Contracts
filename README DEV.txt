
cabal run deployProtocol "0000000000000000000000000000000000000000000000000000000000000000#0" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" 


cabal run deployProtocol "0000000000000000000000000000000000000000000000000000000000000000#0" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" 



----------------------

LAST DEPLOY TESTNET

cabal run deployProtocol "55bd76a19aa8e2e5ab40417a97e01dec4e72700439ce734b2900932aabd205af#2" "e0b33937400326885f7186e2725a84786266ec1eb06d397680233f80" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" 

LAST DEPLOY EMULATOR

cabal run deployProtocol "0000000000000000000000000000000000000000000000000000000000000000#0" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" "MAYZ" "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06" 


----------------------

import Protocol.OffChainHelpers 

readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f0180581ca787a228321d02c2f359ebec3d31a0c6cedbdea5dfb7362eea2e2329581c2a3dce31971075cfea30052c4cd8e67d3c495dd9a8cfad28386d37e300009f581ca44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581cf44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff9fd8799f011a004c4b401a05f5e100ffd8799f021a004c4b401a05f5e100ffd8799f19386b1a004c4b401a05f5e100ffd8799f1a2067346f1a004c4b401a05f5e100ffffd8799f000000ff0000d8799f000000ffd8799f000000ffd8799f000000ff00000080001a001f91b8ffff\"}"

readStringDecodedAsProtocolValidatorDatum "{\"getDatum\":\"d8799fd8799f03581cde9f68591fc2a369a82640aab28a1fc640d8c9e03b737e027f1f082a581cff43ddfb6bcf533f84b5d24b368e2d610c9d413f931f9dcc3d33c6f458200973b805951df6bf13832868c8846bd6bf753234bf33e988ad64f61619eec2039f581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cabff409fd8799f001a3b9aca001b000000174876e800ffffd8799f1b00000001cf7c58001b00000016071384001b000000072957f000ff1a05f5e1001a05f5e100d8799f001a000f42401a0001d4c0ffd8799f001a0007a1201a000186a0ffd8799f001a0007a1201a000186a0ff1a000509101a000530201a00050910801a002d919fffff\"}"

readStringDecodedAsProtocolValidatorRedeemer "{\"getRedeemer\":\"d87980\"}"




----------------------

import Generic.OffChainEvalTesting
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified Protocol.InvestUnit.Types                       as T
import qualified Types                       as T


fundHolding_value = LedgerApiV2.singleton "" "" 1000 <> LedgerApiV2.singleton "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" "A" 1000
iuValues = [("","",100),("a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2","A",1), ("a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2","B",50)]
investUnit = T.InvestUnit {T.iuValues = iuValues}
investUnitTokens = T.iuValues investUnit

createValue_WithTokensFrom_InvestUnit_Plus_CampaignFundsDatum_Value2 fundHolding_value 100 investUnit 


createValue_WithTokensFrom_InvestUnit_Plus_CampaignFundsDatum_Value fundHolding_value 100 investUnit 


fixAmounts2 1 investUnitTokens []

----------------------
import           PlutusTx.Prelude
import Generic.OffChainEvalTesting
import qualified Plutus.V2.Ledger.Api        as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                       as LedgerIntervalV1

interval :: LedgerIntervalV1.Interval LedgerApiV2.POSIXTime
interval = LedgerIntervalV1.Interval {LedgerIntervalV1.ivFrom :1704793899790, LedgerIntervalV1.ivTo 1704794199790  }'


interval =  LedgerIntervalV1.Interval (LedgerApiV2.lowerBound (1704793899790 :: LedgerApiV2.POSIXTime)) (LedgerApiV2.upperBound (1704794199790:: LedgerApiV2.POSIXTime) )

isCampaignOpen 1704794733597 1767089100000 interval