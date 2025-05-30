
A veces no compila lo ultimo
hacer: 
eliminar: dist-newstyle/build
cabal build lib:innovatio-founder


cabal run deploy

cabal test

cabal repl

cabal test UnitTests -j1 --test-options="--list-tests"

cabal test UnitTests -j1 --test-options="-p \"Protocol_Create_Tx\""




cabal repl 

:set -XTypeApplications
:set -XOverloadedStrings
:set -XFlexibleContexts
:set -XNumericUnderscores

import OffChainHelpers


readStringDecodedAsCampaignValidatorDatum "{\"getDatum\":\"d87981d879941865581c822edd9bd03dcdc7fa1c971b5b9d02881f0513b3e6cfb53df07b2222581c8f477ecf8e133e729cac2ab99bb78f8e4907cafc652e8bad557657d581581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce0b33937400326885f7186e2725a84786266ec1eb06d397680233f80d87a80581c822edd9bd03dcdc7fa1c971b5b9d02881f0513b3e6cfb53df07b222244494e4e4f1a0004e2001a0004e2001a0003e80000001b0000019669ef43f11b00000198396b9bf1d87a8083d879821821d87a80d879821821d87a80d879821822d87a8000001a002b42ae\"}"

readStringDecodedAsCampaignFundValidatorDatum "{\"getDatum\":\"d87981d879941865581c822edd9bd03dcdc7fa1c971b5b9d02881f0513b3e6cfb53df07b2222581c8f477ecf8e133e729cac2ab99bb78f8e4907cafc652e8bad557657d581581ce44c67c53e593671792cc27f095bbcc69aaee2ff1b4d875bdbff5cab581ce0b33937400326885f7186e2725a84786266ec1eb06d397680233f80d87a80581c822edd9bd03dcdc7fa1c971b5b9d02881f0513b3e6cfb53df07b222244494e4e4f1a0004e2001a0004e2001a0003e80000001b0000019669ef43f11b00000198396b9bf1d87a8083d879821821d87a80d879821821d87a80d879821822d87a8000001a002b42ae\"}"



-- readStringDecodedAsCampaignValidatorRedeemer "{\"getRedeemer\":\"d9050481d87980\"}"


import Campaign.Funds.OnChain 
import  Campaign.Funds.Types as CampaignFundsT

import qualified Campaign.Helpers          as CampaignHelpers




datumIn' <- readStringDecodedAsCampaignFundValidatorDatum "{\"getDatum\":\"d87981d87989186500581cc89935510f4d74c7a1698e2ca0886932961fe861cea20a0017556903581c6499f359e30649746c9a2287221b2c791fec63002fffa0989a7a47811a000f42400000001a001e2e03\"}"
datumIn = CampaignFundsT.getCampaignFunds_DatumType datumIn'

let amount_CampaignToken = 100
let amount_ADA= 15000000
let campaignFundsDatum_Out_Control = CampaignHelpers.mkUpdated_CampaignFunds_Datum_With_SoldTokens  datumIn amount_CampaignToken amount_ADA



