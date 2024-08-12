{- |
Module      : Tests.UnitTests.Main
Description : Unit Tests.
-}
module Main where

-- Non-IOG imports

import           Prelude
import qualified System.Directory                 as SystemDirectory
import qualified Test.Tasty                       as Tasty

-- Project imports
-- import           Contracts.Campaign.Funds.MintingPolicy
-- import           Contracts.Campaign.Funds.Validator
-- import           Contracts.Campaign.MintingPolicy
import           Contracts.Campaign.Validator
import           Contracts.Protocol.MintingPolicy
import           Contracts.Protocol.Validator
import qualified Deploy
import qualified Helpers.OffChain                 as OffChainHelpers
import           TestUtils.Common                 as TestUtilsCommon
import           TestUtils.Types                  as TestUtilsT



main :: IO ()
main = do
    let
        filePath = "export/test/deploy.json"
    maybeDeployParams <- Deploy.loadFactoryDeployParams filePath True
    tp <- case maybeDeployParams of
        Just params -> TestUtilsCommon.generateTestParams params
        Nothing     -> error "Failed to load deploy parameters"

    Tasty.defaultMain $
        Tasty.testGroup
            "Unit Tests"
            [
                Tasty.testGroup
                "Contracts Tests"
                [ Tasty.testGroup
                    "Protocol Tests"
                    [ protocolValidatorTests tp
                    , protocolMPTests tp
                    ]
                 , Tasty.testGroup
                     "Campaign Tests"
                     [ campaignValidatorTests tp
                    --  , campaignMPTests tp
                     ]
                -- , Tasty.testGroup
                --     "Campaign Funds Tests"
                --     [ campaignFundsValidatorTests tp
                --     , campaignFundsMPTests tp
                --     ]
                ]
            ]
