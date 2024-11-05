--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.UnitTests.Main
Description : Unit Tests.
-}
module Main where
--------------------------------------------------------------------------------3

-- Non-IOG imports
import qualified Data.Proxy                           as DataProxy
import           Prelude
import qualified Test.Tasty                           as Tasty
import qualified Test.Tasty.Options                   as TastyOptions
import qualified Test.Tasty.QuickCheck                as TastyQC

-- Project imports
import           Contracts.Campaign.Funds.MintingPolicy
import           Contracts.Campaign.Funds.Validator
import           Contracts.Campaign.MintingPolicy
import           Contracts.Campaign.Validator
import           Contracts.Protocol.MintingPolicy
import           Contracts.Protocol.Validator
import           TestUtils.Helpers
import           TestUtils.HelpersINNOVATIO

--------------------------------------------------------------------------------3

main :: IO ()
main = do
    putStrLn "---------------"
    tp <- getTestParams "export/test/deploy.json"
    numTests <- getNumTestCases 100
    putStrLn "---------------"
    let testGroup =
            Tasty.testGroup "Unit Tests"
                [ Tasty.testGroup "Contracts Tests"
                    [ Tasty.testGroup "Protocol Tests"
                        [ protocol_Policy_Tests tp
                        , protocol_Validator_Tests tp
                        ]
                    , Tasty.testGroup "Campaign Tests"
                        [ campaign_Policy_Tests tp
                        , campaign_Validator_Tests tp
                        ]
                    , Tasty.testGroup "CampaignFunds Tests"
                        [ campaignFunds_Policy_Tests tp
                        , campaignFunds_Validator_Tests tp
                        ]
                    ]
                ]

    Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (DataProxy.Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup )
