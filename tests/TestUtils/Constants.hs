--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2


{- |
Module      : TestUtils.Constants
Description : Mock Data
-}

module TestUtils.Constants where

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                    as LedgerAda
import qualified Ledger.Crypto                 as LedgerCrypto
import qualified Ledger.Value                  as LedgerValue
import qualified Plutus.V2.Ledger.Api          as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Helpers.OffChain       as OffChainHelpers
import qualified Helpers.OnChain        as OnChainHelpers
import qualified Campaign.Funds.Types as CampaignFundsT
import qualified Campaign.Helpers     as CampaignHelpers
import qualified Campaign.Types       as CampaignT
import qualified Constants            as T
import qualified Types                as T
import qualified TestUtils.Common              as TestUtilsCommon
import qualified TestUtils.Types               as T

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

minAdaForUTxOWithTokens :: Integer
minAdaForUTxOWithTokens = 3_100_000

minAdaProtocolDatum :: Integer
minAdaProtocolDatum = 3_200_000

minAdaCampaignDatum :: Integer
minAdaCampaignDatum = 3_300_000

minAdaCampaignFundsDatum :: Integer
minAdaCampaignFundsDatum = 3_500_000

toAlter_Value_Adding_SomeADA :: LedgerApiV2.Value
toAlter_Value_Adding_SomeADA = LedgerAda.lovelaceValueOf 10_000_000

basicAddress :: Ledger.Address
basicAddress = Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash "a2") Nothing

--------------------------------------------------------------------------------
