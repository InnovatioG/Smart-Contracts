--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.QuickCheckGen.QuickCheckGenINNOVATIO where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Control.Monad                         as ControlMonad
import qualified Debug.Trace                           as DebugTrace
import           Prelude                               as P hiding ((<>))
import qualified Test.QuickCheck                       as QC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2
import           PlutusTx.Prelude                      (find, nub, remainder, sort)

-- Project imports

import qualified Data.List                             as DataList
import qualified Data.Maybe                            as DataMaybe
import qualified Ledger.Value                          as LedgerValue
import qualified Campaign.Funds.Types           as CampaignFundsT
import qualified Campaign.Types                   as CampaignT
import qualified Protocol.Types               as ProtocolT
import qualified Types                        as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.TypesINNOVATIO


----------------------------------------------------------------------------------------

{- | Generates an arbitrary 'MinMaxDef' value.
     'MinMaxDef' is a generic type that captures a range with a minimum,
     maximum, and a default value. This instance will create random instances
     within a reasonable range for testing purposes.
-}
instance QC.Arbitrary a => QC.Arbitrary (ProtocolT.MinMaxDef a) where
    arbitrary = ((ProtocolT.MinMaxDef <$> QC.arbitrary) <*> QC.arbitrary) <*> QC.arbitrary

