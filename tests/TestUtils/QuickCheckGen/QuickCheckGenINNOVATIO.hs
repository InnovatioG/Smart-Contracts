--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.QuickCheckGen.QuickCheckGenINNOVATIO where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                               as P hiding ((<>))
import qualified Test.QuickCheck                       as QC

-- IOG imports

-- Project imports
import qualified Protocol.Types               as ProtocolT



----------------------------------------------------------------------------------------

{- | Generates an arbitrary 'MinMaxDef' value.
     'MinMaxDef' is a generic type that captures a range with a minimum,
     maximum, and a default value. This instance will create random instances
     within a reasonable range for testing purposes.
-}
instance QC.Arbitrary a => QC.Arbitrary (ProtocolT.MinMaxDef a) where
    arbitrary = ((ProtocolT.MinMaxDef <$> QC.arbitrary) <*> QC.arbitrary) <*> QC.arbitrary

