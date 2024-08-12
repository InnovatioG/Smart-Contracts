module Main where

import           Control.Exception  (throwIO)
import           Data.List
import qualified Deploy             
import qualified Prelude            as P
import           System.Environment (getArgs)
import qualified Types              as T

--Modulo:

main :: P.IO ()
main = do
  [ uTxOutRefStr, tokenEmergencyAdminPolicy_CS_Str ] <- getArgs
  _ <- Deploy.deploy_Protocol_And_CampaignFactory_With_StringParams uTxOutRefStr tokenEmergencyAdminPolicy_CS_Str
  P.return ()

-- TESTNET
-- cabal run deploy "55bd76a19aa8e2e5ab40417a97e01dec4e72700439ce734b2900932aabd205af#2" "e0b33937400326885f7186e2725a84786266ec1eb06d397680233f80"

-- tokenEmergencyAdminPolicy_CS =  "d0b2d0f722973df82c0ac1ee163d8f892a9c2ced7bab1de8300bab06"
