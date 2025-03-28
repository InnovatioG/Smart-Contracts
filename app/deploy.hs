module Main where
import qualified Deploy
import qualified Prelude            as P

--Modulo:

main :: P.IO ()
main = do
  _ <- Deploy.deploy_ProtocolFactory_And_CampaingFactory_
  P.return ()
