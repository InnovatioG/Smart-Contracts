
cabal run deploy

cabal test

cabal repl

cabal test UnitTests -j1 --test-options="--list-tests"

cabal test UnitTests -j1 --test-options="-p \"Protocol_Create_Tx\""
