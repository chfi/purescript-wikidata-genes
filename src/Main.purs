module Main where

import Prelude

import Affjax as AX
import Data.Either (Either(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import SPARQL (wd)
import SPARQL as SPARQL
import SPARQL.GeneAliases as GeneAliases
import Unsafe.Coerce (unsafeCoerce)


query1 = (GeneAliases.fromHomologeneID "22758").taxonAndGene

query2 = (GeneAliases.fromGene (wd "Q18247422")).homologeneID

query3 = (GeneAliases.fromGene (wd "Q18247422")).names

query4 = (GeneAliases.taxonToNames (wd "Q83310")).scientific

query5 = (GeneAliases.nameToTaxon "Mus musculus").scientific

query6 = (GeneAliases.nameToTaxon "house mouse").common

query7 = GeneAliases.fromGeneLabel.name (wd "Q83310") "Add1"

query8 = GeneAliases.fromGeneLabel.alias (wd "Q83310") "AI256389"



testrunQuery :: String -> Aff Unit
testrunQuery q = do
  log "------------"
  log "Query:"
  log $ (SPARQL.uriEncodeQuery q)
  log "------------"
  let req = SPARQL.wikidataQueryRequest (SPARQL.uriEncodeQuery q)
  resp <- AX.request req
  log $ unsafeStringify resp
  case resp.body of
    Left err -> log $ AX.printResponseFormatError err
    Right r  -> do

      log $ unsafeStringify r
      log "--    Head   -- "
      log $ unsafeStringify (unsafeCoerce (unsafeCoerce r).head.vars :: Array String)
      log "--  Bindings -- "
      log $ unsafeStringify (unsafeCoerce r).results.bindings
      log ""



testQueries :: Aff Unit
testQueries = do
  testrunQuery query1 -- Works
  delay (wrap 500.0)

  testrunQuery query2 -- Works
  delay (wrap 500.0)

  testrunQuery query3 -- Works
  delay (wrap 500.0)

  testrunQuery query4 -- Works
  delay (wrap 500.0)

  testrunQuery query5 -- Works
  delay (wrap 500.0)

  testrunQuery query6 -- Works
  delay (wrap 500.0)

  testrunQuery query7 -- Works
  delay (wrap 500.0)

  testrunQuery query8 -- Works
  delay (wrap 500.0)


main :: Effect Unit
main = launchAff_ do

  testQueries
