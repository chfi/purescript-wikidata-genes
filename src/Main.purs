module Main where

import Prelude

import Affjax (Request, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List as List
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (F, renderForeignError)
import Global.Unsafe (unsafeStringify)
import Prim.RowList (Cons, Nil)
import Record.Extra (type (:::), SCons, SLProxy(..), SNil, slistKeys)
import SPARQL (RDFTerm(..), wdt, wd)
import SPARQL as SPARQL
import SPARQL.GeneAliases as SPARQL
import Simple.JSON (read', readJSON')
import Type.Prelude (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)


query1 =
  "SELECT ?gene ?taxon WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.homologeneIDToTaxonAndGene' "22758")

query2 =
  "SELECT ?homologeneID WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.geneToHomologeneID (wd "Q18247422"))

query3 =
  "SELECT ?geneLabel (GROUP_CONCAT(DISTINCT ?geneAltLabel; separator=\"; \") AS ?geneAltLabel) WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.geneToNames (wd "Q18247422"))

query4 =
  "SELECT ?taxonName ?taxonCommonName WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.taxonNames (wd "Q83310"))

query5 =
  "SELECT ?taxon WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.taxonFromName "Mus musculus")

query6 =
  "SELECT ?taxon WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.taxonFromCommonName "house mouse")


query7 =
  "SELECT ?gene WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.taxonAndGeneNameToGene (wd "Q83310") "Add1")


query8 =
  "SELECT ?gene WHERE "
  <> (SPARQL.printGraphPattern
      $ SPARQL.taxonAndGeneAliasToGene (wd "Q83310") "AI256389")



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
