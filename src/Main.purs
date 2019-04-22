module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (ResponseFormat(..))
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Foreign (ForeignError(..), Foreign)
import Global.Unsafe (unsafeStringify)
import SPARQL (RDFTerm, uri)
import SPARQL as SPARQL
import SPARQL.GeneAliases as GeneAliases
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)


query1 = (GeneAliases.fromHomologeneID "22758").taxonAndGene
query2 = (GeneAliases.fromGene (wd "Q18247422")).homologeneID
query3 = (GeneAliases.fromGene (wd "Q18247422")).names
query4 = (GeneAliases.taxonToNames (wd "Q83310")).scientific
query5 = (GeneAliases.nameToTaxon "Mus musculus").scientific
query6 = (GeneAliases.nameToTaxon "house mouse").common
query7 = GeneAliases.fromGeneLabel.name (wd "Q83310") "Add1"
query8 = GeneAliases.fromGeneLabel.alias (wd "Q83310") "AI256389"



type JSQuery = EffectFn1
                (EffectFn1 { vars :: Array String
                           , bindings :: Array Json }
                 Unit)
                Unit

jsquery :: String -> JSQuery
jsquery query = mkEffectFn1 \callback -> do
  let req = SPARQL.wikidataQueryRequest query
  launchAff_ do
    resp <- AX.request req
    case resp.body of
      Left err  -> throwError (unsafeCoerce err)
      Right val ->
        let val' = unsafeCoerce val
            vars = val'.head.vars
            bindings = val'.results.bindings
        in liftEffect $ runEffectFn1 callback {vars, bindings}


wd :: String -> RDFTerm
wd = SPARQL.wd

wdt :: String -> RDFTerm
wdt = SPARQL.wdt


fromHomologeneID :: String -> JSQuery
fromHomologeneID id =
  jsquery (GeneAliases.fromHomologeneID id).taxonAndGene

homologeneFromGene :: String -> JSQuery
homologeneFromGene geneURI =
  jsquery (GeneAliases.fromGene (uri geneURI)).homologeneID

namesFromGene :: String -> JSQuery
namesFromGene geneURI =
  jsquery (GeneAliases.fromGene (uri geneURI)).names

scientificNameFromTaxon :: String -> JSQuery
scientificNameFromTaxon taxonURI =
  jsquery (GeneAliases.taxonToNames (uri taxonURI)).scientific

commonNameFromTaxon :: String -> JSQuery
commonNameFromTaxon taxonURI =
  jsquery (GeneAliases.taxonToNames (uri taxonURI)).common

taxonFromScientific :: String -> JSQuery
taxonFromScientific name =
  jsquery (GeneAliases.nameToTaxon name).scientific

taxonFromCommon :: String -> JSQuery
taxonFromCommon name =
  jsquery (GeneAliases.nameToTaxon name).common

fromGeneName :: String -> String -> JSQuery
fromGeneName taxon name =
  jsquery $ GeneAliases.fromGeneLabel.name (uri taxon) name

fromGeneAlias :: String -> String -> JSQuery
fromGeneAlias taxon name =
  jsquery $ GeneAliases.fromGeneLabel.alias (uri taxon) name


testrunQuery :: String -> Aff Unit
testrunQuery q = do
  log "------------"
  log "Query:"
  log $ (SPARQL.uriEncodeQuery q)
  log "------------"
  let req = SPARQL.wikidataQueryRequest q
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




thing :: List String
thing = List.fromFoldable
        [ "first", "second", "thrrrrrd", "hello world" ]

main :: Effect Unit
main = pure unit
