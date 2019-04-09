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
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, renderForeignError)
import Global.Unsafe (unsafeStringify)
import SPARQL.GeneAliases as SPARQL
import Simple.JSON (read', readJSON')
import Unsafe.Coerce (unsafeCoerce)


-- | Ignores the xml:lang and other possible fields; we don't need them for now.
type RDFTerm = { "type" :: String, value :: String }

-- derive instance genericRDFTerm :: Generic RDFTerm _
-- derive instance eqRDFTerm :: Eq RDFTerm

type SPARQLResult a = { head :: { vars :: Array String }
                      , results :: { bindings :: Array a } }

wikidataQueryRequest :: String -> Request Json
wikidataQueryRequest query =
  defaultRequest { url = "https://query.wikidata.org/sparql?query=" <> query
                 , method = Left GET
                 , responseFormat = json }


-- type GeneResult

type ExampleResult = SPARQLResult { gene :: { "type" :: String, value :: String }
                                  , geneLabel :: { "type" :: String, value :: String } }


query0 :: Request Json
query0 = wikidataQueryRequest $ foldMap (_ <> " ")
         [ "SELECT ?gene ?geneLabel"
         , "WHERE"
         , "{ "
         , "?gene wdt:P31 wd:Q7187 ; "
         , "wdt:P593 \"22758\" ; "
         , "wdt:P703 ?tax_id . "
         , "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" } }"]


main :: Effect Unit
main = launchAff_ do
  -- let req = wikidataQueryRequest "SELECT ?gene ?geneLabel WHERE { ?gene wdt:P31 wd:Q7187 ; wdt:P593 \"22758\" ; wdt:P703 ?tax_id . SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" } }"

  -- let req = query0
  let req = SPARQL.homologeneQuery "22758"

  liftEffect $ log $ foldMap (_ <> " ")
    [ "SELECT ?gene ?species ?geneLabel ?geneAltLabel"
    , "WHERE"
    , "{"
    , "?gene wdt:P31 wd:Q7187 ;"
    , "wdt:P593 \"", "22758", "\" ;"
    , "wdt:P703 ?tax_id ."
    , "?tax_id rdfs:label ?species."
    , "FILTER(LANG(?species) = \"en\")."
    , "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }"
    , "}"]

  liftEffect $ log "-------------"

  resp <- AX.request req

  liftEffect $ log $ unsafeStringify resp
  case resp.body of
    Left err -> liftEffect $ log $ AX.printResponseFormatError err
    Right r  -> do
      -- log r.statusText
      liftEffect $ log $ unsafeStringify r
      -- liftEffect $ case runExcept (read' (unsafeCoerce r) :: F ExampleResult) of
      liftEffect $ case runExcept (read' (unsafeCoerce r) :: F SPARQL.HomologeneIDQuery) of
        Left err -> log $ foldMap renderForeignError err
        Right rr -> do
          log $ unsafeStringify rr.results
          log "-----------------"
          log $ "Head: " <> (foldMap (_ <> ", ") rr.head.vars)
          -- log $ unsafeStringify rr
          traverse_ (\r -> log $ r.geneLabel.value <> ": " <> r.gene.value <> " - " <> r.geneAltLabel.value) rr.results.bindings
