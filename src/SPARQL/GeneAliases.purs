module SPARQL.GeneAliases where

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
import Simple.JSON (read', readJSON')
import Unsafe.Coerce (unsafeCoerce)


-- | Ignores the xml:lang and other possible fields; we don't need them for now.
type RDFTerm = { "type" :: String, value :: String }

type SPARQLResult a = { head :: { vars :: Array String }
                      , results :: { bindings :: Array a } }

wikidataQueryRequest :: String -> Request Json
wikidataQueryRequest query =
  defaultRequest { url = "https://query.wikidata.org/sparql?query=" <> query
                 , method = Left GET
                 , responseFormat = json }


type Query1Result = SPARQLResult { gene :: { "type" :: String, value :: String }
                                 , geneLabel :: { "type" :: String, value :: String } }



type HomologeneIDQuery =
  SPARQLResult { gene :: RDFTerm
               , species :: RDFTerm
               , geneLabel :: RDFTerm
               , geneAltLabel :: RDFTerm }


type HomologeneResultRow =
  { species :: String
  , geneURI :: String
  , geneNames :: { primary :: String, aliases :: Array String } }


homologeneResults :: HomologeneIDQuery -> Array HomologeneResultRow
homologeneResults q = map f q.results.bindings
  where f :: { gene :: _, species :: _, geneLabel :: _, geneAltLabel :: _ } -> HomologeneResultRow
        f row = { species: row.species
                , geneURI: row.gene
                , geneNames: { primary: row.geneLabel, aliases: row.geneAltLabel } }



-- query1 :: Request Json
-- query1 = wikidataQueryRequest """SELECT ?item ?species ?itemLabel ?itemAltLabel
-- WHERE
-- {
--   ?item wdt:P31 wd:Q7187 ;
--   wdt:P593 \"22758\" ;
--   wdt:P703 ?tax_id .

--   ?tax_id rdfs:label ?species.
--   FILTER(LANG(?species) = \"en\").
--   SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }
-- }
-- """

homologeneQuery :: String -> Request Json
homologeneQuery homologeneID =
  wikidataQueryRequest
  $ foldMap (_ <> " ")
  [ "SELECT ?gene ?species ?geneLabel ?geneAltLabel"
  , "WHERE"
  , "{"
  , "?gene wdt:P31 wd:Q7187 ;"
  , "wdt:P593", ("\"" <> homologeneID <> "\""), ";"
  , "wdt:P703 ?tax_id ."
  , "?tax_id rdfs:label ?species."
  , "FILTER(LANG(?species) = \"en\")."
  , "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }"
  , "}"]
