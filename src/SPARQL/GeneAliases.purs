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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, renderForeignError)
import Global.Unsafe (unsafeStringify)
import Simple.JSON (read', readJSON')
import Types (Gene(..), HomologeneID(..))
import Types as Types
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


-- type SpeciesGeneQuery =
--   SPARQLResult { gene :: RDFTerm
--                , homologeneID :: RDFTerm }


type GeneSPARQLResult =
  SPARQLResult { gene :: RDFTerm
               , homologeneID :: RDFTerm
               , geneLabel :: RDFTerm
               , geneAltLabel :: RDFTerm
               , species :: RDFTerm }


-- type HomologeneResultRow =
--   { species :: String
--   , geneURI :: String
--   , geneNames :: { primary :: String, aliases :: Array String } }


homologeneResults :: HomologeneID -> HomologeneIDQuery -> Array Types.Gene
homologeneResults id q = map f q.results.bindings
  where f :: _ -> Types.Gene
        -- f = unsafeCoerce
        f result = Gene { species: wrap result.species.value
                        , homologene: id
                        , name: result.geneLabel.value
                        , aliases: String.split (Pattern ", ") result.geneAltLabel.value
                        , uri: result.gene.value }


-- speciesGene


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


data QueryError =
    MissingSpecies
  | MissingGene
  | MissingAll

geneQueryUnsafe :: { species :: Maybe String
                   , geneName :: Maybe String
                   , homologene :: Maybe String }
                -> Request Json
geneQueryUnsafe vars =
  let taxon = maybe "?species" (\n -> "\"" <> n <> "\"") vars.species

      geneLabel = case vars.geneName of
        Nothing -> ""
        Just n  -> "FILTER(STR(?label) = \""
                   <> n <> "\" && LANG(?label) == \"en\")."

      -- geneLabel = maybe "?label" ("\"" <> _ <> "\"") vars.geneName
      homologeneID = maybe "?homologeneID" (\n -> "\"" <> n <> "\"") vars.homologene

  in wikidataQueryRequest
     $ foldMap (_ <> " ")

     [ "SELECT ?gene ?geneLabel (GROUP_CONCAT(DISTINCT ?geneAltLabel; separator=", ") AS ?geneAltLabel) ?homologeneID"
     , "WHERE"
     , "{"
     , "?item wdt:P31 wd:Q7187"
     , "rdfs:label ?label"
     , "skos:altLabel ?altLabel"
     , "wdt:P593", homologeneID
     , "wdt:P703", taxon
     , geneLabel
     , "}"
     , "GROUP BY ?gene ?geneLabel ?homologeneID" ]

-- geneQuery :: { species :: Maybe String
--              , geneName :: Maybe String
--              , homologene :: Maybe String }
--           -> Either QueryError (Request Json)
-- geneQuery = case species, geneName, homologene of
--   Nothing, Nothing, Just id ->
--   Just s, Just g, Nothing   ->
