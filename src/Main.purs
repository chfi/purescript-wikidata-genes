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
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (F, renderForeignError)
import Global.Unsafe (unsafeStringify)
import Prim.RowList (Cons, Nil)
import Record.Extra (type (:::), SCons, SLProxy(..), SNil, slistKeys)
import SPARQL (RDFTerm(..), wdt)
import SPARQL as SPARQL
import SPARQL.GeneAliases as SPARQL
import Simple.JSON (read', readJSON')
import Type.Prelude (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)



main :: Effect Unit
main = launchAff_ $ do

  let whereBlock = "WHERE "
                  <> (SPARQL.printGraphPattern
                    $ SPARQL.homologeneIDToTaxonAndGene "22758"
                   <> (SPARQL.triple (Blank "gene") (wdt "P593") (Blank "homologeneID")))

                  -- <> SPARQL.Basic "?gene wdt:P593 ?homologeneID .")

  let req = SPARQL.wikidataQueryRequest
            $  (SPARQL.printSelect $ List.fromFoldable ["?gene", "?geneLabel", "?homologeneID", "?species", "?tax_id"])
            <> whereBlock


  liftEffect $ log whereBlock
  liftEffect $ log "---------------"

  resp <- AX.request req

  liftEffect do

    log $ SPARQL.printGraphPattern $ SPARQL.homologeneIDToTaxonAndGene "22758"
    log "--------------"


  liftEffect $ log $ unsafeStringify resp
  case resp.body of
    Left err -> liftEffect $ log $ AX.printResponseFormatError err
    Right r  -> do
      -- log r.statusText
      liftEffect $ log $ unsafeStringify r
      -- liftEffect $ case runExcept (read' (unsafeCoerce r) :: F ExampleResult) of

      liftEffect $ do
        log "-----------------"
        log $ unsafeStringify (unsafeCoerce r).results.bindings

        case runExcept (read' (unsafeCoerce r) :: F ( SPARQL.SPARQLResult { gene :: SPARQL.RDFResult
                                                                          , species :: SPARQL.RDFResult
                                                                          , geneLabel :: SPARQL.RDFResult
                                                                          , homologeneID :: SPARQL.RDFResult
                                                                          , tax_id :: SPARQL.RDFResult } )) of

          Left err -> log $ foldMap renderForeignError err
          Right rr -> do
            log "-----------------"
            log $ unsafeStringify rr.results
            log "-----------------"
            log $ "Head: " <> (foldMap (_ <> ", ") rr.head.vars)
            -- log "-----------------"
            -- log $ unsafeStringify rr

            log "-----------------"
            traverse_ (\r -> log $ r.geneLabel.value <> ": "
                                <> r.gene.value <> " - "
                                <> r.species.value <> " - "
                                <> r.homologeneID.value <> " - "
                                <> r.tax_id.value) rr.results.bindings
