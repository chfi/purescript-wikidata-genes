module SPARQL.GeneAliases where

import Prelude

import SPARQL (GraphPattern, RDFTerm, Triple)
import SPARQL as SPARQL


queryString :: String -> GraphPattern (Triple RDFTerm) -> String
queryString select gp =
  "SELECT " <> select <> " WHERE "
  <> SPARQL.printGraphPattern gp

fromHomologeneID :: String -> { taxonAndGene :: String }
fromHomologeneID homologeneID =
  { taxonAndGene: queryString "?gene ?taxon"
                  $ SPARQL.homologeneIDToTaxonAndGene' homologeneID}

fromGene :: RDFTerm -> { homologeneID :: String
                       , names :: String }
fromGene gene =
  { homologeneID: queryString "?homologeneID"
                  $ SPARQL.geneToHomologeneID gene
  , names: queryString "?geneLabel (GROUP_CONCAT(DISTINCT ?geneAltLabel; separator=\"; \") AS ?geneAltLabel)"
           $ SPARQL.geneToNames gene
  }

taxonToNames :: RDFTerm -> { scientific :: String
                           , common :: String }
taxonToNames taxon =
  { scientific: queryString "?taxonName"
                $ SPARQL.taxonNames taxon
  , common: queryString "?taxonCommonName"
                $ SPARQL.taxonNames taxon }


nameToTaxon :: String -> { scientific :: String
                         , common :: String }
nameToTaxon name =
  { scientific: queryString "?taxon"
                $ SPARQL.taxonFromName name
  , common: queryString "?taxon" $ SPARQL.taxonFromCommonName name }

fromGeneLabel :: { name :: RDFTerm -> String -> String
            , alias :: RDFTerm -> String -> String }
fromGeneLabel =
  { name: \taxon label -> queryString "?gene"
                          $ SPARQL.taxonAndGeneNameToGene taxon label
  , alias: \taxon label -> queryString "?gene"
                           $ SPARQL.taxonAndGeneAliasToGene taxon label }
