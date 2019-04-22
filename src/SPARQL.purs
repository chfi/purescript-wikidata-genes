module SPARQL where

import Prelude

import Affjax (Request, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Data.Newtype (wrap)
import Data.String as String
import Effect.Aff (Aff, throwError)
import Foreign (ForeignError(..), Foreign)
import Unsafe.Coerce (unsafeCoerce)


printSelect :: List String -> String
printSelect = append "SELECT " <<< foldMap (_ <> " ")

data GraphPattern a
  = Basic a
  | Filter String
  | Service String
  | Optional (GraphPattern a)
  | Group (List (GraphPattern a))

derive instance genericGraphPattern :: Generic (GraphPattern a) _

instance semigroupGraphPattern :: Semigroup (GraphPattern a) where
  append  (Group l1)   (Group l2) = Group $ l1 <> l2
  append  (Group l)  b@(Basic _)  = Group $ List.Cons b l
  append a@(Basic _)   (Group l)  = Group $ List.Cons a l
  append a           b            = Group $ List.fromFoldable [a, b]


data RDFTerm =
    IRI     String
  | Prefix  String String
  | Literal String
  | Blank   String

derive instance eqRDFTerm :: Eq RDFTerm
derive instance ordRDFTerm :: Ord RDFTerm
derive instance genericRDFTerm :: Generic RDFTerm _

showRDFTerm' :: RDFTerm -> String
showRDFTerm' (IRI str)       = "<" <> str <> ">"
showRDFTerm' (Prefix px str) = px <> ":" <> str
showRDFTerm' (Literal str)   = show str
showRDFTerm' (Blank str)     = "?" <> str

instance showRDFTerm :: Show RDFTerm where
  show = showRDFTerm'


data Triple a = Triple a a a

triple :: RDFTerm -> RDFTerm -> RDFTerm -> GraphPattern (Triple RDFTerm)
triple s p o = Basic $ Triple s p o

subject :: RDFTerm -> RDFTerm -> RDFTerm -> GraphPattern (Triple RDFTerm)
subject s p o = Basic $ Triple s p o

predicate :: RDFTerm -> RDFTerm -> RDFTerm -> GraphPattern (Triple RDFTerm)
predicate p s o = Basic $ Triple s p o

object :: RDFTerm -> RDFTerm -> RDFTerm -> GraphPattern (Triple RDFTerm)
object o s p = Basic $ Triple s p o

type TriplePattern = GraphPattern (Triple RDFTerm)

type Predicate = RDFTerm -> RDFTerm -> TriplePattern


printGraphPattern :: GraphPattern (Triple RDFTerm) -> String
printGraphPattern (Basic (Triple s p o)) = show s <> " " <> show p <> " " <> show o <> " ."
printGraphPattern (Filter s) = "FILTER(" <> s <> "). "
printGraphPattern (Service s) = "SERVICE " <> s
printGraphPattern (Group gp) = "{ " <> (foldMap (\p -> printGraphPattern p <> " ") gp) <> " }"
printGraphPattern (Optional gp) = "OPTIONAL { " <> printGraphPattern gp <> " }"


uri :: String -> RDFTerm
uri = IRI

wdt :: String -> RDFTerm
wdt s = Prefix "wdt" s

wd :: String -> RDFTerm
wd s = Prefix "wd" s

hasHomologeneID :: Predicate
hasHomologeneID = predicate $ wdt "P593"

instanceOf :: Predicate
instanceOf = predicate (wdt "P31")



isGene :: RDFTerm -> TriplePattern
isGene var = var `instanceOf` wd "Q7187"

hasTaxon :: RDFTerm -> RDFTerm -> TriplePattern
hasTaxon = predicate (wdt "P703")

hasLabel :: RDFTerm -> RDFTerm -> TriplePattern
hasLabel = predicate (Prefix "rdfs" "label")

filter :: ∀ a. String -> GraphPattern a
filter expr = Filter expr

useLabelService :: ∀ a. GraphPattern a
useLabelService = Service "wikibase:label { bd:serviceParam wikibase:language \"en\" }"


homologeneIDToTaxonAndGene :: String -> GraphPattern (Triple RDFTerm)
homologeneIDToTaxonAndGene id =
  let gene = Blank "gene"
      tax_id = Blank "tax_id"
      species = Blank "species"
  in isGene gene
  <> gene `hasHomologeneID` (Literal id)
  <> gene `hasTaxon` tax_id
  <> tax_id `hasLabel` species
  <> filter "LANG(?species) = \"en\""
  <> useLabelService



hasTaxonName :: Predicate
hasTaxonName = predicate (wdt "P225")

hasTaxonCommonName :: Predicate
hasTaxonCommonName = predicate (wdt "P1843")

isTaxon :: RDFTerm -> TriplePattern
isTaxon var = var `instanceOf` wd "Q7187"


geneURIToGeneName :: RDFTerm -> GraphPattern (Triple RDFTerm)
geneURIToGeneName gene =
  let geneLabel = Blank "geneLabel"
  in gene `hasLabel` geneLabel


-- SELECT ?gene ?taxon
homologeneIDToTaxonAndGene' :: String -> GraphPattern (Triple RDFTerm)
homologeneIDToTaxonAndGene' id =
  let gene = Blank "gene"
      taxon = Blank "taxon"
  in isGene gene
  <> gene `hasHomologeneID` (Literal id)
  <> gene `hasTaxon` taxon

-- SELECT ?homologeneID
geneToHomologeneID :: RDFTerm -> GraphPattern (Triple RDFTerm)
geneToHomologeneID gene =
  let homologeneID = Blank "homologeneID"
  in Group (pure $ gene `hasHomologeneID` homologeneID)

-- SELECT ?geneLabel (GROUP_CONCAT(DISTINCT ?geneAltLabel; separator="; ") AS ?geneAltLabel)
geneToNames :: RDFTerm -> GraphPattern (Triple RDFTerm)
geneToNames gene =
  let geneLabel = Blank "geneLabel"
      geneAltLabel = Blank "geneAltLabel"
  in gene `hasLabel` geneLabel
  <> gene `predicate (Prefix "skos" "altLabel")` geneAltLabel
  <> filter "LANG(?geneLabel) = \"en\" && LANG(?geneAltLabel) = \"en\""

-- SELECT ?taxonName ?taxonCommonName
taxonNames :: RDFTerm -> GraphPattern (Triple RDFTerm)
taxonNames taxon =
  let taxonName = Blank "taxonName"
      taxonCommonName = Blank "taxonCommonName"
  in taxon `hasTaxonName` taxonName
  <> taxon `hasTaxonCommonName` taxonCommonName
  -- <> filter ("STR(?taxonCommonName) = " <> show name)
  <> filter "LANG(?taxonCommonName) = \"en\""

-- SELECT ?taxon
taxonFromName :: String -> GraphPattern (Triple RDFTerm)
taxonFromName name =
  let taxon = Blank "taxon"
  in Group $ pure $ taxon `hasTaxonName` (Literal name)

-- SELECT ?taxon
taxonFromCommonName :: String -> GraphPattern (Triple RDFTerm)
taxonFromCommonName name =
  let taxon = Blank "taxon"
      taxonCommonName = Blank "taxonCommonName"
  in taxon `hasTaxonCommonName` taxonCommonName
  <> filter ("LANG(?taxonCommonName) = \"en\" && LCASE(STR(?taxonCommonName)) = "
             <> show name)

-- SELECT ?gene
taxonAndGeneNameToGene :: RDFTerm -> String -> GraphPattern (Triple RDFTerm)
taxonAndGeneNameToGene taxon name =
  let gene = Blank "gene"
      geneLabel = Blank "geneLabel"
  in isGene gene
  <> gene `hasLabel` geneLabel
  <> gene `hasTaxon` taxon
  <> filter ("STR(" <> show geneLabel <> ") = " <> show name
             <> " && LANG(" <> show geneLabel <> ") = \"en\"")


-- SELECT ?gene
taxonAndGeneAliasToGene :: RDFTerm -> String -> GraphPattern (Triple RDFTerm)
taxonAndGeneAliasToGene taxon alias =
  let gene = Blank "gene"
      geneAltLabel = Blank "geneAltLabel"
  in isGene gene
  <> gene `predicate (Prefix "skos" "altLabel")` geneAltLabel
  <> gene `hasTaxon` taxon
  <> filter ("STR(" <> show geneAltLabel <> ") = " <> show alias
             <> " && LANG(" <> show geneAltLabel <> ") = \"en\"")

uriEncodeQuery :: String -> String
uriEncodeQuery = String.replaceAll (wrap "&") (wrap "%26")
                 -- <<< String.replaceAll (wrap "|") (wrap "%7C")

wikidataQueryRequest :: String -> Request Json
wikidataQueryRequest query =
  let query' = uriEncodeQuery query
  in defaultRequest { url = "https://query.wikidata.org/sparql?query=" <> query'
                    , method = Left GET
                    , responseFormat = json }




wikidataPerformRequestGET :: String
                             -- { select :: String
                             -- , graphPattern :: GraphPattern (Triple RDFTerm) }
                          -> Aff { vars :: Array String
                                 , bindings :: Foreign
                                 }
wikidataPerformRequestGET query = do
  let queryRequest = wikidataQueryRequest query
                     -- $ "SELECT "
                     -- <> query.select
                     -- <> " WHERE " <> printGraphPattern query.graphPattern

  resp <- AX.request queryRequest
  case resp.body of
    Left err -> throwError (unsafeCoerce err)
    Right val -> let value' = unsafeCoerce val
                 in pure { vars: value'.head.vars
                         , bindings: unsafeCoerce (value'.results.bindings) }


-- wikidataRequestJS :: ({ vars :: Array String
--                       , bindings :: Foreign } -> Effect Unit)
--                   -> String
--                   -> Effect Unit
-- wikidataRequestJS callback query = launchAff_ do
--   results <- wikidataPerformRequestGET request
--   liftEffect $ callback results


-- wikidataRequestJS :: ({ vars :: Array String
--                       , bindings :: Foreign } -> Effect Unit)
--                   -> Aff { vars :: Array String
--                          , bindings :: Foreign }
--                   -> Effect Unit
-- wikidataRequestJS callback request = launchAff_ do
--   results <- request
--   liftEffect $ callback results
