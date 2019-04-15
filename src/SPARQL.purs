module SPARQL where

import Prelude

import Data.Either (Either(..), either)
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Semigroup (genericAppend, genericAppend')
import Data.List (List)
import Data.List as List
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Variant (Variant)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Record.Extra (class Keys, class SListToRowList, slistKeys)
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), reflectSymbol)
import Type.Row (type (+))



{-

data IRI = IRI String

data Literal = Lit String -- (Maybe String) (Maybe String)

data Blank = Blank String

-- data RDFTerm =
--     IRI String
--   | Lit String
--   | Blank String

type RDFTermRow  r = (iri :: IRI, lit :: Literal, blank :: Blank | r)
type QueryVarRow r = (queryVar :: String | r)

type EntityRow     = RDFTermRow (QueryVarRow ())
type PredRow       = (iri :: IRI, queryVar :: String)

type Entity    = Variant EntityRow
type Predicate = Variant PredRow

type QV = Variant (QueryVarRow ())


-- a select expression is either simply a variable name, or an (expression AS varName)
data SelectExpr = SelVar String | SelExp String String

-- data
-- data Triple = Triple RDFTerm RDFTerm RDFTerm
-- data Triple s p o = Triple s p o
data Triple = Triple Entity Predicate Entity
-}

-- class Keys (xs :: RowList) where
--   keys :: RLProxy xs -> List String

-- instance nilKeys :: Keys Nil where
--   keys _ = mempty

-- instance consKeys ::
--   ( IsSymbol name
--   , Keys tail
--   ) => Keys (Cons name ty tail) where
--   keys _ = List.Cons first rest
--     where first = reflectSymbol (SProxy :: _ name)
--           rest = keys (RLProxy :: _ tail)
{-

data Select r = Select (Record r)

class BuildSelect
      (qvarsList :: RowList)
      (qvarsRow :: # Type)
      | qvarsList -> qvarsRow where
  printSelect :: RLProxy qvarsList
              -> Record qvarsRow
              -> String

instance buildSelectNil ::
  ( TypeEquals (Record tr) {}
  ) => BuildSelect Nil tr where
  printSelect _ _ = mempty

instance buildSelectCons ::
  ( IsSymbol name
  , BuildSelect tail rowt
  , Row.Lacks name rowt
  , Row.Cons  name a rowt row
  ) => BuildSelect (Cons name a tail) row where
  printSelect _ r =
    let name = SProxy :: _ name
        str = " ?" <> (reflectSymbol (SProxy :: _ name))
        rest = printSelect (RLProxy :: _ tail) (Record.delete name r)
    in str <> rest


select :: ∀ row list.
          RowToList row list
       => BuildSelect list row
       => Record row
       -> String
select r = "SELECT " <> printSelect (RLProxy :: _ list) r


select' :: ∀ g ts rl.
           SListToRowList ts rl
        => Keys rl
        => g ts
        -> String
select' slist = "SELECT " <> (foldMap (\v -> "?" <> v <> " ") $ slistKeys slist)

                -}


printSelect :: List String -> String
printSelect = append "SELECT " <<< foldMap (_ <> " ")

data GraphPattern a
  -- = Basic (List a)
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
  | Literal String
  | Blank   String

derive instance eqRDFTerm :: Eq RDFTerm
derive instance ordRDFTerm :: Ord RDFTerm
derive instance genericRDFTerm :: Generic RDFTerm _

instance showRDFTerm :: Show RDFTerm where
  show (IRI str)     = str
  show (Literal str) = show str
  show (Blank str)   = "?" <> str




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
printGraphPattern (Filter s) = "FILTER(" <> s <> ")."
printGraphPattern (Service s) = "SERVICE " <> s
printGraphPattern (Group gp) = "{ " <> (foldMap (\p -> printGraphPattern p <> " ") gp) <> " }"
printGraphPattern (Optional gp) = "OPTIONAL { " <> printGraphPattern gp <> " }"



wdt :: String -> RDFTerm
wdt s = IRI $ "wdt:" <> s

wd :: String -> RDFTerm
wd s = IRI $ "wd:" <> s

hasHomologeneID :: Predicate
hasHomologeneID = predicate $ wdt "P593"

instanceOf :: Predicate
instanceOf = predicate (wdt "P31")



isGene :: RDFTerm -> TriplePattern
isGene var = var `instanceOf` wd "Q7187"

hasTaxon :: RDFTerm -> RDFTerm -> TriplePattern
hasTaxon = predicate (wdt "P703")

hasLabel :: RDFTerm -> RDFTerm -> TriplePattern
hasLabel = predicate (IRI "rdfs:label")

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
hasTaxonCommonName var name =
    Filter $ "STR(" <> show var <> ") = " <> show name <> "). "

isTaxon :: RDFTerm -> TriplePattern
isTaxon var = var `instanceOf` wd "Q7187"
