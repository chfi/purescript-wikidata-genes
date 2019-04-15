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


-- data

{-

Use something like I did here:
    traverse_ log $ SPARQL.keys (RLProxy :: _ (Cons "aaaa" Void (Cons "bbbb" Void Nil)))

I.e. just construct a rowlist where the type is "Void" at each label.

In fact, could use Homogeneous with this.

Then I just (""just"") want a function like

∀ r. Homogeneous r Void
  => RowToList r l
  => List String -> Maybe (Select r)


-}


-- class BuildSelect (row :: # Type) where
--   buildSelect ::

-- mkSelect :: ∀ r. Homogeneous r Void
--          =>

-- mkSelect ::
-- type RDFTriple



printSelect' :: List String -> String
-- mkSelect vs = "SELECT " <> foldMap (_ <> " ") vs
printSelect' = append "SELECT " <<< foldMap (_ <> " ")

data GraphPattern a
  -- = Basic (List a)
  = Basic a
  | Optional (GraphPattern a)
  | Group (List (GraphPattern a))

derive instance genericGraphPattern :: Generic (GraphPattern a) _


instance semigroupGraphPattern :: Semigroup (GraphPattern a) where
  append  (Group l1)   (Group l2) = Group $ l1 <> l2
  append  (Group l)  b@(Basic _)  = Group $ List.Cons b l
  append a@(Basic _)   (Group l)  = Group $ List.Cons a l
  append a           b            = Group $ List.fromFoldable [a, b]






-- newtype Triple a = Triple { subject :: a
--                           , predicate :: a
--                           , object :: a }

-- derive instance newtypeTriple :: Newtype (Triple a) _
-- derive instance eqTriple :: Eq a => Eq (Triple a)



-- mkTriple :: ∀ a. a -> a -> a -> Triple a
-- mkTriple subject predicate object = Triple { subject, predicate, object }

-- printTriple :: Triple String -> String
-- printTriple (Triple t) = t.subject   <> " "
--                       <> t.predicate <> " "
--                       <> t.object    <> ". "

-- printExpr :: Either String (Triple String) -> String
-- printExpr = either (_ <> " ") printTriple

-- printWhere :: List (Either String (Triple String)) -> String
-- printWhere ts = "WHERE { " <> foldMap printExpr ts <> "}"

printGraphPattern :: GraphPattern String -> String
printGraphPattern (Basic l) = l
printGraphPattern (Group gp) = "{ " <> (foldMap (\p -> printGraphPattern p <> " ") gp) <> " }"
printGraphPattern (Optional gp) = "OPTIONAL { " <> printGraphPattern gp <> " }"


hasHomologeneID :: String -> String -> GraphPattern String
hasHomologeneID var id = Basic $ var <> " wdt:P593 " <> show id <> " ."

isGene :: String -> GraphPattern String
isGene var = Basic $ var <> " wdt:P31 wd:Q7187 ."

hasTaxon :: String -> String -> GraphPattern String
hasTaxon var taxonURI = Basic $ var <> " wdt:P703 " <> taxonURI <> " ."

hasLabel :: String -> String -> GraphPattern String
hasLabel var label = Basic $ var <> " rdfs:label " <> label <> " ."

filter :: String -> GraphPattern String
filter expr = Basic $ "FILTER(" <> expr <> "). "

useLabelService :: GraphPattern String
useLabelService = Basic "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }"



-- homologeneIDToTaxonGene :: String -> GraphPattern String
-- homologeneIDToTaxonGene id =
--   Group
--   $ List.fromFoldable
--   $ Basic
--   <$> [ "?gene wdt:P31 wd:Q7187 ;"
--       , "wdt:P593 \"" <> id <> "\" ;"
--       , "wdt:P703 ?tax_id ."
--       , "?tax_id rdfs:label ?species ."
--       , "FILTER(LANG(?species) = \"en\"). "
--       , "SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\" }" ]




homologeneIDToTaxonAndGene :: String -> GraphPattern String
homologeneIDToTaxonAndGene id =
     isGene "?gene"
  <> "?gene" `hasHomologeneID` id
  <> "?gene" `hasTaxon` "?tax_id"
  <> "?tax_id" `hasLabel` "?species"
  <> filter "LANG(?species) = \"en\""
  <> useLabelService

hasTaxonName :: String -> String -> GraphPattern String
hasTaxonName var name = Basic $ var <> " wdt:P225 " <> name <> " ."

hasTaxonCommonName :: String -> String -> GraphPattern String
hasTaxonCommonName var name =
    filter $ "STR(" <> var <> ") = " <> show name <> "). "


isTaxon :: String -> GraphPattern String
isTaxon var = Basic $ var <> " wdt:P31 wd:Q16521 ."

taxonURIToName :: String -> String -> GraphPattern String
taxonURIToName = hasTaxonName
-- taxonURIToName tax_id tax_name =
--   tax_id `hasTaxonName` tax_name

taxonURIToCommonName :: String -> String -> GraphPattern String
taxonURIToCommonName = hasTaxonCommonName
-- taxonURIToCommonName tax_id label =
  -- tax_id `hasTaxonCommonName` label


geneHasName :: String -> String -> GraphPattern String
geneHasName = hasLabel

-- geneHasAliases =

-- taxonNameToURI =


-- data QueryPattern a = QP { boundVariables :: Set String
--                          ,

-- data QueryVar (name :: Symbol) = QueryVar


-- newtype TriplePattern = { subject :: String
--                         ,
-- newtype SPARQLQuery =
--   SPARQLQuery { queryVars :: Set String
--               , graphPattern :: GraphPattern String }

-- derive instance newtypeSPARQLQuery :: Newtype SPARQLQuery
