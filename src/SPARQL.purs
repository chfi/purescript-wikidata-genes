module SPARQL where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Variant (Variant)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..), reflectSymbol)



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

-- data
-- data Triple = Triple RDFTerm RDFTerm RDFTerm
-- data Triple s p o = Triple s p o
data Triple = Triple Entity Predicate Entity


class Keys (xs :: RowList) where
  keys :: RLProxy xs -> List String

instance nilKeys :: Keys Nil where
  keys _ = mempty

instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keys _ = List.Cons first rest
    where first = reflectSymbol (SProxy :: _ name)
          rest = keys (RLProxy :: _ tail)


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




data SPARQLQuery =
  SPARQLQuery { select :: Array String
              , triples :: List Triple }
-- data SPARQLTerm a =
--     SELECT (Array String)
--   | WHERE
