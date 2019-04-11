module Types where

import Prelude

import Data.List (List)
import Data.Newtype (class Newtype)

newtype Species = Species String

derive instance newtypeSpecies :: Newtype Species _
derive instance eqSpecies :: Eq Species
derive instance ordSpecies :: Ord Species
derive newtype instance showSpecies :: Show Species

newtype HomologeneID = HomologeneID String

derive instance newtypeHomologeneID :: Newtype HomologeneID _
derive instance eqHomologeneID :: Eq HomologeneID
derive instance ordHomologeneID :: Ord HomologeneID
derive newtype instance showHomologeneID :: Show HomologeneID

newtype Gene = Gene { species :: Species
                    , homologene :: HomologeneID
                    , name :: String
                    , uri :: String
                    , aliases :: Array String }

derive instance newtypeGene :: Newtype Gene _
derive newtype instance showGene :: Show Gene
