{-# LANGUAGE NamedFieldPuns #-}

module NFA (Node(..), Transition(..), NFA(..), mkNfa, joinTs, Transitions) where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.List.Extra (merge)
import Control.Applicative ((<|>))

data Node = Node Int deriving (Eq, Ord, Show)
data Transition = Eps | Transition Char deriving (Eq, Show)
type Transitions = Map.Map Node [(Transition, Node)]

data NFA = NFA
  { initial     :: Node
  , terminal    :: [Node]
  , transitions :: Transitions
  }

mkNfa :: Node -> [Node] -> Transitions -> NFA
mkNfa initial terminal transitions = NFA { initial, terminal, transitions }

joinTs :: Transitions -> Transitions -> Transitions
-- Cryptic thingie is a concise way to concatenates two Maybe lists
joinTs = zipMaps (\a b -> liftA2 (++) a b <|> a <|> b)
  where
    zipMaps f m1 m2 =
      let keys = merge (Map.keys m1) (Map.keys m2)
          elemsMaybe = map (\k -> (k, f (Map.lookup k m1) (Map.lookup k m2))) keys
          elems = [(fst p, fromJust $ snd p) | p <- elemsMaybe, isJust $ snd p]
      in Map.fromList elems