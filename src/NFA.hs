{-# LANGUAGE NamedFieldPuns #-}

module NFA (Node(..), Transition(..), NFA(..), nfa, joinTs, Transitions) where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.List.Extra (merge)
import Control.Applicative ((<|>))

data Node = Node Int deriving (Eq, Ord, Show)
data Transition = Eps | Transition Char deriving (Eq, Show)
type Transitions = Map.Map Node [(Transition, Node)]

data NFA = NFA
  { initial     :: Node,
    terminal    :: [Node],
    transitions :: Transitions
  }

nfa :: Node -> [Node] -> Transitions -> NFA
nfa initial terminal transitions = NFA { initial, terminal, transitions }

joinTs :: Transitions -> Transitions -> Transitions
joinTs = zipMaps (\a b -> liftA2 (++) a b <|> a <|> b) -- This cryptic thing basically concatenates two Maybe lists
  where
    zipMaps f m1 m2 =
      let keys = merge (Map.keys m1) (Map.keys m2)
          elemsMaybe = map (\k -> (k, f (Map.lookup k m1) (Map.lookup k m2))) keys
          elems = [(fst p, fromJust $ snd p) | p <- elemsMaybe, isJust $ snd p]
      in Map.fromList elems