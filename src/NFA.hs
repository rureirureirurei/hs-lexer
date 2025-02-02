{-# LANGUAGE NamedFieldPuns #-}


module NFA (Node(..), Transition(..), NFA(..), makeNfa, (+++)) where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Graph

data Node = Node Int deriving (Eq, Ord, Show)

data Transition = Eps | Transition Char deriving (Eq, Show)

data NFA = NFA
  { initial     :: Node,
    terminal    :: [Node],
    transitions :: Map.Map Node [(Transition, Node)]
  }

instance Show NFA where
  show (NFA initial terminal transitions) =
    "NFA {\n"
      ++ "  Initial State: "
      ++ show initial
      ++ ",\n"
      ++ "  Terminal States: "
      ++ show terminal
      ++ ",\n"
      ++ "  Transitions:\n"
      ++ showTransitions transitions
      ++ "}"

showTransitions :: Map.Map Node [(Transition, Node)] -> String
showTransitions transitions =
  unlines $
    map
      (\(node, edges) -> "    " ++ show node ++ " -> " ++ showEdges edges)
      (Map.toList transitions)

showEdges :: [(Transition, Node)] -> String
showEdges edges =
  "[" ++ unwords (map showEdge edges) ++ "]"

showEdge :: (Transition, Node) -> String
showEdge (Eps, node) = "(Eps -> " ++ show node ++ ")"
showEdge (Transition c, node) = "('" ++ [c] ++ "' -> " ++ show node ++ ")"

makeNfa :: Node -> [Node] -> Map.Map Node [(Transition, Node)] -> NFA
makeNfa initial terminal transitions =
  NFA
    { initial,
      terminal,
      transitions
    }

mergeLists :: (Ord a) => [a] -> [a] -> [a]
mergeLists l1@(h1 : tl1) l2@(h2 : tl2) =
  if h1 < h2
    then h1 : mergeLists tl1 l2
    else h2 : mergeLists l1 tl2
mergeLists [] l2 = l2
mergeLists l1 [] = l1

zipMaps :: (Ord k) => (k -> Maybe a -> Maybe b -> Maybe c) -> Map.Map k a -> Map.Map k b -> Map.Map k c
zipMaps f m1 m2 =
  let keys = mergeLists (Map.keys m1) (Map.keys m2)
      elemsMaybe = map (\k -> (k, f k (Map.lookup k m1) (Map.lookup k m2))) keys
      elems = [(fst p, fromJust $ snd p) | p <- elemsMaybe, isJust $ snd p]
   in Map.fromList elems

(+++) :: Map.Map Node [(Transition, Node)] -> Map.Map Node [(Transition, Node)] -> Map.Map Node [(Transition, Node)]
(+++) ts1 ts2 =
  zipMaps (\_ l1 l2 -> concatMaybe l1 l2) ts1 ts2
  where
    concatMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
    concatMaybe (Just l1) (Just l2) = Just (l1 ++ l2)
    concatMaybe Nothing (Just l2) = Just l2
    concatMaybe (Just l1) Nothing = Just l1
    concatMaybe Nothing Nothing = Nothing