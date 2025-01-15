import Control.Monad.State
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)

-- Data Types --

data Regex
  = Empty
  | Cat Regex Regex
  | Alt Regex Regex
  | Lit Char
  | Star Regex
  | Plus Regex
  | Range Char Char

data Node = Node Int deriving (Eq, Ord, Show)

data Transition = Eps | Transition Char

data NFA = NFA
  { initial :: Node,
    terminal :: [Node],
    transitions :: Map.Map Node [(Transition, Node)]
  }

-- data DFA = DFA
--   {
--     initial :: Node,
--     terminal :: [Node], 
--     transitions2 :: Map.Map Node [(Transition, Node)]
--   }

-- Helper functions --

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
  zipMaps (\k l1 l2 -> concatMaybe l1 l2) ts1 ts2
  where
    concatMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
    concatMaybe (Just l1) (Just l2) = Just (l1 ++ l2)
    concatMaybe Nothing (Just l2) = Just l2
    concatMaybe (Just l1) Nothing = Just l1
    concatMaybe Nothing Nothing = Nothing

-- Regex to NFA --

translate :: Regex -> State Int NFA
translate Empty = do
  node <- newNode ()
  return $ makeNfa node [] Map.empty
translate (Lit c) = do
  init <- newNode ()
  term <- newNode ()
  let transitions = Map.fromList [(init, [((Transition c), term)])]
  return $ makeNfa init [term] transitions
translate (Alt r1 r2) = do
  a1 <- (translate r1)
  a2 <- (translate r2)
  i0 <- newNode ()
  let (t1, i1, ts1) = ((terminal a1), (initial a1), (transitions a1))
      (t2, i2, ts2) = ((terminal a2), (initial a2), (transitions a2))
      edges = [(Eps, i1), (Eps, i2)]
      newTs = Map.fromList [(i0, edges)]
   in return $
        makeNfa
          i0
          (t1 ++ t2)
          (ts1 +++ ts2 +++ newTs)
translate (Cat r1 r2) = do
  a1 <- (translate r1)
  a2 <- (translate r2)
  let (t1, i1, ts1) = ((terminal a1), (initial a1), (transitions a1))
      (t2, i2, ts2) = ((terminal a2), (initial a2), (transitions a2))
      edge = [(Eps, i2)]
      newTs = Map.fromList [(t, edge) | t <- t1]
   in return $
        makeNfa
          i1
          t2
          (ts1 +++ ts2 +++ newTs)
translate (Star r) = do
  a <- (translate r)
  newI <- newNode ()
  let (t, i, ts) = ((terminal a), (initial a), (transitions a))
      edge = [(Eps, newI)]
      newTs = Map.fromList $ [(t', edge) | t' <- t] ++ [(newI, [(Eps, i)])]
   in return $
        makeNfa
          newI
          [newI]
          (ts +++ newTs)
translate (Plus r) = translate (Cat r (Star r))
translate (Range c1 c2) =
  if c1 > c2
    then error "Invalid range expression"
    else
      if c1 == c2
        then translate (Lit c1)
        else translate (Cat (Lit c1) (Range (succ c1) c2))

newNode :: () -> State Int Node
newNode () = do
  i <- get
  put (i + 1)
  return $ Node i

main :: IO () =
  let expr = Plus (Alt (Lit 'A') (Cat (Lit 'X') (Lit 'Y')))
   in let nfa = runState (translate expr) 0
       in putStrLn $ show nfa