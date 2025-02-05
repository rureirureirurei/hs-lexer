{-# LANGUAGE NamedFieldPuns #-}

module Lexer (main) where

import Control.Monad.State
import Control.Monad (guard)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Regex
import NFA
import Graph


-- Auxillary function that merges two NFA's and adds new edges
mergeNFAs :: Node -> [Node] -> [(Node, [(Transition, Node)])] -> [NFA] -> NFA 
mergeNFAs start terminals newEdges nfas = 
  nfa 
    start 
    terminals 
    (foldl joinTs Map.empty ((Map.fromList newEdges) : (map transitions nfas)))


-- Auxillary function that gives new Node id
newNode :: State Int Node
newNode = do
  i <- get
  put (i + 1)
  return $ Node i


-- Translates Regular expression to NFA --
translate :: Regex -> State Int NFA

translate Empty = do
  node <- newNode
  return $ nfa node [node] Map.empty

translate (Lit c) = do
  i <- newNode
  t <- newNode
  return $ nfa i [t] (Map.singleton i [(Transition c, t)])

translate (Alt r1 r2) = do
  a1 <- translate r1
  a2 <- translate r2
  start <- newNode
  return $ mergeNFAs 
    start 
    (terminal a1 ++ terminal a2) 
    [(start, [(Eps, initial a1), (Eps, initial a2)])]
    [a1, a2]

translate (Cat r1 r2) = do
  a1 <- (translate r1)
  a2 <- (translate r2)
  return $ mergeNFAs 
    (initial a1)
    (terminal a2)
    [(t, [(Eps, initial a2)]) | t <- terminal a1]
    [a1, a2]

translate (Star r) = do
  a <- (translate r)
  start <- newNode
  return $ mergeNFAs
    start
    [start]
    ([(t', [(Eps, start)]) | t' <- terminal a] ++ [(start, [(Eps, initial a)])])
    [a]

translate (Plus r) = translate (Cat r (Star r))

translate (Range c1 c2)
  | c1 > c2   = error "Invalid range expression"
  | c1 == c2  = translate (Lit c1)
  | otherwise = translate (Cat (Lit c1) (Range (succ c1) c2))


 -- Takes list of rules and translates them to the single NFA instance
translateMany ::  [(Token, Regex)] -> NFA
translateMany rules = 
  let 
    mnfa = do
      start <- newNode
      nfas  <- mapM (\(t, rgx) -> translate rgx) rules
      let 
        newEdges       = Map.fromList $ [(start, map (\a -> (Eps, (initial a))) nfas )]
        allTerminals   = foldl (\acc a -> acc ++ (terminal a)) [] nfas
        allTransitions = foldl (\acc a -> acc `joinTs` (transitions a)) Map.empty nfas
      return $ nfa start allTerminals (allTransitions `joinTs` newEdges)
  in 
    fst $ runState mnfa 0


data Token = ID | NUM | OP deriving Show


-- Computes the epsilon closure over the NFA.
closure :: NFA -> [Node] -> [Node]
closure nfa nodes = Set.toList (closureSet nfa (Set.fromList nodes))

-- Helper function using a Set for efficient tracking
closureSet :: NFA -> Set.Set Node -> Set.Set Node
closureSet nfa visited =
  let 
    edges = transitions nfa
    get_eps_transitions n = 
      [m | (Eps, m) <- Map.findWithDefault [] n edges]  -- Get only epsilon transitions
    new_nodes = Set.fromList (concatMap get_eps_transitions (Set.toList visited))
    all_nodes = visited `Set.union` new_nodes  -- Keep track of visited nodes
  in 
    if new_nodes `Set.isSubsetOf` visited
      then visited
      else closureSet nfa all_nodes

-- Computes the next nodes in the nondeterministic step for a given input character.
ngoto :: NFA -> [Node] -> Char -> [Node]
ngoto nfa nodes l = Set.toList (ngotoSet nfa (Set.fromList nodes) l)

-- Helper function using a Set for efficient tracking
ngotoSet :: NFA -> Set.Set Node -> Char -> Set.Set Node
ngotoSet nfa visited l =
  let 
    edges = transitions nfa
    -- Get transitions for character `l` only (ignoring epsilon)
    get_transitions n = [m | (Transition c, m) <- Map.findWithDefault [] n edges, c == l]
    new_nodes = Set.fromList (concatMap get_transitions (Set.toList visited))
  in 
    new_nodes  -- No recursion needed, just one-step transitions

-- Filters out the terminal nodes, and if there are some - returns the Token associated with the first one.
get_term_token :: NFA -> [Node] -> (Map.Map Node Token) -> Maybe Token
get_term_token _ nodes terminal_to_token = 
  case filter (`Map.member` terminal_to_token) nodes of
    []     -> Nothing         -- No terminal nodes found
    (t:_)  -> Map.lookup t terminal_to_token  -- Return the first matching token

-- Takes an NFA, a dictionary mapping terminal nodes to tokens, and a list of characters
-- Returns the list of tokens.
lex :: NFA -> (Map.Map Node Token) -> [Char] -> [Token]
lex nfa terminal_to_token literals = 
  case aux (closure nfa [initial nfa]) 0 of 
    Nothing       -> []  -- No token found, return an empty list
    Just (t, n)   -> t : lex nfa terminal_to_token (drop n literals)  -- Move forward `n` characters

  where 
    -- Recursive helper function to match the longest valid token
    aux :: [Node] -> Int -> Maybe (Token, Int)
    aux nodes n
      | n >= length literals = get_token_result (length literals) nodes  -- End of input
      | otherwise =
          let 
            next_nodes = closure nfa (ngoto nfa nodes (literals !! n))  -- Compute next states with closure
            try_next   = aux next_nodes (n + 1)  -- Try consuming another character
          in 
            try_next <|> get_token_result n nodes  -- Prefer longer matches

    -- Returns a token if any terminal state is reached
    get_token_result :: [Node] -> Int -> Maybe (Token, Int)
    get_token_result nodes =
      case get_term_token nfa nodes terminal_to_token of
        Just token -> Just (token, n)
        Nothing    -> Nothing

main :: IO ()
main = 
  let 
    rules =
      [ (ID, Plus (Range 'a' 'b'))
      , (NUM, Plus (Range '0' '1'))
      , (OP, Alt (Lit '+') (Lit '-'))
      ]
    str = "a+b*10"
    nfa = translateMany rules
  in 
    print $ Lexer.lex nfa str 


