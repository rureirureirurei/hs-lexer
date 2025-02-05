{-# LANGUAGE NamedFieldPuns #-}

module Lexer (main) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Regex
import NFA
import qualified Graph (showNFA)


-- Auxillary function that merges two NFA's and adds new edges
mergeNFAs :: Node -> [Node] -> [(Node, [(Transition, Node)])] -> [NFA] -> NFA 
mergeNFAs start terminals newEdges nfas = 
 mkNfa 
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
  return $ mkNfa node [node] Map.empty

translate (Lit c) = do
  i <- newNode
  t <- newNode
  return $ mkNfa i [t] (Map.singleton i [(Transition c, t)])

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
  | otherwise = translate $ foldl (Alt) (Lit c1) (map Lit [succ c1 .. c2])
  

-- Takes a list of rules and translates them into a single NFA instance along with a terminal-to-token map
translateMany :: [(Token, Regex)] -> (NFA, Map.Map Node Token)
translateMany rules = fst $ runState buildNFA 0
  where 
    buildNFA = do
      start <- newNode
      nfas  <- mapM (translate . snd) rules  -- Translate regexes to NFAs

      let 
        newEdges        = Map.singleton start [(Eps, initial a) | a <- nfas]
        allTerminals    = concatMap terminal nfas
        terminalToToken = Map.fromList [(n, t) | (t, a) <- zip (map fst rules) nfas, n <- terminal a]
        allTransitions  = foldl joinTs Map.empty (map transitions nfas)

      return $ (mkNfa start allTerminals (allTransitions `joinTs` newEdges), terminalToToken)


data Token = ID | NUM | OP deriving Show

-- Computes the epsilon closure over the NFA.
closure :: NFA -> Set.Set Node -> Set.Set Node
closure nfa visited =
  let 
    edges = transitions nfa
    get_eps_transitions n = 
      [m | (Eps, m) <- Map.findWithDefault [] n edges]  -- Get only epsilon transitions
    new_nodes = Set.fromList (concatMap get_eps_transitions (Set.toList visited))
    all_nodes = visited `Set.union` new_nodes  -- Keep track of visited nodes
  in 
    if new_nodes `Set.isSubsetOf` visited
      then visited
      else closure nfa all_nodes


-- Computes the next nodes in the nondeterministic step for a given input character.
ngoto :: NFA -> Set.Set Node -> Char -> Set.Set Node
ngoto nfa nodes l =
  let 
    edges = transitions nfa
    get_transitions n = [m | (Transition c, m) <- Map.findWithDefault [] n edges, c == l]
  in 
    Set.fromList (concatMap get_transitions (Set.toList nodes))


-- Filters out the terminal nodes, and if there are some - returns the Token associated with the first one.
get_term_token :: NFA -> Set.Set Node -> Map.Map Node Token -> Maybe Token
get_term_token _ nodes terminal_to_token = 
  case Set.lookupMin (Set.filter (`Map.member` terminal_to_token) nodes) of
    Nothing -> Nothing         -- No terminal nodes found
    Just t  -> Map.lookup t terminal_to_token  -- Return the first matching token


-- Takes an NFA, a dictionary mapping terminal nodes to tokens, and a list of characters
-- Returns the list of tokens.
lex :: NFA -> Map.Map Node Token -> [Char] -> [Token]
lex nfa terminal_to_token literals = 
  case aux (closure nfa (Set.singleton (initial nfa))) 0 of 
    Nothing       -> []  -- No token found, return an empty list
    Just (t, n)   -> t : Lexer.lex nfa terminal_to_token (drop n literals)  -- Move forward `n` characters

  where 
    -- Recursive helper function to match the longest valid token
    aux :: Set.Set Node -> Int -> Maybe (Token, Int)
    aux nodes n
      | n >= length literals = get_token_result n nodes  -- End of input
      | otherwise =
          let 
            next_nodes = closure nfa (ngoto nfa nodes (literals !! n))  -- Compute next states with closure
            try_next   = aux next_nodes (n + 1)  -- Try consuming another character
          in 
            try_next <|> get_token_result n nodes  -- Prefer longer matches

    -- Returns a token if any terminal state is reached
    get_token_result :: Int -> Set.Set Node -> Maybe (Token, Int)
    get_token_result n nodes =
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
    str = "a+b-10"
    (nfa, t2t) = translateMany rules
  in do 
    Graph.showNFA nfa
    print $ t2t
    print $ (show $ Lexer.lex nfa t2t str)


 