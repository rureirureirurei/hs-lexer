{-# LANGUAGE NamedFieldPuns #-}

module Lexer (main) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import Regex
import NFA
import qualified Graph (showNFA)


data Token = ID | NUM | OP deriving Show

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


main :: IO ()
main = 
  let 
    rules =
      [ (ID, Plus (Range 'a' 'z'))
      , (NUM, Plus (Range '0' '9'))
      , (OP, Alt (Lit '+') (Alt (Lit '-') (Alt (Lit '*') (Lit '/'))))
      ]
    str = "id+20*x-abc/20"
    (nfa, t2t) = translateMany rules
  in do 
    Graph.showNFA nfa
    print $ t2t
    print $ (show $ Lexer.lex nfa t2t str)


 