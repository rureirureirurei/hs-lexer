{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lexer (Lexer.lex) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative ((<|>))
import NFA


-- Takes an NFA, a dictionary mapping terminal nodes to tokens, and a list of characters
-- Returns the list of tokens.
lex :: forall t. NFA -> Map.Map Node (String -> t) -> [Char] -> [t]
lex nfa terminal_to_token literals = 
  case aux (closure nfa (Set.singleton (initial nfa))) 0 of 
    Nothing       -> if (length literals == 0) then [] else error "Can't lex"
      -- No token found, return an empty list
    Just (t, n)   -> (t $ take n literals) : Lexer.lex nfa terminal_to_token (drop n literals)  -- Move forward `n` characters

  where 
    -- Recursive helper function to match the longest valid token
    aux :: Set.Set Node -> Int -> Maybe ((String -> t), Int)
    aux nodes n
      | n >= length literals = get_token_result n nodes  -- End of input
      | otherwise =
          let 
            next_nodes = closure nfa (ngoto nfa nodes (literals !! n))  -- Compute next states with closure
            try_next   = aux next_nodes (n + 1)  -- Try consuming another character
          in 
            try_next <|> get_token_result n nodes  -- Prefer longer matches

    -- Returns a token if any terminal state is reached
    -- get_token_result :: Int -> Set.Set Node -> Maybe (t, Int)
    get_token_result n nodes =
      case get_term_token nodes of
        Just token -> Just (token, n)
        Nothing    -> Nothing


    -- Computes the epsilon closure over the NFA.
    closure :: NFA -> Set.Set Node -> Set.Set Node
    closure a visited =
      let 
        edges = transitions a
        get_eps_transitions n = 
          [m | (Eps, m) <- Map.findWithDefault [] n edges]  -- Get only epsilon transitions
        new_nodes = Set.fromList (concatMap get_eps_transitions (Set.toList visited))
        all_nodes = visited `Set.union` new_nodes  -- Keep track of visited nodes
      in 
        if new_nodes `Set.isSubsetOf` visited
          then visited
          else closure a all_nodes


    -- Computes the next nodes in the nondeterministic step for a given input character.
    ngoto :: NFA -> Set.Set Node -> Char -> Set.Set Node
    ngoto a nodes l =
      let 
        edges = transitions a
        get_transitions n = [m | (Transition c, m) <- Map.findWithDefault [] n edges, c == l]
      in 
        Set.fromList (concatMap get_transitions (Set.toList nodes))


    -- Filters out the terminal nodes, and if there are some - returns the Token associated with the first one.
    get_term_token :: Set.Set Node -> Maybe (String -> t)
    get_term_token nodes = 
      case Set.lookupMin (Set.filter (`Map.member` terminal_to_token) nodes) of
        Nothing -> Nothing         -- No terminal nodes found
        Just t  -> Map.lookup t terminal_to_token  -- Return the first matching token


 