module Main (Main.main) where

import Lexer
import Graph
import Regex

data Token = ID | NUM | OP deriving Show

main :: IO ()
main = 
  let 
    rules =
      [ (ID, Plus (Range 'a' 'z'))
      , (NUM, Plus (Range '0' '9'))
      , (OP, Alt (Lit '+') (Alt (Lit '-') (Alt (Lit '*') (Cat (Lit '/') (Lit '/')))))
      ]
    (nfa, t2t) = translateMany rules  -- Get the NFA and auxillary Map
  in do 
    str <- getLine                    -- String to parse
    Graph.showNFA nfa
    -- print $ t2t
    print $ (show $ Lexer.lex nfa t2t str)