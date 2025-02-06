module Main (Main.main) where

import Lexer
import Graph
import Regex

data Token = ID | NUM | OP deriving Show

main :: IO ()
main = 
  let 
    rules =
      [ (id, Plus (Range 'a' 'z'))
      , (id, Plus (Range '0' '9'))
      , (id, Alt (Lit '+') (Alt (Lit '-') (Alt (Lit '*') (Lit '/'))))
      ]
    (nfa, t2t) = translateMany rules  -- Get the NFA and auxillary Map
    str = "aoeu +1023/bu+123"
  in do 
    -- str <- getLine                    -- String to parse
    Graph.showNFA nfa
    -- print $ t2t
    print $ Lexer.lex nfa t2t str