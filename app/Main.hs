module Main (Main.main) where

import Lexer
import Graph
import Regex

data Token = ID String | NUM String | OP String  deriving (Show, Eq)

main :: IO ()
main = 
  let 
    rules =
      [ (ID, Plus (Range 'a' 'z'))
      , (NUM, Plus (Range '0' '9'))
      , (OP, Alt (Lit '+') (Alt (Lit '-') (Alt (Lit '*') (Lit '/'))))
      ]
    (nfa, t2t) = translateMany rules     -- Get the NFA and auxillary Map
    -- str = "aoeu+1023/bu+123"
  in do 
    str <- getLine                    -- String to parse
    Graph.showNFA nfa
    -- print $ t2t
    print $ Lexer.lex nfa t2t str