module Regex (Regex(..)) where 

data Regex
    = Empty
    | Cat Regex Regex
    | Alt Regex Regex
    | Lit Char
    | Star Regex
    | Plus Regex
    | Range Char Char 
  deriving Show
