{-# LANGUAGE NamedFieldPuns #-}

module Lexer (main) where

import Control.Monad.State
import qualified Data.Map as Map
import Regex
import NFA
import Graph

mergeNFAs :: Node -> [Node] -> [(Node, [(Transition, Node)])] -> [NFA] -> NFA 
mergeNFAs start terminals newEdges nfas = 
  nfa 
    start 
    terminals 
    (foldl joinTs Map.empty ((Map.fromList newEdges) : (map transitions nfas)))

-- Regex to NFA --
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



translateMany ::  [(TestToken, Regex)] -> State Int NFA
translateMany rules = do
  start <- newNode
  nfas <- mapM (\(t, rgx) -> translate rgx) rules
  let 
    newEdges = Map.fromList $ [(start, map (\a -> (Eps, (initial a))) nfas )]
    allTerminals = foldl (\acc a -> acc ++ (terminal a)) [] nfas
    allTransitions = foldl (\acc a -> acc `joinTs` (transitions a)) Map.empty nfas
  return $ nfa start allTerminals (allTransitions `joinTs` newEdges)


newNode :: State Int Node
newNode = do
  i <- get
  put (i + 1)
  return $ Node i

-- q :: 

data TestToken = ID | NUM | OP deriving Show

main :: IO ()
main = 
  let 
    rules =
        [ (ID, Plus (Range 'a' 'b')),
          (NUM, Plus (Range '0' '1')),
          (OP, Alt (Lit '+') (Lit '-'))
        ]
    (a, _) = runState (translateMany rules) 0
  in 
    showNFA a
    -- putStrLn $ show a

