{-# LANGUAGE NamedFieldPuns #-}

module Lexer (main) where

import Control.Monad.State
import qualified Data.Map as Map
import Regex
import NFA
import Graph

-- Regex to NFA --
translate :: Regex -> State Int NFA
translate Empty = do
  node <- newNode ()
  return $ nfa node [node] Map.empty
translate (Lit c) = do
  i <- newNode ()
  term <- newNode ()
  let transitions = Map.fromList [(i, [((Transition c), term)])]
  return $ nfa i [term] transitions
translate (Alt r1 r2) = do
  a1 <- (translate r1)
  a2 <- (translate r2)
  i0 <- newNode ()
  let (t1, i1, ts1) = ((terminal a1), (initial a1), (transitions a1))
      (t2, i2, ts2) = ((terminal a2), (initial a2), (transitions a2))
      edges = [(Eps, i1), (Eps, i2)]
      newTs = Map.fromList [(i0, edges)]
   in return $
        nfa
          i0
          (t1 ++ t2)
          (ts1 `joinTs` ts2 `joinTs` newTs)
translate (Cat r1 r2) = do
  a1 <- (translate r1)
  a2 <- (translate r2)
  let (t1, i1, ts1) = ((terminal a1), (initial a1), (transitions a1))
      (t2, i2, ts2) = ((terminal a2), (initial a2), (transitions a2))
      edge = [(Eps, i2)]
      newTs = Map.fromList [(t, edge) | t <- t1]
   in return $
        nfa
          i1
          t2
          (ts1 `joinTs` ts2 `joinTs` newTs)
translate (Star r) = do
  a <- (translate r)
  newI <- newNode ()
  let (t, i, ts) = ((terminal a), (initial a), (transitions a))
      edge = [(Eps, newI)]
      newTs = Map.fromList $ [(t', edge) | t' <- t] ++ [(newI, [(Eps, i)])]
   in return $
        nfa
          newI
          [newI]
          (ts `joinTs` newTs)
translate (Plus r) = translate (Cat r (Star r))
translate (Range c1 c2) =
  if c1 > c2
    then error "Invalid range expression"
    else
      if c1 == c2
        then translate (Lit c1)
        else translate (Cat (Lit c1) (Range (succ c1) c2))


translateMany ::  [(TestToken, Regex)] -> State Int NFA
translateMany rules = do
  start <- newNode ()
  nfas <- mapM (\(t, rgx) -> translate rgx) rules
  let 
    newEdges = Map.fromList $ [(start, map (\a -> (Eps, (initial a))) nfas )]
    allTerminals = foldl (\acc a -> acc ++ (terminal a)) [] nfas
    allTransitions = foldl (\acc a -> acc `joinTs` (transitions a)) Map.empty nfas
  return $ nfa start allTerminals (allTransitions `joinTs` newEdges)

newNode :: () -> State Int Node
newNode () = do
  i <- get
  put (i + 1)
  return $ Node i

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

