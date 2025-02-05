module Regex (Regex(..), translateMany) where 

import Control.Monad.State
import qualified Data.Map as Map
import NFA

data Regex
    = Empty
    | Cat Regex Regex
    | Alt Regex Regex
    | Lit Char
    | Star Regex
    | Plus Regex
    | Range Char Char 
  deriving Show


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
translateMany :: (Read t) => [(t, Regex)] -> (NFA, Map.Map Node t)
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