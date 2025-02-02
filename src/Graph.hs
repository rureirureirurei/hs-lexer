module Graph (showNFA) where 

import qualified Data.Map as Map
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Text.Lazy (unpack)
import NFA

type NFA_Graph = Gr String String  -- Graph with labeled nodes & edges

convertNFAtoGraph :: NFA -> NFA_Graph
convertNFAtoGraph nfa = mkGraph nodes edges
  where
    nodes = [(n, show n) | Node n <- Map.keys (transitions nfa)]  -- Node labels
    edges = [ (n1, n2, showTrans t)  -- Labeled edges
            | (Node n1, transList) <- Map.toList (transitions nfa)
            , (t, Node n2) <- transList
            ]
    
    showTrans NFA.Eps = "ε"
    showTrans (Transition c) = [c]

-- Convert the graph to an xdot string and write to file
writeXDot :: FilePath -> NFA_Graph -> IO ()
writeXDot filename g = writeFile filename (unpack (renderDot (toDot (graphToDot params g))))
  where
    params = nonClusteredParams { fmtEdge = \(_, _, l) -> [toLabel l] }

-- Generate an xdot file from NFA
showNFA :: NFA -> IO ()
showNFA nfa = writeXDot "nfa.xdot" (convertNFAtoGraph nfa)