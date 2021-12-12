module Day12(day12a, day12b) where

import Data.Graph
import Common (split)
import Data.List (groupBy, sort)
import Data.Tuple (swap)
import Data.Maybe (fromJust, mapMaybe)
import Data.Array
import Data.Char (isUpper)

day12a :: IO Int
day12a = do
  edgeList <- readEdgeList "/Users/marksinke/IdeaProjects/aoc2021/data/day12input.txt"
  let undirectedEdgeList = sort (edgeList ++ map swap edgeList)
  let nodes = map toNode (groupBy fstEq undirectedEdgeList)
  let (graph, _, vertexFromKey) = graphFromEdges nodes
  let multiVisitVertices = mapMaybe vertexFromKey (filter isMultiVisit (map toNodeName nodes))
  let paths = findPathsA graph multiVisitVertices vertexFromKey
  return (length paths)

toNodeName :: (Int, String, [String]) -> String
toNodeName (_, name, _) = name

isMultiVisit :: String -> Bool
isMultiVisit str = isUpper (head str)

findPathsA :: Graph -> [Vertex] -> (String -> Maybe Vertex) -> [[Vertex]]
findPathsA graph multiVisitVertices vertexFromKey =
  let start = fromJust (vertexFromKey "start")
      end = fromJust (vertexFromKey "end")
  in findPathsReqA graph end multiVisitVertices [[start]] []

findPathsReqA :: Graph -> Vertex -> [Vertex] -> [[Vertex]] -> [[Vertex]] -> [[Vertex]]
findPathsReqA graph end multiVisitVertices pathsToExplore paths =
  if null pathsToExplore then paths
  else
    let curPath = head pathsToExplore
        rest = tail pathsToExplore
        cur = head curPath
        recurse = findPathsReqA graph end multiVisitVertices
  in
    if cur == end then recurse rest (curPath : paths)
    else
      let nextNodes = filter (isAvailableNodeA multiVisitVertices curPath) (graph ! cur)
      in if null nextNodes then recurse rest paths
        else recurse (rest ++ map (: curPath) nextNodes) paths

isAvailableNodeA :: [Vertex] -> [Vertex] -> Vertex -> Bool
isAvailableNodeA multiVisitVertices vertexPath vertex =
  vertex `elem` multiVisitVertices || vertex `notElem` vertexPath

fstEq :: (String, String) -> (String, String) -> Bool
fstEq (a1, _) (a2, _) = a1 == a2

toNode :: [(String, String)] -> (Int, String, [String])
toNode edgeList =
  (0, fst (head edgeList), map snd edgeList)

day12b :: IO Int
day12b = do
  return 0

readEdgeList :: FilePath -> IO [(String, String)]
readEdgeList filePath = do
  contents <- readFile filePath
  let myLines = lines contents
  return (map toEdge myLines)

toEdge :: String -> (String, String)
toEdge str =
  let parts = split '-' str
  in (head parts, parts !! 1)