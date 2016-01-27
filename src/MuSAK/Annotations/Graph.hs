module MuSAK.Annotations.Graph where

import Data.Graph
import MuSAK.Annotations.Types

type Weight = Double

connectionThreshold :: Weight
connectionThreshold = 1.0

-- FIXME Should we retain the (end a) == (start b) check? It might
-- turn out that this check fails in the real data.
weight :: Mark -> Mark -> Weight
-- weight a b | end a == start b = asSeconds (time b) - asSeconds (time a)
--            | otherwise        = connectionThreshold
weight a b = asTime b - asTime a
  where asTime = asSeconds . time

pageGraph :: Page -> (Graph, Vertex -> (Point, PointKey, [PointKey]), PointKey -> Maybe Vertex)
pageGraph pg = graphFromEdges $ concat (edges marks)
  where
    marks = pg_marks pg
    edges (a:b:es) = (marksToEdge pg a (Just b)) : edges (b : es)
    edges (a:es)   = (marksToEdge pg a Nothing) : edges es
    edges []       = []

type PointKey = (FilePath, Point)

marksToEdge :: Page -> Mark -> Maybe Mark -> [(Point, PointKey, [PointKey])]
marksToEdge pg a (Just b)
  | weight a b < connectionThreshold = [ (st, stKey, [enKey])
                                       , (en, enKey, [stKey]) ]
  | otherwise                        = [ (st, stKey, [])
                                       , (en, enKey, []) ]
  where (st, stKey, en, enKey) = decomposeMark pg a

marksToEdge pg a Nothing = [ (st, stKey, [enKey])
                           , (en, enKey, [stKey]) ]
  where (st, stKey, en, enKey) = decomposeMark pg a

decomposeMark :: Page -> Mark -> (Point, PointKey, Point, PointKey)
decomposeMark pg m = (st, stKey, en, enKey)
  where
    st       = start m
    stKey    = (pageName, st)
    en       = end m
    enKey    = (pageName, en)
    pageName = pg_sourceFile pg

treeShape :: Tree Vertex -> Shape
treeShape t = undefined

shapes :: Page -> [Shape]
shapes pg = map treeShape $ components g
  where (g, _, _) = pageGraph pg
