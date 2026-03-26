-- | Isotropic remeshing for improved triangle quality.
--
-- Implements a simplified Botsch-Kobbelt style remeshing pipeline:
-- split long edges, collapse short edges, and apply Laplacian
-- smoothing. The mesh is converted to an intermediate indexed
-- representation for efficient local modifications, then rebuilt
-- with recomputed normals and tangents.
module NovaEngine.Mesh.Remesh
  ( -- * Remeshing
    remesh,
    remeshAdaptive,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import NovaEngine.Mesh.Combine (recomputeNormals, recomputeTangents)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Isotropic remeshing to a target edge length.
--
-- For each iteration the algorithm splits edges longer than
-- @(4\/3) * targetEdgeLength@, collapses edges shorter than
-- @(4\/5) * targetEdgeLength@, and applies one pass of Laplacian
-- smoothing. Normals and tangents are recomputed after all
-- iterations.
--
-- The target edge length is clamped to a positive minimum. Zero
-- or negative iterations return the mesh unchanged.
remesh ::
  -- | Target edge length
  Float ->
  -- | Number of iterations
  Int ->
  Mesh ->
  Mesh
remesh targetEdgeLength iterations mesh
  | iterations <= 0 = mesh
  | otherwise =
      let target = max minEdgeLength targetEdgeLength
          upper = splitRatio * target
          lower = collapseRatio * target
          rmesh0 = toRMesh mesh
          rmeshN = applyIterations iterations (remeshStep upper lower) rmesh0
       in fromRMesh rmeshN

-- | Adaptive remeshing with separate minimum and maximum edge lengths.
--
-- Edges longer than @maxLen@ are split; edges shorter than @minLen@
-- are collapsed. One Laplacian smoothing pass follows each
-- iteration. This gives more control than 'remesh' when the desired
-- edge length varies across the mesh.
remeshAdaptive ::
  -- | Minimum edge length (collapse threshold)
  Float ->
  -- | Maximum edge length (split threshold)
  Float ->
  -- | Number of iterations
  Int ->
  Mesh ->
  Mesh
remeshAdaptive minLen maxLen iterations mesh
  | iterations <= 0 = mesh
  | otherwise =
      let lower = max minEdgeLength minLen
          upper = max (lower + minEdgeLength) maxLen
          rmesh0 = toRMesh mesh
          rmeshN = applyIterations iterations (remeshStep upper lower) rmesh0
       in fromRMesh rmeshN

-- ----------------------------------------------------------------
-- Internal representation
-- ----------------------------------------------------------------

-- | Reduced vertex: position and UV only. Normal and tangent are
-- recomputed from geometry when converting back to 'Mesh'.
data RVertex = RVertex !V3 !V2

-- | Working mesh representation with IntMap-backed vertices and
-- triangles for efficient insertion and deletion during remeshing.
data RMesh = RMesh
  { rVertices :: !(IntMap RVertex),
    rTriangles :: !(IntMap (Int, Int, Int)),
    rNextVert :: !Int,
    rNextTri :: !Int
  }

-- ----------------------------------------------------------------
-- Conversion
-- ----------------------------------------------------------------

-- | Convert a 'Mesh' to the working representation.
toRMesh :: Mesh -> RMesh
toRMesh (Mesh verts idxs _) =
  let rverts =
        IntMap.fromList
          [ (i, RVertex (vPosition v) (vUV v))
          | (i, v) <- zip [0 ..] verts
          ]
      tris = buildTriMap 0 IntMap.empty (groupTriangles idxs)
   in RMesh
        { rVertices = rverts,
          rTriangles = tris,
          rNextVert = length verts,
          rNextTri = IntMap.size tris
        }
  where
    buildTriMap !_nextId !acc [] = acc
    buildTriMap !nextId !acc ((a, b, c) : rest) =
      buildTriMap
        (nextId + 1)
        (IntMap.insert nextId (fromIntegral a, fromIntegral b, fromIntegral c) acc)
        rest

-- | Convert the working representation back to a 'Mesh' with
-- recomputed normals and tangents.
fromRMesh :: RMesh -> Mesh
fromRMesh rmesh =
  recomputeTangents
    . recomputeNormals
    $ mkMesh outVerts outIdxs
  where
    -- Build a dense remapping from sparse vertex IDs to 0..n-1
    sortedVerts = IntMap.toAscList (rVertices rmesh)
    remapTable =
      IntMap.fromList
        (zipWith (\newIdx (oldIdx, _) -> (oldIdx, newIdx)) [0 :: Int ..] sortedVerts)
    outVerts =
      [ vertex pos (V3 0 1 0) uv (V4 1 0 0 1)
      | (_, RVertex pos uv) <- sortedVerts
      ]
    outIdxs = concatMap remapTri (IntMap.elems (rTriangles rmesh))
    remapTri (a, b, c) =
      case (IntMap.lookup a remapTable, IntMap.lookup b remapTable, IntMap.lookup c remapTable) of
        (Just ra, Just rb, Just rc) ->
          [fromIntegral ra, fromIntegral rb, fromIntegral rc :: Word32]
        _ -> []

-- ----------------------------------------------------------------
-- Remeshing steps
-- ----------------------------------------------------------------

-- | One full remeshing iteration: split, collapse, smooth.
remeshStep :: Float -> Float -> RMesh -> RMesh
remeshStep upperBound lowerBound =
  smoothVertices . collapseEdges lowerBound . splitEdges upperBound

-- | Split all edges longer than the given threshold.
--
-- Scans every triangle edge; when an edge exceeds the threshold a
-- midpoint vertex is inserted and the adjacent triangles are
-- bisected.
splitEdges :: Float -> RMesh -> RMesh
splitEdges threshold rmesh =
  let upperSq = threshold * threshold
      edges = collectAllEdges (rTriangles rmesh)
   in foldl' (trySplitEdge upperSq) rmesh edges

-- | Attempt to split a single edge if it exceeds the squared
-- length threshold.
trySplitEdge :: Float -> RMesh -> (Int, Int) -> RMesh
trySplitEdge upperSq rmesh (v0, v1) =
  case (IntMap.lookup v0 (rVertices rmesh), IntMap.lookup v1 (rVertices rmesh)) of
    (Just (RVertex p0 uv0), Just (RVertex p1 uv1)) ->
      let edgeSq = distanceSq p1 p0
       in if edgeSq > upperSq
            then
              let midPos = lerpFloat 0.5 `mapV3` (p0, p1)
                  midUV = lerpV2 0.5 uv0 uv1
                  midVert = RVertex midPos midUV
                  midId = rNextVert rmesh
                  -- Find all triangles containing this edge
                  trisWithEdge = findTrianglesWithEdge v0 v1 (rTriangles rmesh)
                  -- Insert midpoint vertex
                  rmesh1 =
                    rmesh
                      { rVertices = IntMap.insert midId midVert (rVertices rmesh),
                        rNextVert = midId + 1
                      }
               in -- Split each triangle containing the edge
                  foldl' (splitTriangleAtEdge v0 v1 midId) rmesh1 trisWithEdge
            else rmesh
    _ -> rmesh
  where
    mapV3 t (V3 ax ay az, V3 bx by bz) =
      V3 (t ax bx) (t ay by) (t az bz)

-- | Split a single triangle at the midpoint of a given edge.
--
-- The triangle @(v0, v1, v2)@ sharing edge @(eA, eB)@ is replaced
-- by two triangles: @(eA, mid, v2)@ and @(mid, eB, v2)@, where
-- @v2@ is the vertex opposite the split edge.
splitTriangleAtEdge :: Int -> Int -> Int -> RMesh -> Int -> RMesh
splitTriangleAtEdge eA eB midId rmesh triId =
  case IntMap.lookup triId (rTriangles rmesh) of
    Nothing -> rmesh
    Just (a, b, c) ->
      let opposite = findOpposite eA eB a b c
          nextTri = rNextTri rmesh
          -- Check if eA->eB follows the triangle's cyclic winding
          forward =
            (a == eA && b == eB)
              || (b == eA && c == eB)
              || (c == eA && a == eB)
          (tri1, tri2)
            | forward = ((eA, midId, opposite), (midId, eB, opposite))
            | otherwise = ((midId, eA, opposite), (eB, midId, opposite))
          newTris =
            IntMap.insert nextTri tri2
              . IntMap.insert triId tri1
              $ rTriangles rmesh
       in rmesh
            { rTriangles = newTris,
              rNextTri = nextTri + 1
            }

-- | Find the vertex in a triangle that is not part of the given
-- edge.
findOpposite :: Int -> Int -> Int -> Int -> Int -> Int
findOpposite eA eB a b c
  | a /= eA && a /= eB = a
  | b /= eA && b /= eB = b
  | otherwise = c

-- | Collapse all edges shorter than the given threshold.
--
-- For each short edge the two endpoints are merged to their
-- midpoint. All triangles referencing the removed vertex are
-- rewritten, and degenerate triangles are deleted.
collapseEdges :: Float -> RMesh -> RMesh
collapseEdges threshold rmesh =
  let lowerSq = threshold * threshold
      edges = collectAllEdges (rTriangles rmesh)
   in foldl' (tryCollapseEdge lowerSq) rmesh edges

-- | Attempt to collapse a single edge if it is shorter than the
-- squared length threshold.
tryCollapseEdge :: Float -> RMesh -> (Int, Int) -> RMesh
tryCollapseEdge lowerSq rmesh (v0, v1) =
  -- Vertices may have been removed by a prior collapse in this pass
  case (IntMap.lookup v0 (rVertices rmesh), IntMap.lookup v1 (rVertices rmesh)) of
    (Just (RVertex p0 uv0), Just (RVertex p1 uv1)) ->
      let edgeSq = distanceSq p1 p0
       in if edgeSq < lowerSq
            then
              let midPos = vlerp 0.5 p0 p1
                  midUV = lerpV2 0.5 uv0 uv1
                  midVert = RVertex midPos midUV
                  -- Replace v0 with midpoint, remove v1
                  newVerts =
                    IntMap.delete v1
                      . IntMap.insert v0 midVert
                      $ rVertices rmesh
                  -- Remap v1 -> v0 in all triangles
                  remappedTris =
                    IntMap.map (remapVertex v1 v0) (rTriangles rmesh)
                  -- Remove degenerate triangles
                  cleanTris =
                    IntMap.filter isNonDegenerate remappedTris
               in rmesh
                    { rVertices = newVerts,
                      rTriangles = cleanTris
                    }
            else rmesh
    _ -> rmesh

-- | One pass of Laplacian smoothing: move each vertex toward the
-- average position of its edge-connected neighbors with a fixed
-- blend weight of 0.5.
smoothVertices :: RMesh -> RMesh
smoothVertices rmesh =
  let adj = buildRMeshAdjacency (rTriangles rmesh)
      verts = rVertices rmesh
      smoothed = IntMap.mapWithKey (smoothOneVertex adj verts) verts
   in rmesh {rVertices = smoothed}

-- | Smooth a single vertex by blending toward its neighbor average.
smoothOneVertex :: IntMap IntSet.IntSet -> IntMap RVertex -> Int -> RVertex -> RVertex
smoothOneVertex adj verts vid rv@(RVertex pos uv) =
  case IntMap.lookup vid adj of
    Nothing -> rv
    Just neighbors
      | IntSet.null neighbors -> rv
      | otherwise ->
          let nCount = fromIntegral (IntSet.size neighbors) :: Float
              invCount = 1.0 / nCount
              sumPos =
                IntSet.foldl'
                  ( \acc nid -> case IntMap.lookup nid verts of
                      Just (RVertex np _) -> acc ^+^ np
                      Nothing -> acc
                  )
                  vzero
                  neighbors
              avgPos = invCount *^ sumPos
              blendedPos = vlerp smoothingWeight pos avgPos
           in RVertex blendedPos uv

-- ----------------------------------------------------------------
-- Edge and adjacency helpers
-- ----------------------------------------------------------------

-- | Collect all unique edges from the triangle map. Edges are
-- stored with the smaller index first.
collectAllEdges :: IntMap (Int, Int, Int) -> [(Int, Int)]
collectAllEdges tris =
  IntSet.foldl'
    (\acc packed -> unpackEdge packed : acc)
    []
    edgeSet
  where
    edgeSet = IntMap.foldl' insertTriEdges IntSet.empty tris
    insertTriEdges !acc (a, b, c) =
      IntSet.insert (packEdge a b)
        . IntSet.insert (packEdge b c)
        . IntSet.insert (packEdge a c)
        $ acc

-- | Pack an edge as a single Int for IntSet storage. The smaller
-- index goes into the upper 32 bits.
packEdge :: Int -> Int -> Int
packEdge a b
  | a <= b = shiftL a 32 .|. b
  | otherwise = shiftL b 32 .|. a

-- | Unpack an edge from its packed representation.
unpackEdge :: Int -> (Int, Int)
unpackEdge packed = (shiftR packed 32, packed .&. 0xFFFFFFFF)

-- | Find all triangle IDs that contain the given edge.
findTrianglesWithEdge :: Int -> Int -> IntMap (Int, Int, Int) -> [Int]
findTrianglesWithEdge eA eB =
  IntMap.foldlWithKey'
    ( \acc tid (a, b, c) ->
        if triangleHasEdge eA eB a b c
          then tid : acc
          else acc
    )
    []

-- | Check whether a triangle contains a specific edge (in any
-- winding).
triangleHasEdge :: Int -> Int -> Int -> Int -> Int -> Bool
triangleHasEdge eA eB a b c =
  hasVert eA && hasVert eB
  where
    hasVert v = v == a || v == b || v == c

-- | Build an adjacency map from the working triangle set. Maps
-- each vertex ID to the set of its edge-connected neighbor IDs.
buildRMeshAdjacency :: IntMap (Int, Int, Int) -> IntMap IntSet.IntSet
buildRMeshAdjacency = IntMap.foldl' insertAdj IntMap.empty
  where
    insertAdj !acc (a, b, c) =
      addPair a b
        . addPair b a
        . addPair a c
        . addPair c a
        . addPair b c
        . addPair c b
        $ acc
    addPair from to =
      IntMap.insertWith IntSet.union from (IntSet.singleton to)

-- | Remap a vertex index within a triangle.
remapVertex :: Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
remapVertex from to (a, b, c) = (rw a, rw b, rw c)
  where
    rw v
      | v == from = to
      | otherwise = v

-- | Check that a triangle has three distinct vertices.
isNonDegenerate :: (Int, Int, Int) -> Bool
isNonDegenerate (a, b, c) = a /= b && b /= c && a /= c

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Linear interpolation of two 'V2' values.
lerpV2 :: Float -> V2 -> V2 -> V2
lerpV2 t (V2 ax ay) (V2 bx by) =
  V2 (ax + t * (bx - ax)) (ay + t * (by - ay))

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Edge split ratio: edges longer than @(4\/3) * target@ are
-- split.
splitRatio :: Float
splitRatio = 4.0 / 3.0

-- | Edge collapse ratio: edges shorter than @(4\/5) * target@ are
-- collapsed.
collapseRatio :: Float
collapseRatio = 4.0 / 5.0

-- | Minimum allowed edge length to prevent degenerate behavior.
minEdgeLength :: Float
minEdgeLength = 1.0e-6

-- | Laplacian smoothing blend weight.
smoothingWeight :: Float
smoothingWeight = 0.5
