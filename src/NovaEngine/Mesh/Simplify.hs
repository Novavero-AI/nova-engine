-- | Mesh simplification via quadric error metrics.
--
-- Implementation of the Garland-Heckbert (1997) algorithm. For each
-- vertex a symmetric 4x4 quadric matrix is accumulated from adjacent
-- face plane equations. Edges are collapsed in order of increasing
-- quadric error until the desired triangle count is reached.
module NovaEngine.Mesh.Simplify
  ( -- * Simplification
    simplify,
    simplifyRatio,

    -- * Single edge collapse
    decimateEdge,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import NovaEngine.Mesh.Combine (recomputeNormals, recomputeTangents)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Simplify a mesh to approximately @targetTriangles@ triangles
-- using quadric error metrics.
--
-- The target is clamped to at least 1. If the mesh already has
-- fewer or equal triangles, it is returned unchanged. Normals and
-- tangents are recomputed after simplification.
simplify :: Int -> Mesh -> Mesh
simplify targetTriangles mesh
  | currentTriangles <= clampedTarget = mesh
  | otherwise =
      recomputeTangents
        . recomputeNormals
        . collapseStateToMesh
        . collapseLoop clampedTarget
        . buildCollapseState
        $ mesh
  where
    currentTriangles = length (meshIndices mesh) `div` indicesPerTriangle
    clampedTarget = max minimumTriangleTarget targetTriangles

-- | Simplify to a fraction of the original triangle count.
--
-- The ratio is clamped to @(0, 1]@. A ratio of @1.0@ returns the
-- mesh unchanged; a ratio of @0.5@ targets half as many triangles.
simplifyRatio :: Float -> Mesh -> Mesh
simplifyRatio ratio mesh =
  let clampedRatio = max minimumSimplifyRatio (min maximumSimplifyRatio ratio)
      currentTriangles = length (meshIndices mesh) `div` indicesPerTriangle
      targetTriangles = max minimumTriangleTarget (round (clampedRatio * fromIntegral currentTriangles))
   in simplify targetTriangles mesh

-- | Collapse a single specific edge, merging its two vertices.
--
-- If the edge does not exist in the mesh the mesh is returned
-- unchanged. Normals and tangents are recomputed after collapse.
decimateEdge :: Mesh -> (Word32, Word32) -> Mesh
decimateEdge mesh (vertA, vertB) =
  let state = buildCollapseState mesh
      edgeCanonical = canonicalEdge vertA vertB
   in case Map.lookup edgeCanonical (csEdgeCosts state) of
        Nothing -> mesh
        Just _ ->
          recomputeTangents
            . recomputeNormals
            . collapseStateToMesh
            . collapseEdge edgeCanonical
            $ state

-- ----------------------------------------------------------------
-- Quadric representation
-- ----------------------------------------------------------------

-- | A symmetric 4x4 matrix stored as 10 unique floats.
--
-- Layout:
--
-- > | a11 a12 a13 a14 |
-- > | a12 a22 a23 a24 |
-- > | a13 a23 a33 a34 |
-- > | a14 a24 a34 a44 |
data Quadric = Quadric
  { q11 :: !Float,
    q12 :: !Float,
    q13 :: !Float,
    q14 :: !Float,
    q22 :: !Float,
    q23 :: !Float,
    q24 :: !Float,
    q33 :: !Float,
    q34 :: !Float,
    q44 :: !Float
  }

-- | The zero quadric (all entries zero).
zeroQuadric :: Quadric
zeroQuadric = Quadric 0 0 0 0 0 0 0 0 0 0

-- | Add two quadrics element-wise.
addQuadric :: Quadric -> Quadric -> Quadric
addQuadric qa qb =
  Quadric
    { q11 = q11 qa + q11 qb,
      q12 = q12 qa + q12 qb,
      q13 = q13 qa + q13 qb,
      q14 = q14 qa + q14 qb,
      q22 = q22 qa + q22 qb,
      q23 = q23 qa + q23 qb,
      q24 = q24 qa + q24 qb,
      q33 = q33 qa + q33 qb,
      q34 = q34 qa + q34 qb,
      q44 = q44 qa + q44 qb
    }

-- | Evaluate the quadric error for a position: @v^T Q v@.
--
-- The position is treated as a homogeneous vector @(x, y, z, 1)@.
evalQuadricError :: Quadric -> V3 -> Float
evalQuadricError q (V3 px py pz) =
  q11 q * px * px
    + quadricSymmetricWeight * q12 q * px * py
    + quadricSymmetricWeight * q13 q * px * pz
    + quadricSymmetricWeight * q14 q * px
    + q22 q * py * py
    + quadricSymmetricWeight * q23 q * py * pz
    + quadricSymmetricWeight * q24 q * py
    + q33 q * pz * pz
    + quadricSymmetricWeight * q34 q * pz
    + q44 q

-- | Build a quadric from a plane equation @(a, b, c, d)@ where
-- @ax + by + cz + d = 0@.
--
-- The quadric is the outer product of the plane vector with itself.
planeQuadric :: Float -> Float -> Float -> Float -> Quadric
planeQuadric a b c d =
  Quadric
    { q11 = a * a,
      q12 = a * b,
      q13 = a * c,
      q14 = a * d,
      q22 = b * b,
      q23 = b * c,
      q24 = b * d,
      q33 = c * c,
      q34 = c * d,
      q44 = d * d
    }

-- | Compute the optimal vertex position that minimizes @v^T Q v@.
--
-- Solves the 3x3 linear system obtained by taking the partial
-- derivatives. If the system is near-singular (determinant below
-- threshold), falls back to the given fallback position.
optimalVertex :: Quadric -> V3 -> V3
optimalVertex q fallback =
  let -- The 3x3 system: upper-left block of Q times [x,y,z]^T = -[q14,q24,q34]^T
      a = q11 q
      b = q12 q
      c = q13 q
      d = q12 q
      e = q22 q
      f = q23 q
      g = q13 q
      h = q23 q
      k = q33 q
      detVal =
        a * (e * k - f * h)
          - b * (d * k - f * g)
          + c * (d * h - e * g)
   in if abs detVal < singularDeterminantThreshold
        then fallback
        else
          let invDet = 1.0 / detVal
              rhsX = negate (q14 q)
              rhsY = negate (q24 q)
              rhsZ = negate (q34 q)
              solX =
                invDet
                  * ( rhsX * (e * k - f * h)
                        + rhsY * (c * h - b * k)
                        + rhsZ * (b * f - c * e)
                    )
              solY =
                invDet
                  * ( rhsX * (f * g - d * k)
                        + rhsY * (a * k - c * g)
                        + rhsZ * (c * d - a * f)
                    )
              solZ =
                invDet
                  * ( rhsX * (d * h - e * g)
                        + rhsY * (b * g - a * h)
                        + rhsZ * (a * e - b * d)
                    )
           in V3 solX solY solZ

-- ----------------------------------------------------------------
-- Collapse state
-- ----------------------------------------------------------------

-- | Mutable-ish state threaded through the collapse loop.
--
-- Vertices are stored in an 'IntMap' so that removed vertices leave
-- no gaps. Triangles reference vertex IDs that index into the map.
data CollapseState = CollapseState
  { csVertices :: !(IntMap Vertex),
    csTriangles :: ![(Word32, Word32, Word32)],
    csTriangleCount :: !Int,
    csQuadrics :: !(IntMap Quadric),
    csEdgeCosts :: !(Map (Word32, Word32) EdgeCost),
    -- | Cost-ordered index: cost -> list of edges with that cost.
    -- 'Map.findMin' yields the cheapest edge in O(log n).
    csCostQueue :: !(Map Float [(Word32, Word32)])
  }

-- | Precomputed cost and optimal position for an edge collapse.
data EdgeCost = EdgeCost
  { ecError :: !Float,
    ecOptimalPos :: !V3
  }

-- | Build the initial collapse state from a mesh.
buildCollapseState :: Mesh -> CollapseState
buildCollapseState (Mesh vertices indices _count) =
  let vertMap = IntMap.fromList (zip [0 ..] vertices)
      triangles = groupTriangles indices
      quadrics = computeAllQuadrics vertMap triangles
      edgeCosts = computeAllEdgeCosts vertMap quadrics triangles
      costQueue = buildCostQueue edgeCosts
   in CollapseState
        { csVertices = vertMap,
          csTriangles = triangles,
          csTriangleCount = length triangles,
          csQuadrics = quadrics,
          csEdgeCosts = edgeCosts,
          csCostQueue = costQueue
        }

-- | Convert collapse state back to a mesh.
--
-- Vertices are compacted into a dense list and indices are
-- remapped accordingly.
collapseStateToMesh :: CollapseState -> Mesh
collapseStateToMesh state =
  mkMesh compactVertices compactIndices
  where
    -- Build a remapping from sparse vertex IDs to dense [0..]
    sparseVertices = IntMap.toAscList (csVertices state)
    remapTable =
      IntMap.fromList
        (zipWith (\newIdx (oldIdx, _) -> (oldIdx, fromIntegral newIdx :: Word32)) [0 :: Int ..] sparseVertices)
    compactVertices = map snd sparseVertices
    compactIndices = concatMap remapTriangle (csTriangles state)

    remapTriangle (idx0, idx1, idx2) =
      case (lookupRemap idx0, lookupRemap idx1, lookupRemap idx2) of
        (Just r0, Just r1, Just r2) -> [r0, r1, r2]
        _ -> []

    lookupRemap idx = IntMap.lookup (fromIntegral idx) remapTable

-- ----------------------------------------------------------------
-- Collapse loop
-- ----------------------------------------------------------------

-- | Repeatedly collapse the cheapest edge until the triangle count
-- reaches the target.  Uses the cost-ordered queue for O(log E)
-- minimum finding instead of scanning all edges.
collapseLoop :: Int -> CollapseState -> CollapseState
collapseLoop targetTriangles state
  | csTriangleCount state <= targetTriangles = state
  | Map.null (csCostQueue state) = state
  | otherwise =
      case findCheapestEdge state of
        Nothing -> state
        Just (cheapestEdge, cheapestCost) ->
          collapseLoop
            targetTriangles
            (collapseEdgeWithPos cheapestEdge (ecOptimalPos cheapestCost) state)

-- | Find the cheapest valid edge from the cost queue, skipping
-- stale entries that are no longer in 'csEdgeCosts'.
findCheapestEdge :: CollapseState -> Maybe ((Word32, Word32), EdgeCost)
findCheapestEdge state = go (csCostQueue state)
  where
    edgeCosts = csEdgeCosts state
    go queue
      | Map.null queue = Nothing
      | otherwise =
          let ((_cost, edges), restQueue) = Map.deleteFindMin queue
           in findValid edges restQueue
    findValid [] restQueue = go restQueue
    findValid (edge : remaining) restQueue =
      case Map.lookup edge edgeCosts of
        Just ec -> Just (edge, ec)
        Nothing -> findValid remaining restQueue

-- | Collapse a single edge, computing the optimal position from
-- the combined quadric. Used by 'decimateEdge'.
collapseEdge :: (Word32, Word32) -> CollapseState -> CollapseState
collapseEdge (vertA, vertB) state =
  let vertexMap = csVertices state
      quadricMap = csQuadrics state
      quadricA = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertA) quadricMap)
      quadricB = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertB) quadricMap)
      combinedQuadric = addQuadric quadricA quadricB
      posA = maybe vzero vPosition (IntMap.lookup (fromIntegral vertA) vertexMap)
      posB = maybe vzero vPosition (IntMap.lookup (fromIntegral vertB) vertexMap)
      midpoint = edgeMidpointWeight *^ (posA ^+^ posB)
      optimalPos = optimalVertex combinedQuadric midpoint
   in collapseEdgeWithPos (vertA, vertB) optimalPos state

-- | Collapse a single edge, merging vertex B into vertex A at the
-- given optimal position.
--
-- Steps:
--   1. Place the merged vertex at the optimal position
--   2. Move vertex A to the optimal position; remove vertex B
--   3. Rewrite all index references from B to A
--   4. Remove degenerate triangles (two or more identical vertices)
--   5. Recompute quadrics and edge costs for affected edges
collapseEdgeWithPos :: (Word32, Word32) -> V3 -> CollapseState -> CollapseState
collapseEdgeWithPos (vertA, vertB) optimalPos state =
  let vertexMap = csVertices state
      quadricMap = csQuadrics state

      -- Combined quadric for the merged vertex
      quadricA = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertA) quadricMap)
      quadricB = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertB) quadricMap)
      combinedQuadric = addQuadric quadricA quadricB

      -- Interpolate vertex attributes (position is overridden by
      -- the optimal quadric solution)
      mergedVertex = interpolateVertices vertexMap vertA vertB optimalPos

      -- Update vertex map: replace A, remove B
      updatedVertMap =
        IntMap.delete (fromIntegral vertB)
          . IntMap.insert (fromIntegral vertA) mergedVertex
          $ vertexMap

      -- Update quadric map: replace A, remove B
      updatedQuadricMap =
        IntMap.delete (fromIntegral vertB)
          . IntMap.insert (fromIntegral vertA) combinedQuadric
          $ quadricMap

      -- Rewrite indices: replace all references to B with A
      rewrittenTriangles =
        map (rewriteIndex vertB vertA) (csTriangles state)

      -- Remove degenerate triangles (collapsed to a line or point)
      cleanTriangles = filter isNonDegenerate rewrittenTriangles

      -- Collect vertices adjacent to the collapsed edge for cost
      -- recomputation
      affectedVertices = collectAffectedVertices cleanTriangles vertA

      -- Start from costs with stale edges removed
      baseCosts = removeStaleEdges vertA vertB (csEdgeCosts state)

      -- Rebuild edge costs only for affected edges
      updatedEdgeCosts =
        recomputeAffectedEdgeCosts
          updatedVertMap
          updatedQuadricMap
          cleanTriangles
          affectedVertices
          baseCosts

      -- Rebuild cost queue from the updated edge costs
      updatedCostQueue = buildCostQueue updatedEdgeCosts
   in CollapseState
        { csVertices = updatedVertMap,
          csTriangles = cleanTriangles,
          csTriangleCount = length cleanTriangles,
          csQuadrics = updatedQuadricMap,
          csEdgeCosts = updatedEdgeCosts,
          csCostQueue = updatedCostQueue
        }

-- ----------------------------------------------------------------
-- Quadric computation
-- ----------------------------------------------------------------

-- | Compute per-vertex quadrics by summing plane quadrics from all
-- adjacent faces.
computeAllQuadrics ::
  IntMap Vertex ->
  [(Word32, Word32, Word32)] ->
  IntMap Quadric
computeAllQuadrics vertMap =
  foldl' accumulateTriangleQuadric emptyQuadricMap
  where
    emptyQuadricMap =
      IntMap.map (const zeroQuadric) vertMap

    accumulateTriangleQuadric !acc (idx0, idx1, idx2) =
      case lookupTrianglePositions vertMap idx0 idx1 idx2 of
        Nothing -> acc
        Just (pos0, pos1, pos2) ->
          let faceQuadric = trianglePlaneQuadric pos0 pos1 pos2
           in addQuadricAtVertex idx0 faceQuadric
                . addQuadricAtVertex idx1 faceQuadric
                . addQuadricAtVertex idx2 faceQuadric
                $ acc

    addQuadricAtVertex idx =
      IntMap.insertWith addQuadric (fromIntegral idx)

-- | Compute the plane quadric for a triangle.
--
-- Returns the zero quadric for degenerate (collinear) triangles
-- whose normal length is below threshold.
trianglePlaneQuadric :: V3 -> V3 -> V3 -> Quadric
trianglePlaneQuadric pos0 pos1 pos2 =
  let edge1 = pos1 ^-^ pos0
      edge2 = pos2 ^-^ pos0
      rawNormal = cross edge1 edge2
      normalLength = vlength rawNormal
   in if normalLength < degenerateTriangleThreshold
        then zeroQuadric
        else
          let V3 px py pz = pos0
              V3 nx ny nz = (1.0 / normalLength) *^ rawNormal
              planeD = negate (nx * px + ny * py + nz * pz)
           in planeQuadric nx ny nz planeD

-- ----------------------------------------------------------------
-- Edge cost computation
-- ----------------------------------------------------------------

-- | Build the cost-ordered queue from the edge cost map.
-- Maps each cost to the list of edges with that cost.
buildCostQueue :: Map (Word32, Word32) EdgeCost -> Map Float [(Word32, Word32)]
buildCostQueue =
  Map.foldlWithKey' insertEdge Map.empty
  where
    insertEdge !acc edge ec =
      Map.insertWith (++) (ecError ec) [edge] acc

-- | Compute initial edge costs for all edges in the mesh.
computeAllEdgeCosts ::
  IntMap Vertex ->
  IntMap Quadric ->
  [(Word32, Word32, Word32)] ->
  Map (Word32, Word32) EdgeCost
computeAllEdgeCosts vertMap quadricMap =
  foldl' addTriangleEdges Map.empty
  where
    addTriangleEdges !acc (idx0, idx1, idx2) =
      let edge01 = canonicalEdge idx0 idx1
          edge12 = canonicalEdge idx1 idx2
          edge02 = canonicalEdge idx0 idx2
       in insertEdgeCost edge02 (insertEdgeCost edge12 (insertEdgeCost edge01 acc))

    insertEdgeCost edgeCanon acc
      | Map.member edgeCanon acc = acc
      | otherwise =
          case computeEdgeCost vertMap quadricMap edgeCanon of
            Nothing -> acc
            Just cost -> Map.insert edgeCanon cost acc

-- | Compute the collapse cost for a single edge.
computeEdgeCost ::
  IntMap Vertex ->
  IntMap Quadric ->
  (Word32, Word32) ->
  Maybe EdgeCost
computeEdgeCost vertMap quadricMap (vertA, vertB) = do
  vA <- IntMap.lookup (fromIntegral vertA) vertMap
  vB <- IntMap.lookup (fromIntegral vertB) vertMap
  let quadricA = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertA) quadricMap)
      quadricB = fromMaybe zeroQuadric (IntMap.lookup (fromIntegral vertB) quadricMap)
      combined = addQuadric quadricA quadricB
      midpoint = edgeMidpointWeight *^ (vPosition vA ^+^ vPosition vB)
      optimal = optimalVertex combined midpoint
      cost = evalQuadricError combined optimal
  pure (EdgeCost cost optimal)

-- | Recompute edge costs for edges touching any of the affected
-- vertices. Stale edges (referencing removed vertices) are
-- excluded.
recomputeAffectedEdgeCosts ::
  IntMap Vertex ->
  IntMap Quadric ->
  [(Word32, Word32, Word32)] ->
  [Word32] ->
  Map (Word32, Word32) EdgeCost ->
  Map (Word32, Word32) EdgeCost
recomputeAffectedEdgeCosts vertMap quadricMap triangles affectedVerts baseCosts =
  let affectedEdges = collectEdgesForVertices triangles affectedVerts
   in foldl' recomputeOne baseCosts affectedEdges
  where
    recomputeOne !acc edgeCanon =
      case computeEdgeCost vertMap quadricMap edgeCanon of
        Nothing -> Map.delete edgeCanon acc
        Just cost -> Map.insert edgeCanon cost acc

-- | Remove edge costs that reference either of the two collapsed
-- vertices. The merged vertex (vertA) edges will be recomputed
-- separately.
removeStaleEdges ::
  Word32 ->
  Word32 ->
  Map (Word32, Word32) EdgeCost ->
  Map (Word32, Word32) EdgeCost
removeStaleEdges vertA vertB =
  Map.filterWithKey (\(ea, eb) _ -> not (touchesVertex ea eb))
  where
    touchesVertex ea eb =
      ea == vertA || eb == vertA || ea == vertB || eb == vertB

-- ----------------------------------------------------------------
-- Adjacency helpers
-- ----------------------------------------------------------------

-- | Canonical edge key with the smaller index first.
canonicalEdge :: Word32 -> Word32 -> (Word32, Word32)
canonicalEdge a b
  | a <= b = (a, b)
  | otherwise = (b, a)

-- | Rewrite a single index in a triangle.
rewriteIndex ::
  Word32 ->
  Word32 ->
  (Word32, Word32, Word32) ->
  (Word32, Word32, Word32)
rewriteIndex from to (a, b, c) =
  (rw a, rw b, rw c)
  where
    rw idx
      | idx == from = to
      | otherwise = idx

-- | Check that a triangle has three distinct vertices.
isNonDegenerate :: (Word32, Word32, Word32) -> Bool
isNonDegenerate (a, b, c) = a /= b && b /= c && a /= c

-- | Collect all vertex IDs adjacent to a given vertex in the
-- triangle list.
collectAffectedVertices ::
  [(Word32, Word32, Word32)] ->
  Word32 ->
  [Word32]
collectAffectedVertices triangles targetVert =
  map fromIntegral (IntMap.keys neighborSet)
  where
    neighborSet :: IntMap ()
    neighborSet = foldl' addNeighbors IntMap.empty triangles

    addNeighbors !acc (a, b, c)
      | a == targetVert = insertVert b (insertVert c acc)
      | b == targetVert = insertVert a (insertVert c acc)
      | c == targetVert = insertVert a (insertVert b acc)
      | otherwise = acc

    insertVert v = IntMap.insert (fromIntegral v) ()

-- | Collect all unique edges that touch any vertex in the given
-- list from the triangle list.
collectEdgesForVertices ::
  [(Word32, Word32, Word32)] ->
  [Word32] ->
  [(Word32, Word32)]
collectEdgesForVertices triangles affectedVerts =
  Map.keys edgeSet
  where
    vertSet :: IntMap ()
    vertSet = IntMap.fromList [(fromIntegral v, ()) | v <- affectedVerts]

    isAffected :: Word32 -> Bool
    isAffected v = IntMap.member (fromIntegral v) vertSet

    edgeSet :: Map (Word32, Word32) ()
    edgeSet = foldl' addTriangleEdgesIfAffected Map.empty triangles

    addTriangleEdgesIfAffected !acc (a, b, c) =
      let anyAffected = isAffected a || isAffected b || isAffected c
       in if anyAffected
            then
              Map.insert (canonicalEdge a b) ()
                . Map.insert (canonicalEdge b c) ()
                . Map.insert (canonicalEdge a c) ()
                $ acc
            else acc

-- ----------------------------------------------------------------
-- Vertex interpolation
-- ----------------------------------------------------------------

-- | Interpolate two vertices, placing the result at the given
-- optimal position. UV and tangent are averaged; normal is a
-- placeholder (recomputed later).
interpolateVertices :: IntMap Vertex -> Word32 -> Word32 -> V3 -> Vertex
interpolateVertices vertMap idxA idxB optimalPos =
  case (IntMap.lookup (fromIntegral idxA) vertMap, IntMap.lookup (fromIntegral idxB) vertMap) of
    (Just vA, Just vB) ->
      Vertex
        { vPosition = optimalPos,
          vNormal = normalize (vNormal vA ^+^ vNormal vB),
          vUV = vertexInterpolationWeight *^ (vUV vA ^+^ vUV vB),
          vTangent = averageTangent (vTangent vA) (vTangent vB),
          vColor = defaultColor
        }
    (Just vA, Nothing) -> vA {vPosition = optimalPos}
    (Nothing, Just vB) -> vB {vPosition = optimalPos}
    (Nothing, Nothing) ->
      vertex optimalPos (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)

-- | Average two tangent vectors, preserving the handedness of the
-- first tangent.
averageTangent :: V4 -> V4 -> V4
averageTangent (V4 tx0 ty0 tz0 tw0) (V4 tx1 ty1 tz1 _tw1) =
  let V3 avgTx avgTy avgTz =
        normalize (V3 tx0 ty0 tz0 ^+^ V3 tx1 ty1 tz1)
   in V4 avgTx avgTy avgTz tw0

-- ----------------------------------------------------------------
-- Position lookups
-- ----------------------------------------------------------------

-- | Look up positions for three triangle vertices.
lookupTrianglePositions ::
  IntMap Vertex ->
  Word32 ->
  Word32 ->
  Word32 ->
  Maybe (V3, V3, V3)
lookupTrianglePositions vertMap idx0 idx1 idx2 = do
  v0 <- IntMap.lookup (fromIntegral idx0) vertMap
  v1 <- IntMap.lookup (fromIntegral idx1) vertMap
  v2 <- IntMap.lookup (fromIntegral idx2) vertMap
  pure (vPosition v0, vPosition v1, vPosition v2)

-- ----------------------------------------------------------------
-- Named constants
-- ----------------------------------------------------------------

-- | Number of indices per triangle.
indicesPerTriangle :: Int
indicesPerTriangle = 3

-- | Minimum allowed target triangle count.
minimumTriangleTarget :: Int
minimumTriangleTarget = 1

-- | Minimum simplification ratio (exclusive lower bound clamped).
minimumSimplifyRatio :: Float
minimumSimplifyRatio = 1.0e-6

-- | Maximum simplification ratio.
maximumSimplifyRatio :: Float
maximumSimplifyRatio = 1.0

-- | Multiplier 2.0 for off-diagonal symmetric quadric entries
-- in the error evaluation (@v^T Q v@ expansion).
quadricSymmetricWeight :: Float
quadricSymmetricWeight = 2.0

-- | Determinant threshold below which the 3x3 optimal-vertex
-- system is considered singular and the midpoint fallback is used.
singularDeterminantThreshold :: Float
singularDeterminantThreshold = 1.0e-10

-- | Normal length threshold below which a triangle is considered
-- degenerate (collinear vertices).
degenerateTriangleThreshold :: Float
degenerateTriangleThreshold = 1.0e-10

-- | Weight 0.5 for computing edge midpoints.
edgeMidpointWeight :: Float
edgeMidpointWeight = 0.5

-- | Weight 0.5 for vertex attribute interpolation (average of two).
vertexInterpolationWeight :: Float
vertexInterpolationWeight = 0.5
