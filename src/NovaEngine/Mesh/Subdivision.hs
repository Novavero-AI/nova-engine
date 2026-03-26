-- | Subdivision surfaces.
--
-- Catmull-Clark (quad output, any topology) and Loop (triangle
-- meshes).
module NovaEngine.Mesh.Subdivision
  ( -- * Subdivision
    subdivide,
    subdivideLoop,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import NovaEngine.Mesh.Combine (recomputeNormals, recomputeTangents)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Catmull-Clark subdivision, @n@ levels.
--
-- Input is a triangle mesh. The first level converts triangles to
-- quads; subsequent levels are quad-to-quad. Only the final output
-- is triangulated back. Normals and tangents are recomputed from
-- geometry after the last step.
subdivide :: Int -> Mesh -> Mesh
subdivide levels mesh
  | levels <= 0 = mesh
  | otherwise =
      recomputeTangents
        . recomputeNormals
        . facesToMesh
        . catmullClarkLevels levels
        . meshToFaces
        $ mesh

-- | Loop subdivision, @n@ levels.
--
-- Input must be a triangle mesh. Each triangle becomes 4 triangles
-- per level. Normals and tangents are recomputed after the last
-- step.
subdivideLoop :: Int -> Mesh -> Mesh
subdivideLoop levels mesh
  | levels <= 0 = mesh
  | otherwise =
      recomputeTangents
        . recomputeNormals
        . facesToMesh
        . loopLevels levels
        . meshToFaces
        $ mesh

-- ----------------------------------------------------------------
-- Internal vertex type (position + UV only; normals/tangents
-- recomputed at the end)
-- ----------------------------------------------------------------

-- | Lightweight vertex carrying only the attributes that are
-- interpolated during subdivision.  Normals and tangents are
-- recomputed from geometry at the very end.
data SubVertex = SubVertex
  { svPosition :: !V3,
    svUV :: !V2
  }

-- | Average a non-empty list of 'SubVertex' values.
averageSubVertices :: [SubVertex] -> SubVertex
averageSubVertices [] = SubVertex vzero vzero
averageSubVertices verts =
  let count = length verts
      invCount = 1.0 / fromIntegral count
      sumPos = foldl' (^+^) vzero (map svPosition verts)
      sumUV = foldl' (^+^) vzero (map svUV verts)
   in SubVertex (invCount *^ sumPos) (invCount *^ sumUV)

-- | Linearly interpolate two sub-vertices with equal weight.
midpointSubVertex :: SubVertex -> SubVertex -> SubVertex
midpointSubVertex (SubVertex posA uvA) (SubVertex posB uvB) =
  SubVertex
    (midpointWeight *^ (posA ^+^ posB))
    (midpointWeight *^ (uvA ^+^ uvB))

-- | Blend a sub-vertex by a scalar weight.
scaleSubVertex :: Float -> SubVertex -> SubVertex
scaleSubVertex w (SubVertex pos uv) = SubVertex (w *^ pos) (w *^ uv)

-- | Add two sub-vertices component-wise.
addSubVertex :: SubVertex -> SubVertex -> SubVertex
addSubVertex (SubVertex posA uvA) (SubVertex posB uvB) =
  SubVertex (posA ^+^ posB) (uvA ^+^ uvB)

-- | Zero sub-vertex.
zeroSubVertex :: SubVertex
zeroSubVertex = SubVertex vzero vzero

-- ----------------------------------------------------------------
-- Face-based internal representation
-- ----------------------------------------------------------------

-- | A face is a list of vertex indices (3 for triangles, 4 for
-- quads).
type Face = [Word32]

-- | Internal mesh: vertex array + face list.
data FaceMesh = FaceMesh !(IntMap SubVertex) ![Face]

-- ----------------------------------------------------------------
-- Mesh <-> FaceMesh conversion
-- ----------------------------------------------------------------

-- | Convert a triangle 'Mesh' to the internal face representation.
meshToFaces :: Mesh -> FaceMesh
meshToFaces (Mesh vertices indices _count) =
  FaceMesh vertMap faces
  where
    vertMap =
      IntMap.fromList
        [ (idx, SubVertex (vPosition v) (vUV v))
        | (idx, v) <- zip [0 ..] vertices
        ]
    faces = groupFaces indices

    groupFaces (a : b : c : rest) = [a, b, c] : groupFaces rest
    groupFaces _ = []

-- | Convert the internal face representation back to a triangle
-- 'Mesh'.  Quads are split along the first diagonal.
facesToMesh :: FaceMesh -> Mesh
facesToMesh (FaceMesh vertMap faces) =
  mkMesh outVertices outIndices
  where
    -- Collect all vertex indices used by faces, in order
    maxIdx = case IntMap.lookupMax vertMap of
      Just (mx, _) -> mx
      Nothing -> -1

    outVertices =
      [ toVertex (fromMaybe zeroSubVertex (IntMap.lookup idx vertMap))
      | idx <- [0 .. maxIdx]
      ]

    toVertex (SubVertex pos uv) =
      Vertex
        { vPosition = pos,
          vNormal = V3 0 1 0, -- placeholder, recomputed later
          vUV = uv,
          vTangent = V4 1 0 0 1, -- placeholder, recomputed later
          vColor = defaultColor
        }

    outIndices = concatMap triangulateFace faces

    triangulateFace [a, b, c] = [a, b, c]
    triangulateFace [a, b, c, d] = [a, b, c, a, c, d]
    triangulateFace _ = []

-- ----------------------------------------------------------------
-- Edge key helpers
-- ----------------------------------------------------------------

-- | Canonical edge key with smaller index first.
edgeKey :: Word32 -> Word32 -> (Word32, Word32)
edgeKey a b
  | a <= b = (a, b)
  | otherwise = (b, a)

-- ----------------------------------------------------------------
-- Adjacency structures
-- ----------------------------------------------------------------

-- | Info about an edge's adjacent faces and opposite vertices.
-- Fields: face indices, opposite vertex indices.
data EdgeInfo = EdgeInfo ![Int] ![Word32]

-- | Build edge-to-face adjacency.  Each face is identified by its
-- index in the face list.  For each edge we record which faces
-- share it and which vertices of those faces are opposite.
buildEdgeMap :: [Face] -> Map (Word32, Word32) EdgeInfo
buildEdgeMap faces =
  foldl' addFace Map.empty (zip [0 ..] faces)
  where
    addFace acc (faceIdx, face) =
      let edges = faceEdgesWithOpposite face
       in foldl' (addEdge faceIdx) acc edges

    addEdge faceIdx acc (ek, opp) =
      Map.insertWith mergeEdgeInfo ek (EdgeInfo [faceIdx] [opp]) acc

    mergeEdgeInfo (EdgeInfo fa oa) (EdgeInfo fb ob) =
      EdgeInfo (fa ++ fb) (oa ++ ob)

-- | Extract edges from a face, each paired with the opposite
-- vertex (or vertices).  For a triangle [a,b,c], edge (a,b) has
-- opposite vertex c, etc.  For a quad [a,b,c,d], edge (a,b) has
-- opposite vertices c and d, but we only record the face-local
-- opposite (the vertex not on this edge).
faceEdgesWithOpposite :: Face -> [((Word32, Word32), Word32)]
faceEdgesWithOpposite [] = []
faceEdgesWithOpposite [_] = []
faceEdgesWithOpposite [_, _] = []
faceEdgesWithOpposite face =
  [ (edgeKey a b, opp)
  | (idx, (a, b)) <- zip [0 ..] (faceEdgePairs face),
    Just opp <- [oppositeVertex face idx]
  ]

-- | All directed edge pairs from a face (cyclic).
faceEdgePairs :: Face -> [(Word32, Word32)]
faceEdgePairs [] = []
faceEdgePairs face =
  zip face (drop 1 face ++ take 1 face)

-- | The vertex opposite to the edge at position @idx@ in the
-- face.  For triangles this is unambiguous.  For quads, pick the
-- vertex diagonally opposite to the first vertex of the edge.
-- Returns 'Nothing' for degenerate (empty) faces.
oppositeVertex :: Face -> Int -> Maybe Word32
oppositeVertex [] _ = Nothing
oppositeVertex face idx =
  let len = length face
      oppositeIdx = (idx + 2) `mod` len
   in case drop oppositeIdx face of
        (v : _) -> Just v
        [] -> Nothing

-- | Build vertex-to-neighbor adjacency from face list.
-- Uses 'IntSet' internally for O(n log n) deduplication.
buildVertexNeighbors :: [Face] -> IntMap [Word32]
buildVertexNeighbors faces =
  IntMap.map (map fromIntegral . IntSet.toList) setMap
  where
    setMap :: IntMap IntSet.IntSet
    setMap = foldl' addFace IntMap.empty faces

    addFace acc face =
      foldl' addEdge acc (faceEdgePairs face)

    addEdge acc (a, b) =
      let accWithA =
            IntMap.insertWith
              IntSet.union
              (fromIntegral a)
              (IntSet.singleton (fromIntegral b))
              acc
       in IntMap.insertWith
            IntSet.union
            (fromIntegral b)
            (IntSet.singleton (fromIntegral a))
            accWithA

-- | Whether an edge is on the boundary (only one adjacent face).
isBoundaryEdge :: Map (Word32, Word32) EdgeInfo -> Word32 -> Word32 -> Bool
isBoundaryEdge edgeMap a b =
  case Map.lookup (edgeKey a b) edgeMap of
    Just (EdgeInfo fs _) -> length fs < 2
    Nothing -> True

-- | Whether a vertex is on the boundary.
isBoundaryVertex :: Map (Word32, Word32) EdgeInfo -> IntMap [Word32] -> Word32 -> Bool
isBoundaryVertex edgeMap neighbors vid =
  case IntMap.lookup (fromIntegral vid) neighbors of
    Nothing -> True
    Just neighs -> any (isBoundaryEdge edgeMap vid) neighs

-- | Get the boundary neighbors of a vertex (neighbors connected
-- by boundary edges), in the order they appear in the neighbor
-- list.
boundaryNeighbors :: Map (Word32, Word32) EdgeInfo -> IntMap [Word32] -> Word32 -> [Word32]
boundaryNeighbors edgeMap neighbors vid =
  case IntMap.lookup (fromIntegral vid) neighbors of
    Nothing -> []
    Just neighs -> filter (isBoundaryEdge edgeMap vid) neighs

-- ----------------------------------------------------------------
-- Catmull-Clark subdivision
-- ----------------------------------------------------------------

-- | Run @n@ levels of Catmull-Clark subdivision.
catmullClarkLevels :: Int -> FaceMesh -> FaceMesh
catmullClarkLevels n fm
  | n <= 0 = fm
  | otherwise = catmullClarkLevels (n - 1) (catmullClarkStep fm)

-- | One step of Catmull-Clark subdivision.
catmullClarkStep :: FaceMesh -> FaceMesh
catmullClarkStep (FaceMesh vertMap faces) =
  FaceMesh newVertMap newFaces
  where
    edgeMap = buildEdgeMap faces
    neighbors = buildVertexNeighbors faces
    vertFaceMap = buildVertexFaceMap faces

    -- ---- 1. Face points ----
    -- One new vertex per face, at the centroid.
    facePointStart :: Word32
    facePointStart = fromIntegral (maybe 0 ((+ 1) . fst) (IntMap.lookupMax vertMap))

    facePoints :: IntMap SubVertex
    facePoints =
      IntMap.fromList
        [ (faceIdx, computeFaceCentroid vertMap face)
        | (faceIdx, face) <- zip [0 ..] faces
        ]

    facePointIndex :: Int -> Word32
    facePointIndex faceIdx = facePointStart + fromIntegral faceIdx

    -- ---- 2. Edge points ----
    edgePointStart :: Word32
    edgePointStart = facePointStart + fromIntegral (length faces)

    -- Assign each unique edge a sequential index.
    uniqueEdges :: [(Word32, Word32)]
    uniqueEdges = Map.keys edgeMap

    edgeIndexMap :: Map (Word32, Word32) Word32
    edgeIndexMap =
      Map.fromList
        (zip uniqueEdges [edgePointStart ..])

    edgePoints :: Map (Word32, Word32) SubVertex
    edgePoints =
      Map.mapWithKey (computeEdgePoint vertMap facePoints edgeMap) edgeMap

    lookupEdgePointIndex :: Word32 -> Word32 -> Word32
    lookupEdgePointIndex a b =
      fromMaybe 0 (Map.lookup (edgeKey a b) edgeIndexMap)

    -- ---- 3. Vertex points ----
    vertexPoints :: IntMap SubVertex
    vertexPoints =
      IntMap.mapWithKey
        (computeVertexPoint vertMap facePoints edgeMap neighbors vertFaceMap)
        vertMap

    -- ---- 4. Build the new vertex map ----
    edgeVertMap :: IntMap SubVertex
    edgeVertMap =
      IntMap.fromList
        [ (fromIntegral idx, sv)
        | (ek, sv) <- Map.toList edgePoints,
          idx <- case Map.lookup ek edgeIndexMap of
            Just i -> [i]
            Nothing -> []
        ]

    faceVertMap :: IntMap SubVertex
    faceVertMap =
      IntMap.fromList
        [ (fromIntegral (facePointIndex faceIdx), sv)
        | (faceIdx, sv) <- IntMap.toList facePoints
        ]

    newVertMap :: IntMap SubVertex
    newVertMap =
      IntMap.unions [vertexPoints, edgeVertMap, faceVertMap]

    -- ---- 5. Build new faces ----
    -- For each face, create one quad per vertex of the original
    -- face.  The quad connects:
    --   vertex point -> edge point on prev edge -> face point ->
    --   edge point on next edge
    newFaces :: [Face]
    newFaces = concatMap buildQuadsForFace (zip [0 ..] faces)

    buildQuadsForFace :: (Int, Face) -> [Face]
    buildQuadsForFace (faceIdx, face) =
      let fp = facePointIndex faceIdx
          len = length face
          indexed = zip [0 ..] face
       in [ let prevIdx = (i - 1) `mod` len
                prevVert = case drop prevIdx face of
                  (pv : _) -> pv
                  [] -> vid
                nextIdx = (i + 1) `mod` len
                nextVert = case drop nextIdx face of
                  (nv : _) -> nv
                  [] -> vid
                epPrev = lookupEdgePointIndex vid prevVert
                epNext = lookupEdgePointIndex vid nextVert
             in [vid, epNext, fp, epPrev]
          | (i, vid) <- indexed
          ]

-- | Compute face centroid.
computeFaceCentroid :: IntMap SubVertex -> Face -> SubVertex
computeFaceCentroid vertMap face =
  averageSubVertices (lookupVertices vertMap face)

-- | Compute edge point for Catmull-Clark.
--
-- Interior edges: average of edge midpoint and adjacent face
-- centroids.
-- Boundary edges: simple midpoint.
computeEdgePoint ::
  IntMap SubVertex ->
  IntMap SubVertex ->
  Map (Word32, Word32) EdgeInfo ->
  (Word32, Word32) ->
  EdgeInfo ->
  SubVertex
computeEdgePoint vertMap facePoints _edgeMap (a, b) (EdgeInfo faceIdxs _) =
  let va = lookupSubVertex vertMap a
      vb = lookupSubVertex vertMap b
   in if length faceIdxs < 2
        then -- Boundary edge: midpoint
          midpointSubVertex va vb
        else -- Interior edge: avg of edge midpoint + face centroids
          let edgeMid = midpointSubVertex va vb
              facePts = lookupFacePoints facePoints faceIdxs
              faceMid = averageSubVertices facePts
           in midpointSubVertex edgeMid faceMid

-- | Compute vertex point for Catmull-Clark.
--
-- Interior: @(Q + 2R + (n-3)V) / n@
-- Boundary: @1/8 v_prev + 3/4 V + 1/8 v_next@
computeVertexPoint ::
  IntMap SubVertex ->
  IntMap SubVertex ->
  Map (Word32, Word32) EdgeInfo ->
  IntMap [Word32] ->
  IntMap [Int] ->
  Int ->
  SubVertex ->
  SubVertex
computeVertexPoint vertMap facePoints edgeMap neighbors vertFaceMap vidInt origV =
  let vid = fromIntegral vidInt :: Word32
   in if isBoundaryVertex edgeMap neighbors vid
        then computeBoundaryVertex vertMap edgeMap neighbors vid origV
        else computeInteriorVertex vertMap facePoints neighbors vertFaceMap vid origV

-- | Interior vertex: @(Q + 2R + (n-3)V) / n@
computeInteriorVertex ::
  IntMap SubVertex ->
  IntMap SubVertex ->
  IntMap [Word32] ->
  IntMap [Int] ->
  Word32 ->
  SubVertex ->
  SubVertex
computeInteriorVertex vertMap facePoints neighbors vertFaceMap vid origV =
  let neighs = fromMaybe [] (IntMap.lookup (fromIntegral vid) neighbors)
      valence = length neighs
      invValence = 1.0 / fromIntegral valence

      -- Q: average of face points for faces touching this vertex
      adjacentFaceIdxs = vertexAdjacentFacesFrom vertFaceMap vid
      adjFacePts = lookupFacePoints facePoints adjacentFaceIdxs
      avgFacePoint = averageSubVertices adjFacePts

      -- R: average of edge midpoints
      edgeMidpoints =
        [ midpointSubVertex origV (lookupSubVertex vertMap nb)
        | nb <- neighs
        ]
      avgEdgeMid = averageSubVertices edgeMidpoints

      -- (Q + 2R + (n-3)V) / n
      valenceMinusThree = fromIntegral valence - vertexPointMagicThree
   in scaleSubVertex
        invValence
        ( addSubVertex
            avgFacePoint
            ( addSubVertex
                (scaleSubVertex vertexPointEdgeWeight avgEdgeMid)
                (scaleSubVertex valenceMinusThree origV)
            )
        )

-- | Boundary vertex: @1/8 v_prev + 3/4 V + 1/8 v_next@
computeBoundaryVertex ::
  IntMap SubVertex ->
  Map (Word32, Word32) EdgeInfo ->
  IntMap [Word32] ->
  Word32 ->
  SubVertex ->
  SubVertex
computeBoundaryVertex vertMap edgeMap neighbors vid origV =
  let bNeighs = boundaryNeighbors edgeMap neighbors vid
   in case bNeighs of
        (na : nb : _) ->
          let va = lookupSubVertex vertMap na
              vb = lookupSubVertex vertMap nb
           in addSubVertex
                (scaleSubVertex boundaryCornerWeight va)
                ( addSubVertex
                    (scaleSubVertex boundaryCenterWeight origV)
                    (scaleSubVertex boundaryCornerWeight vb)
                )
        [na] ->
          let va = lookupSubVertex vertMap na
           in addSubVertex
                (scaleSubVertex midpointWeight va)
                (scaleSubVertex midpointWeight origV)
        [] -> origV

-- | Build a map from each vertex to the list of face indices
-- containing it.  O(V+F) total instead of O(V*F) per lookup.
buildVertexFaceMap :: [Face] -> IntMap [Int]
buildVertexFaceMap faces =
  foldl' addFace IntMap.empty (zip [0 ..] faces)
  where
    addFace acc (faceIdx, face) =
      foldl' (\m vid -> IntMap.insertWith (++) (fromIntegral vid) [faceIdx] m) acc face

-- | Find which face indices contain a given vertex, using a
-- precomputed map.
vertexAdjacentFacesFrom :: IntMap [Int] -> Word32 -> [Int]
vertexAdjacentFacesFrom vfMap vid =
  fromMaybe [] (IntMap.lookup (fromIntegral vid) vfMap)

-- ----------------------------------------------------------------
-- Loop subdivision
-- ----------------------------------------------------------------

-- | Run @n@ levels of Loop subdivision.
loopLevels :: Int -> FaceMesh -> FaceMesh
loopLevels n fm
  | n <= 0 = fm
  | otherwise = loopLevels (n - 1) (loopStep fm)

-- | One step of Loop subdivision.
loopStep :: FaceMesh -> FaceMesh
loopStep (FaceMesh vertMap faces) =
  FaceMesh newVertMap newFaces
  where
    edgeMap = buildEdgeMap faces
    neighbors = buildVertexNeighbors faces

    -- ---- 1. Edge points ----
    edgePointStart :: Word32
    edgePointStart = fromIntegral (maybe 0 ((+ 1) . fst) (IntMap.lookupMax vertMap))

    uniqueEdges :: [(Word32, Word32)]
    uniqueEdges = Map.keys edgeMap

    edgeIndexMap :: Map (Word32, Word32) Word32
    edgeIndexMap =
      Map.fromList (zip uniqueEdges [edgePointStart ..])

    edgePoints :: Map (Word32, Word32) SubVertex
    edgePoints =
      Map.mapWithKey (computeLoopEdgePoint vertMap edgeMap) edgeMap

    lookupEdgePointIndex :: Word32 -> Word32 -> Word32
    lookupEdgePointIndex a b =
      fromMaybe 0 (Map.lookup (edgeKey a b) edgeIndexMap)

    -- ---- 2. Vertex points ----
    vertexPoints :: IntMap SubVertex
    vertexPoints =
      IntMap.mapWithKey
        (computeLoopVertexPoint vertMap edgeMap neighbors)
        vertMap

    -- ---- 3. Build new vertex map ----
    edgeVertMap :: IntMap SubVertex
    edgeVertMap =
      IntMap.fromList
        [ (fromIntegral idx, sv)
        | (ek, sv) <- Map.toList edgePoints,
          idx <- case Map.lookup ek edgeIndexMap of
            Just i -> [i]
            Nothing -> []
        ]

    newVertMap :: IntMap SubVertex
    newVertMap = IntMap.union vertexPoints edgeVertMap

    -- ---- 4. Build new faces ----
    -- Each triangle [a,b,c] produces 4 triangles:
    --   [a,  ep_ab, ep_ac]
    --   [b,  ep_bc, ep_ab]
    --   [c,  ep_ac, ep_bc]
    --   [ep_ab, ep_bc, ep_ac]
    newFaces :: [Face]
    newFaces = concatMap splitTriangle faces

    splitTriangle :: Face -> [Face]
    splitTriangle [a, b, c] =
      let epAB = lookupEdgePointIndex a b
          epBC = lookupEdgePointIndex b c
          epAC = lookupEdgePointIndex a c
       in [ [a, epAB, epAC],
            [b, epBC, epAB],
            [c, epAC, epBC],
            [epAB, epBC, epAC]
          ]
    splitTriangle _ = []

-- | Compute Loop edge point.
--
-- Interior: @3/8 (v_a + v_b) + 1/8 (v_c + v_d)@
-- Boundary: midpoint.
computeLoopEdgePoint ::
  IntMap SubVertex ->
  Map (Word32, Word32) EdgeInfo ->
  (Word32, Word32) ->
  EdgeInfo ->
  SubVertex
computeLoopEdgePoint vertMap _edgeMap (a, b) (EdgeInfo faceIdxs opposite) =
  let va = lookupSubVertex vertMap a
      vb = lookupSubVertex vertMap b
   in if length faceIdxs < 2
        then midpointSubVertex va vb
        else
          let oppositeVerts = map (lookupSubVertex vertMap) (take 2 opposite)
           in case oppositeVerts of
                [vc, vd] ->
                  addSubVertex
                    (scaleSubVertex loopEdgePrimaryWeight (addSubVertex va vb))
                    (scaleSubVertex loopEdgeSecondaryWeight (addSubVertex vc vd))
                _ -> midpointSubVertex va vb

-- | Compute Loop vertex point.
--
-- Interior: @(1 - n*beta) * V + beta * sum(neighbors)@
-- Boundary: @1/8 v_prev + 3/4 V + 1/8 v_next@
computeLoopVertexPoint ::
  IntMap SubVertex ->
  Map (Word32, Word32) EdgeInfo ->
  IntMap [Word32] ->
  Int ->
  SubVertex ->
  SubVertex
computeLoopVertexPoint vertMap edgeMap neighbors vidInt origV =
  let vid = fromIntegral vidInt :: Word32
   in if isBoundaryVertex edgeMap neighbors vid
        then computeBoundaryVertex vertMap edgeMap neighbors vid origV
        else
          let neighs = fromMaybe [] (IntMap.lookup vidInt neighbors)
              valence = length neighs
              beta = loopBeta valence
              selfWeight = 1.0 - fromIntegral valence * beta
              neighborSum =
                foldl' addSubVertex zeroSubVertex $
                  map (lookupSubVertex vertMap) neighs
           in addSubVertex
                (scaleSubVertex selfWeight origV)
                (scaleSubVertex beta neighborSum)

-- | Loop's beta weight for a vertex of given valence.
--
-- @beta = 1/n * (5/8 - (3/8 + 1/4 * cos(2*pi/n))^2)@
loopBeta :: Int -> Float
loopBeta valence =
  let n = fromIntegral valence :: Float
      cosVal = cos (loopTwoPi / n)
      inner = loopBetaBaseWeight + loopBetaCosWeight * cosVal
   in (1.0 / n) * (loopBetaOuterWeight - inner * inner)

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Look up a sub-vertex from the map, falling back to zero.
lookupSubVertex :: IntMap SubVertex -> Word32 -> SubVertex
lookupSubVertex vertMap idx =
  fromMaybe zeroSubVertex (IntMap.lookup (fromIntegral idx) vertMap)

-- | Look up vertices from the map for a list of indices.
lookupVertices :: IntMap SubVertex -> [Word32] -> [SubVertex]
lookupVertices vertMap = mapMaybe (\idx -> IntMap.lookup (fromIntegral idx) vertMap)

-- | Look up face points for a list of face indices.
lookupFacePoints :: IntMap SubVertex -> [Int] -> [SubVertex]
lookupFacePoints facePoints = mapMaybe (`IntMap.lookup` facePoints)

-- ----------------------------------------------------------------
-- Named constants
-- ----------------------------------------------------------------

-- | Weight 0.5 for midpoint calculations.
midpointWeight :: Float
midpointWeight = 0.5

-- | Boundary corner weight (1/8).
boundaryCornerWeight :: Float
boundaryCornerWeight = 0.125

-- | Boundary center weight (3/4).
boundaryCenterWeight :: Float
boundaryCenterWeight = 0.75

-- | Weight 2.0 for edge midpoints in vertex point formula.
vertexPointEdgeWeight :: Float
vertexPointEdgeWeight = 2.0

-- | Constant 3.0 in @(n-3)@ of the vertex point formula.
vertexPointMagicThree :: Float
vertexPointMagicThree = 3.0

-- | Loop edge primary weight (3/8).
loopEdgePrimaryWeight :: Float
loopEdgePrimaryWeight = 0.375

-- | Loop edge secondary weight (1/8).
loopEdgeSecondaryWeight :: Float
loopEdgeSecondaryWeight = 0.125

-- | 2 * pi for Loop beta calculation.
loopTwoPi :: Float
loopTwoPi = 2.0 * pi

-- | Base weight in Loop beta: 3/8.
loopBetaBaseWeight :: Float
loopBetaBaseWeight = 0.375

-- | Cosine weight in Loop beta: 1/4.
loopBetaCosWeight :: Float
loopBetaCosWeight = 0.25

-- | Outer weight in Loop beta: 5/8.
loopBetaOuterWeight :: Float
loopBetaOuterWeight = 0.625
