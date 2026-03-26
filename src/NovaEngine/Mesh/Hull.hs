-- | Convex hull generation from point clouds.
--
-- Incremental convex hull algorithm for 3D points, hull extraction
-- from existing meshes, and convexity testing.
module NovaEngine.Mesh.Hull
  ( -- * Hull generation
    convexHull,
    convexHullFromMesh,

    -- * Convexity testing
    isConvex,
  )
where

import Data.List (foldl', partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import NovaEngine.Mesh.Types
  ( Mesh (..),
    V2 (..),
    V3 (..),
    V4 (..),
    VecSpace (..),
    Vertex (..),
    Word32,
    cross,
    dot,
    groupTriangles,
    mkMesh,
    normalize,
    safeIndex,
    vertex,
    vlength,
  )

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Epsilon for coplanarity and visibility tests.
coplanarEpsilon :: Float
coplanarEpsilon = 1.0e-6

-- | Epsilon for convexity check — maximum distance a vertex may lie
-- in front of a face plane and still be considered "on or behind".
convexityEpsilon :: Float
convexityEpsilon = 1.0e-5

-- | Default UV coordinate assigned to hull vertices.
defaultUV :: V2
defaultUV = V2 0 0

-- | Default tangent assigned to hull vertices.
defaultTangent :: V4
defaultTangent = V4 1 0 0 1

-- ----------------------------------------------------------------
-- Internal face representation
-- ----------------------------------------------------------------

-- | A triangular face on the convex hull, storing three vertex
-- indices (i0, i1, i2) and the precomputed outward-facing unit
-- normal. Vertex order determines CCW winding.
data HullFace = HullFace Int Int Int V3

-- ----------------------------------------------------------------
-- Convex hull generation
-- ----------------------------------------------------------------

-- | Generate a convex hull mesh from a list of 3D points using an
-- incremental convex hull algorithm.
--
-- Returns 'Nothing' if fewer than 4 non-coplanar points are provided.
-- The output mesh has outward-facing normals and CCW winding order.
convexHull :: [V3] -> Maybe Mesh
convexHull points
  | length indexedPoints < 4 = Nothing
  | otherwise = case findInitialTetrahedron indexedPoints of
      Nothing -> Nothing
      Just (tetFaces, remainingPoints) ->
        let finalFaces = foldl' (insertPoint pointArray) tetFaces remainingPoints
         in Just (facesToMesh pointArray finalFaces)
  where
    indexedPoints = zip [0 ..] points
    pointArray = Map.fromList indexedPoints

-- | Extract the convex hull from an existing mesh's vertex positions.
convexHullFromMesh :: Mesh -> Mesh
convexHullFromMesh mesh =
  fromMaybe mesh (convexHull positions)
  where
    positions = map vPosition (meshVertices mesh)

-- | Check if a mesh is convex. Every vertex must lie on or behind
-- every face plane (within 'convexityEpsilon' tolerance).
isConvex :: Mesh -> Bool
isConvex (Mesh vertices indices _) =
  all vertexBehindAllFaces positions
  where
    positions = map vPosition vertices
    triangles = groupTriangles indices
    facePlanes = concatMap buildFacePlane triangles

    buildFacePlane (i0, i1, i2) =
      case lookupPositions positions i0 i1 i2 of
        Nothing -> []
        Just (p0, p1, p2) ->
          let edge1 = p1 ^-^ p0
              edge2 = p2 ^-^ p0
              rawNormal = cross edge1 edge2
           in [ (normalize rawNormal, p0)
              | vlength rawNormal >= coplanarEpsilon
              ]

    vertexBehindAllFaces position =
      all (vertexBehindPlane position) facePlanes

    vertexBehindPlane position (normal, planePoint) =
      dot normal (position ^-^ planePoint) <= convexityEpsilon

-- ----------------------------------------------------------------
-- Initial tetrahedron
-- ----------------------------------------------------------------

-- | Find 4 non-coplanar points to form the initial tetrahedron.
--
-- Strategy:
--   1. Pick the two points most distant along the X axis.
--   2. Pick the point most distant from the line through those two.
--   3. Pick the point most distant from the plane of those three.
--   4. Orient all four faces outward.
findInitialTetrahedron ::
  [(Int, V3)] ->
  Maybe ([HullFace], [Int])
findInitialTetrahedron [] = Nothing
findInitialTetrahedron indexedPoints
  | length indexedPoints < 4 = Nothing
  | vlength baseEdge < coplanarEpsilon = Nothing
  | maxLineDist < coplanarEpsilon = Nothing
  | abs maxPlaneDist < coplanarEpsilon = Nothing
  | otherwise =
      let -- Orient the tetrahedron so all normals point outward.
          -- The centroid of the four points is strictly inside.
          centroid = (0.25 :: Float) *^ (p0 ^+^ p1 ^+^ p2 ^+^ p3)
          faces = orientTetrahedron centroid idx0 idx1 idx2 idx3 p0 p1 p2 p3
          remaining =
            [ idx
            | (idx, _) <- indexedPoints,
              idx /= idx0,
              idx /= idx1,
              idx /= idx2,
              idx /= idx3
            ]
       in Just (faces, remaining)
  where
    -- Step 1: find the two points most distant along X.
    foldMax1 cmp (first : rest) = foldl' (\best x -> if cmp x best == GT then x else best) first rest
    foldMax1 _ [] = (0, vzero) -- unreachable: guarded by length >= 4
    foldMin1 cmp (first : rest) = foldl' (\best x -> if cmp x best == LT then x else best) first rest
    foldMin1 _ [] = (0, vzero) -- unreachable: guarded by length >= 4
    compareByX (_, V3 xa _ _) (_, V3 xb _ _) = compare xa xb
    (idx0, p0) = foldMin1 compareByX indexedPoints
    (idx1, p1) = foldMax1 compareByX indexedPoints
    baseEdge = p1 ^-^ p0

    -- Step 2: find the point most distant from the line p0-p1.
    baseDir = normalize baseEdge
    distFromLine (_, pt) =
      let diff = pt ^-^ p0
          projected = dot diff baseDir *^ baseDir
          rejection = diff ^-^ projected
       in vlength rejection
    candidates2 = filter (\(idx, _) -> idx /= idx0 && idx /= idx1) indexedPoints
    (idx2, p2) = foldMax1 (\a b -> compare (distFromLine a) (distFromLine b)) candidates2
    maxLineDist = distFromLine (idx2, p2)

    -- Step 3: find the point most distant from the plane of p0, p1, p2.
    planeNormal = normalize (cross (p1 ^-^ p0) (p2 ^-^ p0))
    distFromPlane (_, pt) = abs (dot planeNormal (pt ^-^ p0))
    candidates3 = filter (\(idx, _) -> idx /= idx0 && idx /= idx1 && idx /= idx2) indexedPoints
    (idx3, p3) = foldMax1 (\a b -> compare (distFromPlane a) (distFromPlane b)) candidates3
    maxPlaneDist = dot planeNormal (p3 ^-^ p0)

-- | Build four faces for the initial tetrahedron with outward
-- normals. Each face's winding is chosen so the normal points away
-- from the centroid.
orientTetrahedron :: V3 -> Int -> Int -> Int -> Int -> V3 -> V3 -> V3 -> V3 -> [HullFace]
orientTetrahedron centroid i0 i1 i2 i3 p0 p1 p2 p3 =
  map orientFace rawFaces
  where
    rawFaces =
      [ (i0, i1, i2, p0, p1, p2),
        (i0, i1, i3, p0, p1, p3),
        (i0, i2, i3, p0, p2, p3),
        (i1, i2, i3, p1, p2, p3)
      ]

    orientFace (a, b, c, pa, pb, pc) =
      let edge1 = pb ^-^ pa
          edge2 = pc ^-^ pa
          rawNormal = normalize (cross edge1 edge2)
          faceMidpoint = (1.0 / 3.0 :: Float) *^ (pa ^+^ pb ^+^ pc)
          toCentroid = centroid ^-^ faceMidpoint
       in if dot rawNormal toCentroid > 0
            then -- Normal points inward — flip winding
              HullFace a c b (negateV rawNormal)
            else -- Normal points outward — keep winding
              HullFace a b c rawNormal

-- ----------------------------------------------------------------
-- Incremental insertion
-- ----------------------------------------------------------------

-- | Insert a single point into the convex hull. If the point is
-- inside the current hull, the face list is unchanged. Otherwise,
-- visible faces are removed and new faces are created connecting
-- the point to the horizon edges.
insertPoint :: Map Int V3 -> [HullFace] -> Int -> [HullFace]
insertPoint pointArray faces pointIdx =
  case Map.lookup pointIdx pointArray of
    Nothing -> faces
    Just point ->
      let (visibleFaces, hiddenFaces) = partition (faceVisibleFrom pointArray point) faces
       in if null visibleFaces
            then faces
            else
              let horizonEdges = findHorizonEdges visibleFaces
                  newFaces = map (makeNewFace pointArray pointIdx point) horizonEdges
               in hiddenFaces ++ newFaces

-- | Test whether a point can see a face (lies strictly in front of
-- the face plane).
faceVisibleFrom :: Map Int V3 -> V3 -> HullFace -> Bool
faceVisibleFrom pointArray point (HullFace fi0 _ _ normal) =
  case Map.lookup fi0 pointArray of
    Nothing -> False
    Just faceVertex ->
      dot normal (point ^-^ faceVertex) > coplanarEpsilon

-- | Find the horizon edges — edges shared between exactly one
-- visible face and one non-visible face. Each horizon edge is
-- returned oriented so that new faces will have CCW winding.
--
-- An edge (a, b) on a visible face has the visible-face winding.
-- The horizon edge should be reversed so that the new triangle
-- (point, b, a) has the correct outward-facing winding.
findHorizonEdges :: [HullFace] -> [(Int, Int)]
findHorizonEdges visibleFaces =
  [ directedEdge
  | (canonical, count) <- Map.toList countMap,
    count == (1 :: Int),
    Just directedEdge <- [Map.lookup canonical directedMap]
  ]
  where
    -- For each visible face, extract edges in reversed winding order.
    -- When a face has winding (a, b, c), its half-edges are (a,b),
    -- (b,c), (c,a). Reversing gives (b,a), (c,b), (a,c) — the
    -- orientation needed for the new triangle (point, a, b).
    allEdges = concatMap faceReversedEdges visibleFaces
    (countMap, directedMap) = foldl' recordEdge (Map.empty, Map.empty) allEdges

    faceReversedEdges (HullFace a b c _) =
      [ (b, a),
        (c, b),
        (a, c)
      ]

    -- Count occurrences by canonical (undirected) key. An edge
    -- shared by two visible faces appears twice; a horizon edge
    -- appears exactly once.
    recordEdge (!counts, !directed) (ea, eb) =
      let canonical = (min ea eb, max ea eb)
       in ( Map.insertWith (+) canonical 1 counts,
            Map.insert canonical (ea, eb) directed
          )

-- | Create a new triangular face connecting a point to a horizon edge.
-- The edge (a, b) is already oriented for correct CCW winding:
-- the new triangle is (point, a, b).
makeNewFace :: Map Int V3 -> Int -> V3 -> (Int, Int) -> HullFace
makeNewFace pointArray pointIdx point (edgeA, edgeB) =
  case (Map.lookup edgeA pointArray, Map.lookup edgeB pointArray) of
    (Just pa, Just pb) ->
      let edge1 = pa ^-^ point
          edge2 = pb ^-^ point
          rawNormal = normalize (cross edge1 edge2)
       in HullFace pointIdx edgeA edgeB rawNormal
    _ ->
      -- Defensive fallback — should never happen with valid input.
      HullFace pointIdx edgeA edgeB (V3 0 1 0)

-- ----------------------------------------------------------------
-- Conversion to mesh
-- ----------------------------------------------------------------

-- | Convert the internal face list to a 'Mesh' with proper vertices,
-- indices, and normals.
facesToMesh :: Map Int V3 -> [HullFace] -> Mesh
facesToMesh pointArray faces =
  mkMesh vertices indices
  where
    -- Accumulate face normals at each vertex for smooth shading.
    normalAccum = foldl' accumulateNormals Map.empty faces

    accumulateNormals !nMap (HullFace a b c normal) =
      Map.insertWith (^+^) a normal
        . Map.insertWith (^+^) b normal
        . Map.insertWith (^+^) c normal
        $ nMap

    -- Assign sequential mesh indices to each unique point index.
    -- The keys of normalAccum are exactly the set of point indices
    -- used by at least one face.
    pointIndices = Map.keys normalAccum
    meshIndexMap = Map.fromList (zip pointIndices [0 :: Int ..])

    vertices =
      [ let position = fromMaybe vzero (Map.lookup pointIdx pointArray)
            normal = maybe (V3 0 1 0) normalize (Map.lookup pointIdx normalAccum)
         in vertex position normal defaultUV defaultTangent
      | pointIdx <- pointIndices
      ]

    indices = concatMap faceToIndices faces

    faceToIndices (HullFace a b c _) =
      case (Map.lookup a meshIndexMap, Map.lookup b meshIndexMap, Map.lookup c meshIndexMap) of
        (Just ia, Just ib, Just ic) ->
          [fromIntegral ia, fromIntegral ib, fromIntegral ic]
        _ -> []

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Look up three vertex positions from a position list by index.
lookupPositions :: [V3] -> Word32 -> Word32 -> Word32 -> Maybe (V3, V3, V3)
lookupPositions positions i0 i1 i2 =
  case (safeIndex positions (fromIntegral i0), safeIndex positions (fromIntegral i1), safeIndex positions (fromIntegral i2)) of
    (Just p0, Just p1, Just p2) -> Just (p0, p1, p2)
    _ -> Nothing
