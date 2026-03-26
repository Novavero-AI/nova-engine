-- | Geodesic sphere (icosphere) generation.
--
-- Starts from a regular icosahedron (12 vertices, 20 triangles) and
-- subdivides each triangle into 4 by splitting edges at midpoints
-- projected onto the sphere. Analytical normals, spherical UVs with
-- seam duplication, and analytical tangents.
module NovaEngine.Mesh.Icosphere
  ( -- * Icosphere generation
    icosphere,
  )
where

import Data.Array (Array, listArray, (!))
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Golden ratio, used to construct icosahedron vertices.
goldenRatio :: Float
goldenRatio = (1.0 + sqrt 5.0) / 2.0

-- | Two times pi.
twoPi :: Float
twoPi = 2.0 * pi

-- | Minimum subdivision level.
minSubdivLevel :: Int
minSubdivLevel = 0

-- | Maximum subdivision level (6 yields ~80k triangles).
maxSubdivLevel :: Int
maxSubdivLevel = 6

-- | Threshold for detecting the north pole in spherical UV mapping.
northPoleThreshold :: Float
northPoleThreshold = 0.999

-- | Threshold for detecting the south pole in spherical UV mapping.
southPoleThreshold :: Float
southPoleThreshold = -0.999

-- | Threshold for detecting the UV seam (where U wraps from 1 to 0).
seamThreshold :: Float
seamThreshold = 0.5

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Generate a geodesic sphere (icosphere) centered at the origin.
--
-- Subdivision level 0 produces the base icosahedron (12 vertices,
-- 20 triangles). Each additional level splits every triangle into 4
-- via edge midpoints projected onto the sphere. The level is clamped
-- to [0, 6].
--
-- Returns 'Nothing' if the radius is not positive.
icosphere ::
  -- | Radius
  Float ->
  -- | Subdivision level (0 = icosahedron, clamped to [0, 6])
  Int ->
  Maybe Mesh
icosphere radius level
  | radius <= 0 = Nothing
  | otherwise =
      let clampedLevel = max minSubdivLevel (min maxSubdivLevel level)
          (positions, triangles) = buildSubdividedIcosphere clampedLevel
          scaledPositions = map (scaleV3 radius) positions
       in Just (buildMeshFromPositions radius scaledPositions triangles)

-- ----------------------------------------------------------------
-- Icosahedron base geometry
-- ----------------------------------------------------------------

-- | The 12 vertices of a regular icosahedron, normalized to unit length.
icosahedronVertices :: [V3]
icosahedronVertices =
  map
    normalizeV3
    [ V3 (-1) goldenRatio 0,
      V3 1 goldenRatio 0,
      V3 (-1) (negate goldenRatio) 0,
      V3 1 (negate goldenRatio) 0,
      V3 0 (-1) goldenRatio,
      V3 0 1 goldenRatio,
      V3 0 (-1) (negate goldenRatio),
      V3 0 1 (negate goldenRatio),
      V3 goldenRatio 0 (-1),
      V3 goldenRatio 0 1,
      V3 (negate goldenRatio) 0 (-1),
      V3 (negate goldenRatio) 0 1
    ]

-- | The 20 triangles of a regular icosahedron as index triples.
-- Winding order is counter-clockwise when viewed from outside.
icosahedronTriangles :: [(Int, Int, Int)]
icosahedronTriangles =
  [ -- 5 faces around vertex 0
    (0, 11, 5),
    (0, 5, 1),
    (0, 1, 7),
    (0, 7, 10),
    (0, 10, 11),
    -- 5 adjacent faces
    (1, 5, 9),
    (5, 11, 4),
    (11, 10, 2),
    (10, 7, 6),
    (7, 1, 8),
    -- 5 faces around vertex 3
    (3, 9, 4),
    (3, 4, 2),
    (3, 2, 6),
    (3, 6, 8),
    (3, 8, 9),
    -- 5 adjacent faces
    (4, 9, 5),
    (2, 4, 11),
    (6, 2, 10),
    (8, 6, 7),
    (9, 8, 1)
  ]

-- ----------------------------------------------------------------
-- Subdivision
-- ----------------------------------------------------------------

-- | Build the subdivided icosphere geometry.
-- Returns positions (normalized to unit sphere) and triangle index triples.
buildSubdividedIcosphere :: Int -> ([V3], [(Int, Int, Int)])
buildSubdividedIcosphere levels =
  let initialPositions = icosahedronVertices
      initialTriangles = icosahedronTriangles
   in applySubdivisionLevels levels initialPositions initialTriangles

-- | Apply the given number of subdivision levels.
applySubdivisionLevels ::
  Int -> [V3] -> [(Int, Int, Int)] -> ([V3], [(Int, Int, Int)])
applySubdivisionLevels levels positions triangles
  | levels <= 0 = (positions, triangles)
  | otherwise =
      let (newPositions, newTriangles) =
            subdivideOnce positions triangles
       in applySubdivisionLevels (levels - 1) newPositions newTriangles

-- | Perform a single subdivision pass.
-- Each triangle is split into 4 by adding midpoints on edges.
-- New vertices are projected onto the unit sphere.
-- Uses a Map to deduplicate shared edge midpoints.
-- Positions are stored in an Array for O(1) lookup; new midpoints
-- are accumulated via O(1) prepend and reversed at the end.
subdivideOnce ::
  [V3] -> [(Int, Int, Int)] -> ([V3], [(Int, Int, Int)])
subdivideOnce positions triangles =
  let posCount = length positions
      posArray = listArray (0, posCount - 1) positions
      -- Accumulate new midpoints (reversed) and new triangles (reversed)
      (_finalMidpointMap, newMidpointsRev, finalTriangles, _) =
        foldl'
          ( \(!midMap, !accNewPos, !accTris, !nextIdx) (ia, ib, ic) ->
              let posA = lookupPosArray posArray posCount ia
                  posB = lookupPosArray posArray posCount ib
                  posC = lookupPosArray posArray posCount ic
                  -- Get or create midpoint for each edge
                  (midAB, midMap1, accNewPos1, nextIdx1) =
                    getOrCreateMidpoint ia ib posA posB midMap accNewPos nextIdx
                  (midBC, midMap2, accNewPos2, nextIdx2) =
                    getOrCreateMidpoint ib ic posB posC midMap1 accNewPos1 nextIdx1
                  (midCA, midMap3, accNewPos3, nextIdx3) =
                    getOrCreateMidpoint ic ia posC posA midMap2 accNewPos2 nextIdx2
                  -- 4 new triangles (prepend for O(1) per iteration)
                  newTris =
                    [ (midAB, midBC, midCA),
                      (midCA, midBC, ic),
                      (midAB, ib, midBC),
                      (ia, midAB, midCA)
                    ]
               in (midMap3, accNewPos3, newTris ++ accTris, nextIdx3)
          )
          (Map.empty, [], [], posCount)
          triangles
   in (positions ++ reverse newMidpointsRev, reverse finalTriangles)

-- | Canonical edge key: smaller index first.
edgeKey :: Int -> Int -> (Int, Int)
edgeKey a b = if a < b then (a, b) else (b, a)

-- | O(1) position lookup from the Array, with bounds check.
lookupPosArray :: Array Int V3 -> Int -> Int -> V3
lookupPosArray arr size idx
  | idx >= 0 && idx < size = arr ! idx
  | otherwise = V3 0 0 0

-- | Look up an edge midpoint in the map, or create it.
-- The new vertex is the normalized midpoint of the two endpoints,
-- projected onto the unit sphere. New midpoints are prepended
-- (O(1)) to the accumulator and reversed by the caller.
getOrCreateMidpoint ::
  Int ->
  Int ->
  V3 ->
  V3 ->
  Map (Int, Int) Int ->
  [V3] ->
  Int ->
  (Int, Map (Int, Int) Int, [V3], Int)
getOrCreateMidpoint idxA idxB posA posB midMap newMidpoints nextIdx =
  let key = edgeKey idxA idxB
   in case Map.lookup key midMap of
        Just existingIdx -> (existingIdx, midMap, newMidpoints, nextIdx)
        Nothing ->
          let midpoint = normalizeV3 (midpointV3 posA posB)
              newMap = Map.insert key nextIdx midMap
           in (nextIdx, newMap, midpoint : newMidpoints, nextIdx + 1)

-- ----------------------------------------------------------------
-- Mesh construction with UVs and tangents
-- ----------------------------------------------------------------

-- | Build the final Mesh from sphere positions and triangles.
-- Computes spherical UVs, duplicates vertices at the UV seam,
-- and computes analytical normals and tangents.
buildMeshFromPositions ::
  Float -> [V3] -> [(Int, Int, Int)] -> Mesh
buildMeshFromPositions radius positions triangles =
  let -- Compute UV for each position (on unit sphere direction)
      uvList = map (sphericalUV . normalizeV3) positions
      posCount = length positions
      posArray = listArray (0, posCount - 1) positions
      uvArray = listArray (0, posCount - 1) uvList
      -- Process triangles, duplicating seam vertices as needed.
      -- Thread a vertex count to avoid O(n) length calls.
      (finalVerts, finalIndices, _) =
        foldl'
          (processTriangle posArray uvArray posCount)
          ([], [], 0 :: Int)
          triangles
   in mkMesh (reverse finalVerts) (reverse finalIndices)
  where
    processTriangle posArr uvArr posSize (!accVerts, !accIndices, !vertCount) (ia, ib, ic) =
      let posA = lookupPosArray posArr posSize ia
          posB = lookupPosArray posArr posSize ib
          posC = lookupPosArray posArr posSize ic
          uvA = lookupUVArray uvArr posSize ia
          uvB = lookupUVArray uvArr posSize ib
          uvC = lookupUVArray uvArr posSize ic
          -- Fix pole UVs: use average U of non-pole vertices
          normA = normalizeV3 posA
          normB = normalizeV3 posB
          normC = normalizeV3 posC
          (fixedUVA, fixedUVB, fixedUVC) =
            fixPoleUVs normA normB normC uvA uvB uvC
          -- Fix seam: if triangle crosses the U seam, wrap UVs
          (seamUVA, seamUVB, seamUVC) =
            fixSeamUVs fixedUVA fixedUVB fixedUVC
          -- Emit vertices
          idxBase = fromIntegral vertCount
          vertA = makeVertex radius posA normA seamUVA
          vertB = makeVertex radius posB normB seamUVB
          vertC = makeVertex radius posC normC seamUVC
          newVerts = vertC : vertB : vertA : accVerts
          newIndices =
            (idxBase + 2) : (idxBase + 1) : idxBase : accIndices
       in (newVerts, newIndices, vertCount + 3)

-- | Compute spherical UV coordinates from a unit direction vector.
-- U wraps around the equator via atan2, V runs pole to pole via acos.
sphericalUV :: V3 -> V2
sphericalUV (V3 nx ny nz) =
  let u = 0.5 + atan2 nz nx / twoPi
      v = acos (clampF (-1.0) 1.0 ny) / pi
   in V2 u v

-- | Fix UV coordinates for pole vertices.
-- At the poles, the U coordinate is undefined; we set it to the
-- average U of the other two vertices in the triangle.
fixPoleUVs :: V3 -> V3 -> V3 -> V2 -> V2 -> V2 -> (V2, V2, V2)
fixPoleUVs (V3 _ nyA _) (V3 _ nyB _) (V3 _ nyC _) uvA uvB uvC =
  let isPoleA = nyA > northPoleThreshold || nyA < southPoleThreshold
      isPoleB = nyB > northPoleThreshold || nyB < southPoleThreshold
      isPoleC = nyC > northPoleThreshold || nyC < southPoleThreshold
      extractU (V2 uVal _) = uVal
      extractV (V2 _ vVal) = vVal
      -- Average two U values with seam-aware unwrapping: if the two
      -- values straddle the seam (differ by more than seamThreshold),
      -- shift the smaller up by 1.0 before averaging, then wrap back.
      seamAverageU uLeft uRight =
        let (uL, uR) =
              if abs (uLeft - uRight) > seamThreshold
                then
                  if uLeft < uRight
                    then (uLeft + 1.0, uRight)
                    else (uLeft, uRight + 1.0)
                else (uLeft, uRight)
            avg = (uL + uR) / 2.0
         in if avg > 1.0 then avg - 1.0 else avg
      fixedA =
        if isPoleA
          then V2 (seamAverageU (extractU uvB) (extractU uvC)) (extractV uvA)
          else uvA
      fixedB =
        if isPoleB
          then V2 (seamAverageU (extractU uvA) (extractU uvC)) (extractV uvB)
          else uvB
      fixedC =
        if isPoleC
          then V2 (seamAverageU (extractU uvA) (extractU uvB)) (extractV uvC)
          else uvC
   in (fixedA, fixedB, fixedC)

-- | Fix UV coordinates across the seam where U wraps from ~1 to ~0.
-- If any pair of U values differs by more than 0.5, the smaller value
-- is shifted up by 1 to avoid the triangle spanning the entire texture.
fixSeamUVs :: V2 -> V2 -> V2 -> (V2, V2, V2)
fixSeamUVs (V2 ua va) (V2 ub vb) (V2 uc vc) =
  let needsSeamFix =
        abs (ua - ub) > seamThreshold
          || abs (ub - uc) > seamThreshold
          || abs (ua - uc) > seamThreshold
   in if needsSeamFix
        then
          let maxU = max ua (max ub uc)
              fixU u = if maxU - u > seamThreshold then u + 1.0 else u
           in (V2 (fixU ua) va, V2 (fixU ub) vb, V2 (fixU uc) vc)
        else (V2 ua va, V2 ub vb, V2 uc vc)

-- | Construct a Vertex with analytical normal and tangent.
-- Normal is just the normalized position direction.
-- Tangent is the spherical theta-derivative direction.
makeVertex :: Float -> V3 -> V3 -> V2 -> Vertex
makeVertex _radius position normal uv =
  let tangent = computeSphericalTangent normal
   in vertex position normal uv tangent

-- | Compute the tangent vector for a point on a sphere.
-- The tangent points in the direction of increasing longitude (phi),
-- which is the derivative of the spherical parameterization with
-- respect to the U coordinate.
computeSphericalTangent :: V3 -> V4
computeSphericalTangent (V3 nx _ny nz) =
  let -- Tangent = d(position)/d(phi), normalized
      -- For a sphere parameterized by (theta, phi):
      --   x = sin(theta) * cos(phi)
      --   z = sin(theta) * sin(phi)
      -- dx/dphi = -sin(phi) = -nz/sinTheta
      -- dz/dphi = cos(phi)  = nx/sinTheta
      -- At poles, fall back to a default tangent
      sinThetaSq = nx * nx + nz * nz
   in if sinThetaSq < nearZeroLength
        then V4 1 0 0 1
        else
          let invSinTheta = 1.0 / sqrt sinThetaSq
              tx = negate nz * invSinTheta
              tz = nx * invSinTheta
           in V4 tx 0 tz 1

-- ----------------------------------------------------------------
-- Vector helpers
-- ----------------------------------------------------------------

-- | Normalize a V3 to unit length.
normalizeV3 :: V3 -> V3
normalizeV3 (V3 x y z) =
  let len = sqrt (x * x + y * y + z * z)
   in if len < nearZeroLength
        then V3 0 0 0
        else V3 (x / len) (y / len) (z / len)

-- | Compute the midpoint of two V3 vectors.
midpointV3 :: V3 -> V3 -> V3
midpointV3 (V3 x1 y1 z1) (V3 x2 y2 z2) =
  V3
    ((x1 + x2) * 0.5)
    ((y1 + y2) * 0.5)
    ((z1 + z2) * 0.5)

-- | Scale a V3 by a scalar.
scaleV3 :: Float -> V3 -> V3
scaleV3 s (V3 x y z) = V3 (s * x) (s * y) (s * z)

-- | O(1) UV lookup from an Array, with bounds check.
lookupUVArray :: Array Int V2 -> Int -> Int -> V2
lookupUVArray arr size idx
  | idx >= 0 && idx < size = arr ! idx
  | otherwise = V2 0 0
