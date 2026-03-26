-- | Ray-mesh intersection with BVH acceleration.
--
-- Provides brute-force and BVH-accelerated ray casting against
-- triangle meshes. The BVH is a binary tree of axis-aligned
-- bounding boxes constructed by median splitting along the longest
-- axis. All operations are pure and allocate no mutable state.
module NovaEngine.Spatial.Raycast
  ( -- * Types
    Ray (..),
    Hit (..),

    -- * Ray-triangle intersection
    rayTriangle,

    -- * Brute-force ray-mesh
    rayMesh,
    closestHit,

    -- * BVH acceleration
    BVH,
    buildBVH,
    rayBVH,
  )
where

import Data.Array (Array, listArray, (!))
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | A ray defined by an origin point and a direction vector.
data Ray = Ray
  { rayOrigin :: !V3,
    rayDirection :: !V3
  }
  deriving (Show, Eq)

-- | The result of a ray-mesh intersection test.
data Hit = Hit
  { -- | Distance along the ray to the hit point.
    hitDistance :: !Float,
    -- | World-space position of the hit.
    hitPosition :: !V3,
    -- | Interpolated surface normal at the hit.
    hitNormal :: !V3,
    -- | Interpolated UV coordinates at the hit.
    hitUV :: !V2,
    -- | Index of the triangle that was hit (face index, not vertex
    -- index).
    hitTriangleIndex :: !Int
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Ray-triangle intersection (Moller-Trumbore)
-- ----------------------------------------------------------------

-- | Moller-Trumbore ray-triangle intersection.
--
-- Returns @Just (t, u, v)@ where @t@ is the distance along the
-- ray and @(u, v)@ are barycentric coordinates of the hit point,
-- or 'Nothing' if the ray misses or hits from behind.
rayTriangle :: Ray -> (V3, V3, V3) -> Maybe (Float, Float, Float)
rayTriangle (Ray origin direction) (v0, v1, v2) =
  let edge1 = v1 ^-^ v0
      edge2 = v2 ^-^ v0
      h = cross direction edge2
      a = dot edge1 h
   in if abs a < rayEpsilon
        then Nothing
        else
          let f = 1.0 / a
              s = origin ^-^ v0
              u = f * dot s h
           in if u < 0.0 || u > 1.0
                then Nothing
                else
                  let q = cross s edge1
                      v = f * dot direction q
                   in if v < 0.0 || u + v > 1.0
                        then Nothing
                        else
                          let t = f * dot edge2 q
                           in if t > rayEpsilon
                                then Just (t, u, v)
                                else Nothing

-- ----------------------------------------------------------------
-- Brute-force ray-mesh
-- ----------------------------------------------------------------

-- | Test a ray against every triangle in a mesh and return the
-- closest hit.
--
-- This is an O(n) scan suitable for small meshes. For large meshes
-- use 'buildBVH' and 'rayBVH'.
rayMesh :: Ray -> Mesh -> Maybe Hit
rayMesh _ (Mesh [] _ _) = Nothing
rayMesh ray mesh =
  let verts = meshVertices mesh
      vertArr = listArray (0, length verts - 1) verts
      idxs = meshIndices mesh
      tris = groupTriangles idxs
   in findClosestHit ray vertArr tris 0 Nothing

-- | Alias for 'rayMesh' provided for convenience.
closestHit :: Ray -> Mesh -> Maybe Hit
closestHit = rayMesh

-- | Scan a list of indexed triangles and accumulate the closest
-- hit.
findClosestHit ::
  Ray ->
  Array Int Vertex ->
  [(Word32, Word32, Word32)] ->
  Int ->
  Maybe Hit ->
  Maybe Hit
findClosestHit _ _ [] _ best = best
findClosestHit ray vertArr ((i0, i1, i2) : rest) triIdx best =
  let v0 = vertArr ! fromIntegral i0
      v1 = vertArr ! fromIntegral i1
      v2 = vertArr ! fromIntegral i2
      p0 = vPosition v0
      p1 = vPosition v1
      p2 = vPosition v2
      candidate = do
        (t, u, v) <- rayTriangle ray (p0, p1, p2)
        pure (buildHit ray t u v v0 v1 v2 triIdx)
      next = case (candidate, best) of
        (Just h, Just b)
          | hitDistance h < hitDistance b -> Just h
          | otherwise -> Just b
        (Just h, Nothing) -> Just h
        (Nothing, _) -> best
   in findClosestHit ray vertArr rest (triIdx + 1) next

-- | Build a 'Hit' from barycentric coordinates by interpolating
-- vertex attributes.
buildHit :: Ray -> Float -> Float -> Float -> Vertex -> Vertex -> Vertex -> Int -> Hit
buildHit (Ray origin direction) t u v v0 v1 v2 triIdx =
  let w = 1.0 - u - v
      pos = origin ^+^ t *^ direction
      -- Interpolate normal
      n0 = vNormal v0
      n1 = vNormal v1
      n2 = vNormal v2
      interpNormal = safeNormalize (V3 0 1 0) (w *^ n0 ^+^ u *^ n1 ^+^ v *^ n2)
      -- Interpolate UV
      V2 u0x u0y = vUV v0
      V2 u1x u1y = vUV v1
      V2 u2x u2y = vUV v2
      interpUV = V2 (w * u0x + u * u1x + v * u2x) (w * u0y + u * u1y + v * u2y)
   in Hit
        { hitDistance = t,
          hitPosition = pos,
          hitNormal = interpNormal,
          hitUV = interpUV,
          hitTriangleIndex = triIdx
        }

-- ----------------------------------------------------------------
-- BVH
-- ----------------------------------------------------------------

-- | Axis-aligned bounding box.
data AABB = AABB
  { aabbMin :: !V3,
    aabbMax :: !V3
  }
  deriving (Show, Eq)

-- | Bounding volume hierarchy for accelerated ray casting.
--
-- Leaf nodes store up to 'bvhLeafSize' triangles. Internal nodes
-- partition triangles by median split along the longest AABB axis.
data BVH
  = -- | A leaf containing a bounding box and a list of
    -- @(triangleIndex, p0, p1, p2, vert0, vert1, vert2)@ tuples.
    BVHLeaf !AABB [(Int, V3, V3, V3, Vertex, Vertex, Vertex)]
  | -- | An internal node with a bounding box and two children.
    BVHNode !AABB BVH BVH
  deriving (Show, Eq)

-- | A triangle with precomputed centroid for BVH construction.
data BVHTri = BVHTri
  { btTriIdx :: !Int,
    btV0 :: !V3,
    btV1 :: !V3,
    btV2 :: !V3,
    btVert0 :: !Vertex,
    btVert1 :: !Vertex,
    btVert2 :: !Vertex,
    btCentroid :: !V3
  }

-- | Build a BVH from a mesh for accelerated ray casting.
--
-- Triangles are recursively partitioned by sorting along the
-- longest AABB axis and splitting at the median. Leaves contain
-- at most 'bvhLeafSize' triangles.
buildBVH :: Mesh -> BVH
buildBVH (Mesh [] _ _) = BVHLeaf emptyAABB []
buildBVH mesh =
  let verts = meshVertices mesh
      vertArr = listArray (0, length verts - 1) verts
      tris = groupTriangles (meshIndices mesh)
      bvhTris = zipWith (makeBVHTriArr vertArr) [0 ..] tris
   in buildBVHNode bvhTris

-- | Construct a BVH subtree from a list of triangles.
buildBVHNode :: [BVHTri] -> BVH
buildBVHNode [] = BVHLeaf emptyAABB []
buildBVHNode tris
  | length tris <= bvhLeafSize =
      let box = triListAABB tris
          leaves = [(btTriIdx t, btV0 t, btV1 t, btV2 t, btVert0 t, btVert1 t, btVert2 t) | t <- tris]
       in BVHLeaf box leaves
  | otherwise =
      let box = triListAABB tris
          axis = longestAxis box
          sorted = sortBy (comparing (axisComponent axis . btCentroid)) tris
          midpoint = length sorted `div` 2
          (leftTris, rightTris) = splitAt midpoint sorted
          leftChild = buildBVHNode leftTris
          rightChild = buildBVHNode rightTris
       in BVHNode box leftChild rightChild

-- | Test a ray against a BVH and return the closest hit.
--
-- The BVH is traversed recursively, pruning subtrees whose
-- bounding box the ray misses.
rayBVH :: Ray -> BVH -> Maybe Hit
rayBVH ray bvh = rayBVHNode ray bvh Nothing

-- | Recursive BVH traversal accumulating the closest hit found so
-- far.
rayBVHNode :: Ray -> BVH -> Maybe Hit -> Maybe Hit
rayBVHNode ray (BVHLeaf box leaves) best
  | not (rayAABB ray box) = best
  | otherwise = foldl' (testLeafTri ray) best leaves
rayBVHNode ray (BVHNode box left right) best
  | not (rayAABB ray box) = best
  | otherwise =
      let bestAfterLeft = rayBVHNode ray left best
       in rayBVHNode ray right bestAfterLeft

-- | Test a ray against a single leaf triangle and keep the closer
-- hit.
testLeafTri :: Ray -> Maybe Hit -> (Int, V3, V3, V3, Vertex, Vertex, Vertex) -> Maybe Hit
testLeafTri ray best (triIdx, p0, p1, p2, vert0, vert1, vert2) =
  case rayTriangle ray (p0, p1, p2) of
    Nothing -> best
    Just (t, u, v) ->
      let h = buildHit ray t u v vert0 vert1 vert2 triIdx
       in case best of
            Just b | hitDistance b <= t -> Just b
            _ -> Just h

-- ----------------------------------------------------------------
-- AABB helpers
-- ----------------------------------------------------------------

-- | An empty AABB at the origin.
emptyAABB :: AABB
emptyAABB = AABB (V3 0 0 0) (V3 0 0 0)

-- | Compute the AABB enclosing all triangles in a list.
triListAABB :: [BVHTri] -> AABB
triListAABB [] = emptyAABB
triListAABB (first : rest) =
  let initBox = triAABB first
   in foldl' (\acc t -> unionAABB acc (triAABB t)) initBox rest

-- | Compute the AABB of a single triangle.
triAABB :: BVHTri -> AABB
triAABB t =
  let V3 x0 y0 z0 = btV0 t
      V3 x1 y1 z1 = btV1 t
      V3 x2 y2 z2 = btV2 t
   in AABB
        (V3 (min x0 (min x1 x2)) (min y0 (min y1 y2)) (min z0 (min z1 z2)))
        (V3 (max x0 (max x1 x2)) (max y0 (max y1 y2)) (max z0 (max z1 z2)))

-- | Compute the union of two AABBs.
unionAABB :: AABB -> AABB -> AABB
unionAABB
  (AABB (V3 minAx minAy minAz) (V3 maxAx maxAy maxAz))
  (AABB (V3 minBx minBy minBz) (V3 maxBx maxBy maxBz)) =
    AABB
      (V3 (min minAx minBx) (min minAy minBy) (min minAz minBz))
      (V3 (max maxAx maxBx) (max maxAy maxBy) (max maxAz maxBz))

-- | Determine which axis has the longest extent in an AABB.
-- Returns 0 for X, 1 for Y, 2 for Z.
longestAxis :: AABB -> Int
longestAxis (AABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) =
  let dx = maxX - minX
      dy = maxY - minY
      dz = maxZ - minZ
   in if dx >= dy && dx >= dz
        then 0
        else
          if dy >= dz
            then 1
            else 2

-- | Extract the component of a 'V3' along the given axis index.
axisComponent :: Int -> V3 -> Float
axisComponent 0 (V3 x _ _) = x
axisComponent 1 (V3 _ y _) = y
axisComponent _ (V3 _ _ z) = z

-- | Ray-AABB intersection test using the slab method.
--
-- Returns 'True' if the ray intersects the box at any non-negative
-- distance.
rayAABB :: Ray -> AABB -> Bool
rayAABB (Ray (V3 ox oy oz) (V3 dx dy dz)) (AABB (V3 minX minY minZ) (V3 maxX maxY maxZ)) =
  let (txMin, txMax) = slabIntersect ox dx minX maxX
      (tyMin, tyMax) = slabIntersect oy dy minY maxY
      (tzMin, tzMax) = slabIntersect oz dz minZ maxZ
      tNear = max txMin (max tyMin tzMin)
      tFar = min txMax (min tyMax tzMax)
   in tNear <= tFar && tFar >= 0

-- | Compute the entry and exit distances for a ray along one axis
-- slab. When the ray direction component is near zero, the slab
-- spans all distances if the origin is inside, or no distances if
-- outside.
slabIntersect :: Float -> Float -> Float -> Float -> (Float, Float)
slabIntersect origin dir slabMin slabMax
  | abs dir < rayEpsilon =
      if origin >= slabMin && origin <= slabMax
        then (negInfinity, posInfinity)
        else (posInfinity, negInfinity)
  | otherwise =
      let invD = 1.0 / dir
          t1 = (slabMin - origin) * invD
          t2 = (slabMax - origin) * invD
       in if t1 <= t2 then (t1, t2) else (t2, t1)

-- ----------------------------------------------------------------
-- BVH construction helpers
-- ----------------------------------------------------------------

-- | Create a 'BVHTri' from an array of vertices.
makeBVHTriArr :: Array Int Vertex -> Int -> (Word32, Word32, Word32) -> BVHTri
makeBVHTriArr vertArr triIdx (i0, i1, i2) =
  let vert0 = vertArr ! fromIntegral i0
      vert1 = vertArr ! fromIntegral i1
      vert2 = vertArr ! fromIntegral i2
      p0 = vPosition vert0
      p1 = vPosition vert1
      p2 = vPosition vert2
      V3 cx cy cz = p0 ^+^ p1 ^+^ p2
      centroid = V3 (cx * centroidScale) (cy * centroidScale) (cz * centroidScale)
   in BVHTri triIdx p0 p1 p2 vert0 vert1 vert2 centroid

-- ----------------------------------------------------------------
-- Utilities
-- ----------------------------------------------------------------

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Epsilon for ray intersection tests. Hits closer than this
-- distance are rejected to avoid self-intersection.
rayEpsilon :: Float
rayEpsilon = 1.0e-6

-- | Maximum number of triangles in a BVH leaf node.
bvhLeafSize :: Int
bvhLeafSize = 4

-- | Scale factor for computing triangle centroids (1\/3).
centroidScale :: Float
centroidScale = 1.0 / 3.0

-- | Positive infinity for slab intersection fallback.
posInfinity :: Float
posInfinity = 1.0 / 0.0

-- | Negative infinity for slab intersection fallback.
negInfinity :: Float
negInfinity = -(1.0 / 0.0)
