-- | Mesh merging and transformation.
--
-- Translate, rotate, scale, flip, reverse, merge, and recompute
-- normals and tangents.
module NovaEngine.Mesh.Combine
  ( -- * Transforms
    translate,
    rotate,
    uniformScale,
    scaleXYZ,

    -- * Winding and normals
    flipNormals,
    reverseWinding,

    -- * Merging
    merge,

    -- * Recomputation
    recomputeNormals,
    recomputeTangents,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import NovaEngine.Mesh.Types
  ( Mesh (..),
    Quaternion,
    V2 (..),
    V3 (..),
    V4 (..),
    VecSpace (..),
    Vertex (..),
    Word32,
    cross,
    dot,
    groupTriangles,
    nearZeroLength,
    normalize,
    rotateV3,
  )

-- ----------------------------------------------------------------
-- Transforms
-- ----------------------------------------------------------------

-- | Offset all vertex positions by a displacement vector.
translate :: V3 -> Mesh -> Mesh
translate offset (Mesh vertices indices count) =
  Mesh (map moveVertex vertices) indices count
  where
    moveVertex v = v {vPosition = vPosition v ^+^ offset}

-- | Rotate all vertex positions, normals, and tangents by a
-- quaternion.
rotate :: Quaternion -> Mesh -> Mesh
rotate q (Mesh vertices indices count) =
  Mesh (map rotateVertex vertices) indices count
  where
    rotateVertex v =
      v
        { vPosition = rotateV3 q (vPosition v),
          vNormal = normalize (rotateV3 q (vNormal v)),
          vTangent = rotateTangent (vTangent v)
        }
    rotateTangent (V4 tx ty tz tw) =
      let V3 rx ry rz = normalize (rotateV3 q (V3 tx ty tz))
       in V4 rx ry rz tw

-- | Scale all vertex positions uniformly. Normals and tangent
-- directions are unchanged by uniform scaling. A negative factor
-- mirrors the mesh, so normals are negated and winding is reversed
-- to maintain correct face orientation.
uniformScale :: Float -> Mesh -> Mesh
uniformScale factor (Mesh vertices indices count)
  | factor < 0 =
      Mesh (map scaleAndFlip vertices) (swapPairs indices) count
  | otherwise =
      Mesh (map scaleVertex vertices) indices count
  where
    scaleVertex v = v {vPosition = factor *^ vPosition v}
    scaleAndFlip v =
      v
        { vPosition = factor *^ vPosition v,
          vNormal = negateV (vNormal v),
          vTangent = let V4 tx ty tz tw = vTangent v in V4 tx ty tz (negate tw)
        }

-- | Scale vertex positions by independent factors along each axis.
--
-- Normals are transformed by the inverse-transpose scale
-- @(1\/sx, 1\/sy, 1\/sz)@ and renormalized. If an odd number of
-- scale components is negative the mesh is mirrored, so winding
-- order is reversed and bitangent handedness is flipped to
-- maintain correct face orientation.
scaleXYZ :: V3 -> Mesh -> Mesh
scaleXYZ (V3 sx sy sz) (Mesh vertices indices count)
  | needsFlip =
      Mesh (map scaleAndFlipVertex vertices) (swapPairs indices) count
  | otherwise =
      Mesh (map scaleVertex vertices) indices count
  where
    -- A mirror occurs when an odd number of axes is negative
    negCount =
      (if sx < 0 then 1 :: Int else 0)
        + (if sy < 0 then 1 else 0)
        + (if sz < 0 then 1 else 0)
    needsFlip = odd negCount

    -- Inverse scale for normals (inverse-transpose of a diagonal
    -- scale matrix). Near-zero components use 0 to avoid NaN.
    invSx = safeRecip sx
    invSy = safeRecip sy
    invSz = safeRecip sz

    scaleVertex v =
      let V3 px py pz = vPosition v
          V3 nx ny nz = vNormal v
          V3 tx ty tz = tangentXYZ (vTangent v)
          tw = tangentW (vTangent v)
       in v
            { vPosition = V3 (sx * px) (sy * py) (sz * pz),
              vNormal = normalize (V3 (invSx * nx) (invSy * ny) (invSz * nz)),
              vTangent =
                let V3 stx sty stz = normalize (V3 (sx * tx) (sy * ty) (sz * tz))
                 in V4 stx sty stz tw
            }

    scaleAndFlipVertex v =
      let V3 px py pz = vPosition v
          V3 nx ny nz = vNormal v
          V3 tx ty tz = tangentXYZ (vTangent v)
          tw = tangentW (vTangent v)
       in v
            { vPosition = V3 (sx * px) (sy * py) (sz * pz),
              vNormal = negateV (normalize (V3 (invSx * nx) (invSy * ny) (invSz * nz))),
              vTangent =
                let V3 stx sty stz = normalize (V3 (sx * tx) (sy * ty) (sz * tz))
                 in V4 stx sty stz (negate tw)
            }

-- | Extract the xyz direction from a tangent vector.
tangentXYZ :: V4 -> V3
tangentXYZ (V4 tx ty tz _) = V3 tx ty tz

-- | Extract the w (handedness) component of a tangent vector.
tangentW :: V4 -> Float
tangentW (V4 _ _ _ tw) = tw

-- | Safe reciprocal that returns 0 for near-zero values.
safeRecip :: Float -> Float
safeRecip val
  | abs val < nearZeroLength = 0
  | otherwise = 1.0 / val

-- ----------------------------------------------------------------
-- Winding and normals
-- ----------------------------------------------------------------

-- | Negate all vertex normals.
flipNormals :: Mesh -> Mesh
flipNormals (Mesh vertices indices count) =
  Mesh (map flipVertex vertices) indices count
  where
    flipVertex v = v {vNormal = negateV (vNormal v)}

-- | Swap the second and third index of each triangle, reversing
-- the winding order.
reverseWinding :: Mesh -> Mesh
reverseWinding (Mesh vertices indices count) =
  Mesh vertices (swapPairs indices) count

-- | Swap indices within each triple to reverse winding order.
swapPairs :: [Word32] -> [Word32]
swapPairs (a : b : c : rest) = a : c : b : swapPairs rest
swapPairs remaining = remaining

-- ----------------------------------------------------------------
-- Merging
-- ----------------------------------------------------------------

-- | Combine a list of meshes into one. Vertex lists are concatenated
-- and indices are offset to match. This is the 'Monoid' fold.
merge :: [Mesh] -> Mesh
merge = mconcat

-- ----------------------------------------------------------------
-- Recomputation
-- ----------------------------------------------------------------

-- | Recompute vertex normals from triangle geometry.
--
-- For each triangle, the face normal is computed from edge cross
-- products. Normals are accumulated at each vertex weighted by
-- triangle area, then normalized. Use after any operation that
-- moves vertices (displacement, FFD, etc.).
recomputeNormals :: Mesh -> Mesh
recomputeNormals (Mesh vertices indices count) =
  Mesh updatedVertices indices count
  where
    vertexMap = IntMap.fromList (zip [0 ..] vertices)
    triangles = groupTriangles indices
    normalAccum = foldl' accumulateNormal IntMap.empty triangles
    updatedVertices = zipWith applyNormal [0 ..] vertices

    applyNormal idx v = case IntMap.lookup idx normalAccum of
      Just accNormal -> v {vNormal = normalize accNormal}
      Nothing -> v

    accumulateNormal !acc (i0, i1, i2) =
      case lookupPositions i0 i1 i2 of
        Just (p0, p1, p2) ->
          let edge1 = p1 ^-^ p0
              edge2 = p2 ^-^ p0
              faceNormal = cross edge1 edge2
           in addAtVertex i0 faceNormal
                . addAtVertex i1 faceNormal
                . addAtVertex i2 faceNormal
                $ acc
        Nothing -> acc

    lookupPositions i0 i1 i2 = do
      v0 <- IntMap.lookup (fromIntegral i0) vertexMap
      v1 <- IntMap.lookup (fromIntegral i1) vertexMap
      v2 <- IntMap.lookup (fromIntegral i2) vertexMap
      pure (vPosition v0, vPosition v1, vPosition v2)

    addAtVertex idx =
      IntMap.insertWith (^+^) (fromIntegral idx)

-- | Recompute tangent basis from mesh geometry and UVs.
--
-- Per-triangle tangent and bitangent are computed from UV gradients
-- (MikkTSpace-style), accumulated at shared vertices, then
-- orthogonalized against the normal via Gram-Schmidt. Bitangent
-- handedness is stored in tangent w. Use after any operation that
-- invalidates tangents (subdivision, deformation, normal
-- recomputation).
recomputeTangents :: Mesh -> Mesh
recomputeTangents (Mesh vertices indices count) =
  Mesh updatedVertices indices count
  where
    vertexMap = IntMap.fromList (zip [0 ..] vertices)
    triangles = groupTriangles indices
    (tangentAccum, bitangentAccum) =
      foldl' accumulateTangent (IntMap.empty, IntMap.empty) triangles
    updatedVertices = zipWith applyTangent [0 ..] vertices

    applyTangent idx v =
      case (IntMap.lookup idx tangentAccum, IntMap.lookup idx bitangentAccum) of
        (Just accTangent, Just accBitangent) ->
          let normal = vNormal v
              projected = dot normal accTangent *^ normal
              tangent = normalize (accTangent ^-^ projected)
              handedness =
                if dot (cross normal tangent) accBitangent >= 0
                  then 1.0
                  else -1.0
              V3 tx ty tz = tangent
           in v {vTangent = V4 tx ty tz handedness}
        _ -> v

    accumulateTangent (!tMap, !bMap) (i0, i1, i2) =
      case lookupVertexData i0 i1 i2 of
        Just (p0, p1, p2, uv0, uv1, uv2) ->
          let edge1 = p1 ^-^ p0
              edge2 = p2 ^-^ p0
              V2 du1 dv1 = uv1 ^-^ uv0
              V2 du2 dv2 = uv2 ^-^ uv0
              det = du1 * dv2 - du2 * dv1
           in if abs det < degenerateUVThreshold
                then (tMap, bMap)
                else
                  let invDet = 1.0 / det
                      tangent = invDet *^ (dv2 *^ edge1 ^-^ dv1 *^ edge2)
                      bitangent = invDet *^ (du1 *^ edge2 ^-^ du2 *^ edge1)
                      tUpdated =
                        addV3AtVertex i0 tangent
                          . addV3AtVertex i1 tangent
                          . addV3AtVertex i2 tangent
                          $ tMap
                      bUpdated =
                        addV3AtVertex i0 bitangent
                          . addV3AtVertex i1 bitangent
                          . addV3AtVertex i2 bitangent
                          $ bMap
                   in (tUpdated, bUpdated)
        Nothing -> (tMap, bMap)

    lookupVertexData i0 i1 i2 = do
      v0 <- IntMap.lookup (fromIntegral i0) vertexMap
      v1 <- IntMap.lookup (fromIntegral i1) vertexMap
      v2 <- IntMap.lookup (fromIntegral i2) vertexMap
      pure
        ( vPosition v0,
          vPosition v1,
          vPosition v2,
          vUV v0,
          vUV v1,
          vUV v2
        )

    addV3AtVertex idx =
      IntMap.insertWith (^+^) (fromIntegral idx)

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Threshold below which a UV triangle determinant is considered
-- degenerate.
degenerateUVThreshold :: Float
degenerateUVThreshold = 1.0e-10
