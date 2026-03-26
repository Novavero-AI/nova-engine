-- | Mesh morphing and blend shape deformation.
--
-- Linear interpolation between meshes, multi-target blend shapes,
-- and position-only morphing. All operations renormalize normals
-- after interpolation.
module NovaEngine.Animation.Morph
  ( -- * Full mesh morphing
    morphMesh,

    -- * Blend shapes
    blendShapes,

    -- * Position-only morphing
    morphPositions,
  )
where

import Data.List (foldl')
import NovaEngine.Mesh.Types
  ( Mesh (..),
    V2,
    V3 (..),
    V4 (..),
    VecSpace (..),
    Vertex (..),
    Word32,
    lerp,
    mkMesh,
    safeNormalize,
  )

-- ----------------------------------------------------------------
-- Full mesh morphing
-- ----------------------------------------------------------------

-- | Linearly interpolate between two meshes.
--
-- @t = 0@ gives the first mesh, @t = 1@ gives the second.
-- Interpolates positions, normals, UVs, and tangents.
-- If vertex counts differ, the shorter count is used and
-- indices are truncated to remain valid. Normals are
-- renormalized after interpolation.
morphMesh :: Float -> Mesh -> Mesh -> Mesh
morphMesh t meshA meshB =
  let verticesA = meshVertices meshA
      verticesB = meshVertices meshB
      blendedVertices = zipWith (lerpVertex t) verticesA verticesB
      vertexCount = length blendedVertices
      indices = truncateIndices vertexCount (meshIndices meshA)
   in mkMesh blendedVertices indices

-- | Interpolate all vertex attributes between two vertices.
-- Renormalizes the interpolated normal.
lerpVertex :: Float -> Vertex -> Vertex -> Vertex
lerpVertex t vertA vertB =
  let V4 tx ty tz tw = lerp t (vTangent vertA) (vTangent vertB)
      snappedW = if tw >= 0 then 1.0 else (-1.0)
   in Vertex
        { vPosition = lerp t (vPosition vertA) (vPosition vertB),
          vNormal = safeNormalize (V3 0 1 0) (lerp t (vNormal vertA) (vNormal vertB)),
          vUV = lerp t (vUV vertA) (vUV vertB),
          vTangent = V4 tx ty tz snappedW,
          vColor = lerp t (vColor vertA) (vColor vertB)
        }

-- ----------------------------------------------------------------
-- Blend shapes
-- ----------------------------------------------------------------

-- | Apply multiple weighted morph targets to a base mesh.
--
-- Each target is a @(weight, mesh)@ pair. The blended value for
-- each attribute is:
--
-- @blended = base + sum(weight_i * (target_i - base))@
--
-- If any target has fewer vertices than the base, only the
-- overlapping vertices are blended. Normals are renormalized
-- after blending.
blendShapes :: Mesh -> [(Float, Mesh)] -> Mesh
blendShapes baseMesh targets =
  let baseVertices = meshVertices baseMesh
      -- Pre-zip: for each target, pair its vertices with weight
      targetColumns = [(w, meshVertices m) | (w, m) <- targets]
      blendedVertices = blendAll baseVertices targetColumns
      indices = truncateIndices (length blendedVertices) (meshIndices baseMesh)
   in mkMesh blendedVertices indices
  where
    blendAll [] _ = []
    blendAll (baseVtx : restBase) columns =
      let deltas = collectDeltas baseVtx columns
          blendedVtx = applyDeltas baseVtx deltas
          advancedColumns = [(w, drop 1 vs) | (w, vs) <- columns]
       in blendedVtx : blendAll restBase advancedColumns

-- | Collect weighted deltas from all pre-zipped target columns for one vertex.
-- Each column is a @(weight, remainingVertices)@ pair where the head is the
-- corresponding target vertex. Returns accumulated (position, normal, uv, tangent) delta.
collectDeltas :: Vertex -> [(Float, [Vertex])] -> (V3, V3, V2, V4)
collectDeltas baseVtx =
  foldl' accumDelta (vzero, vzero, vzero, vzero)
  where
    basePos = vPosition baseVtx
    baseNrm = vNormal baseVtx
    baseUv = vUV baseVtx
    baseTan = vTangent baseVtx

    accumDelta (!accPos, !accNrm, !accUv, !accTan) (weight, targetVerts) =
      case targetVerts of
        [] -> (accPos, accNrm, accUv, accTan)
        (targetVtx : _) ->
          let deltaPos = vPosition targetVtx ^-^ basePos
              deltaNrm = vNormal targetVtx ^-^ baseNrm
              deltaUv = vUV targetVtx ^-^ baseUv
              deltaTan = vTangent targetVtx ^-^ baseTan
           in ( accPos ^+^ weight *^ deltaPos,
                accNrm ^+^ weight *^ deltaNrm,
                accUv ^+^ weight *^ deltaUv,
                accTan ^+^ weight *^ deltaTan
              )

-- | Apply accumulated deltas to a base vertex and renormalize the normal.
applyDeltas :: Vertex -> (V3, V3, V2, V4) -> Vertex
applyDeltas baseVtx (deltaPos, deltaNrm, deltaUv, deltaTan) =
  let V4 tx ty tz tw = vTangent baseVtx ^+^ deltaTan
      snappedW = if tw >= 0 then 1.0 else (-1.0)
   in Vertex
        { vPosition = vPosition baseVtx ^+^ deltaPos,
          vNormal = safeNormalize (V3 0 1 0) (vNormal baseVtx ^+^ deltaNrm),
          vUV = vUV baseVtx ^+^ deltaUv,
          vTangent = V4 tx ty tz snappedW,
          vColor = vColor baseVtx
        }

-- ----------------------------------------------------------------
-- Position-only morphing
-- ----------------------------------------------------------------

-- | Like 'morphMesh' but only interpolates positions.
--
-- Normals, UVs, and tangents are kept from the first mesh.
-- If vertex counts differ, the shorter count is used and
-- indices are truncated.
morphPositions :: Float -> Mesh -> Mesh -> Mesh
morphPositions t meshA meshB =
  let verticesA = meshVertices meshA
      verticesB = meshVertices meshB
      blendedVertices = zipWith (lerpPositionOnly t) verticesA verticesB
      vertexCount = length blendedVertices
      indices = truncateIndices vertexCount (meshIndices meshA)
   in mkMesh blendedVertices indices

-- | Interpolate only the position, keeping other attributes from
-- the first vertex.
lerpPositionOnly :: Float -> Vertex -> Vertex -> Vertex
lerpPositionOnly t vertA vertB =
  vertA {vPosition = lerp t (vPosition vertA) (vPosition vertB)}

-- ----------------------------------------------------------------
-- Utility helpers
-- ----------------------------------------------------------------

-- | Truncate an index list so that all indices are valid for
-- the given vertex count. Indices referencing beyond the count
-- cause the enclosing triangle to be dropped.
truncateIndices :: Int -> [Word32] -> [Word32]
truncateIndices vertexCount = go
  where
    maxIndex = fromIntegral vertexCount

    go (idxA : idxB : idxC : rest)
      | idxA < maxIndex && idxB < maxIndex && idxC < maxIndex =
          idxA : idxB : idxC : go rest
      | otherwise = go rest
    go _ = []
