-- | Skeletal mesh skinning with linear blend and dual quaternion modes.
--
-- Computes per-vertex bone weights (by proximity or manually) and
-- deforms a mesh according to a posed skeleton. Each vertex is
-- influenced by up to 'maxInfluences' bones.
module NovaEngine.Animation.Skin
  ( -- * Types
    BoneWeight (..),
    SkinVertex (..),
    SkinBinding (..),
    DualQuat (..),

    -- * Constants
    maxInfluences,

    -- * Weight manipulation
    normalizeWeights,

    -- * Automatic binding
    buildSkinBinding,

    -- * Skinning
    applySkin,
    applySkinDQ,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import NovaEngine.Animation.Pose (Pose, applyPoseFull)
import NovaEngine.Animation.Skeleton (Skeleton, skelBones, skelRestPositions, skelRoot)
import NovaEngine.Mesh.Types
  ( Mesh (..),
    Quaternion (..),
    V3 (..),
    VecSpace (..),
    Vertex (..),
    clampF,
    dot,
    identityQuat,
    inverseQuat,
    mkMesh,
    mulQuat,
    nearZeroLength,
    normalize,
    rotateV3,
    vlength,
  )

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | A single bone influence: which joint and how much weight.
data BoneWeight = BoneWeight
  { bwBone :: !Int,
    bwWeight :: !Float
  }
  deriving (Show, Eq)

-- | Per-vertex bone influences, up to 'maxInfluences' entries.
newtype SkinVertex = SkinVertex
  { svWeights :: [BoneWeight]
  }
  deriving (Show, Eq)

-- | Binding that maps each mesh vertex to its bone influences.
-- The list has one 'SkinVertex' per mesh vertex, in order.
newtype SkinBinding = SkinBinding
  { skinWeights :: [SkinVertex]
  }
  deriving (Show, Eq)

-- | Dual quaternion representation for rigid transforms.
data DualQuat = DualQuat !Quaternion !Quaternion
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Maximum number of bone influences per vertex.
maxInfluences :: Int
maxInfluences = 4

-- ----------------------------------------------------------------
-- Weight manipulation
-- ----------------------------------------------------------------

-- | Normalize bone weights so they sum to 1.0. If the total weight
-- is near zero, all weights are set to zero to avoid division by
-- zero.
normalizeWeights :: SkinVertex -> SkinVertex
normalizeWeights (SkinVertex weights) =
  SkinVertex (map scaleWeight weights)
  where
    totalWeight = foldl' (\acc bw -> acc + bwWeight bw) 0 weights
    scaleWeight bw
      | totalWeight < nearZeroWeight = bw {bwWeight = 0}
      | otherwise = bw {bwWeight = bwWeight bw / totalWeight}

-- ----------------------------------------------------------------
-- Automatic binding
-- ----------------------------------------------------------------

-- | Compute skin weights automatically by proximity. For each mesh
-- vertex, find the closest bones (using closest point on the bone
-- segment for distance), assign weights inversely proportional to
-- distance, and normalize.
-- The 'Float' parameter is a falloff radius — bones farther than
-- this distance receive zero weight.
buildSkinBinding :: Skeleton -> Mesh -> Float -> SkinBinding
buildSkinBinding skel mesh falloffRadius =
  SkinBinding (map bindVertex (meshVertices mesh))
  where
    restPositions = skelRestPositions skel
    bones = skelBones skel

    -- Precompute bone segments: for each (parent, child) pair,
    -- store the joint ID and the segment endpoints.
    boneSegments :: [(Int, V3, V3)]
    boneSegments =
      [ (childId, parentPos, childPos)
      | (parentId, childId) <- bones,
        let parentPos = IntMap.findWithDefault vzero parentId restPositions,
        let childPos = IntMap.findWithDefault vzero childId restPositions
      ]

    -- Also include the root joint itself as a potential influence,
    -- since it has no parent bone.
    rootId = skelRoot skel
    rootPos = IntMap.findWithDefault vzero rootId restPositions

    bindVertex :: Vertex -> SkinVertex
    bindVertex vtx =
      let vertexPos = vPosition vtx
          -- Distance to root (point, not segment)
          rootDist = vlength (vertexPos ^-^ rootPos)
          -- Compute distance to closest point on each bone segment
          segDists :: [(Int, Float)]
          segDists =
            [ (boneId, vlength (vertexPos ^-^ closestPt))
            | (boneId, segA, segB) <- boneSegments,
              let closestPt = closestPointOnSegment segA segB vertexPos
            ]
          distances :: [(Int, Float)]
          distances = (rootId, rootDist) : segDists
          -- Sort by distance and take the closest N
          sorted = take maxInfluences (sortBy (comparing snd) distances)
          -- Assign inverse-distance weights, respecting falloff
          rawWeights =
            [ BoneWeight boneId (inverseDistanceWeight dist)
            | (boneId, dist) <- sorted
            ]
       in normalizeWeights (SkinVertex rawWeights)

    inverseDistanceWeight :: Float -> Float
    inverseDistanceWeight dist
      | dist >= falloffRadius = 0
      | dist < nearZeroWeight = 1
      | otherwise = 1.0 / dist

-- ----------------------------------------------------------------
-- Skinning
-- ----------------------------------------------------------------

-- | Apply linear blend skinning. For each vertex:
--
-- 1. Look up the world-space transform (position + rotation) for
--    each influencing bone from the posed skeleton.
-- 2. Compute the rest-pose inverse transform for each bone.
-- 3. For each influence: @transform = worldTransform * inverseRestTransform@
-- 4. Blend: @finalPos = sum(weight_i * transform_i * restPos)@
-- 5. Transform normals with rotation only (no translation).
applySkin :: Skeleton -> Pose -> SkinBinding -> Mesh -> Mesh
applySkin skel pose binding mesh =
  mkMesh skinnedVertices (meshIndices mesh)
  where
    -- Posed world-space positions and rotations
    (posedPositions, posedRotations) = applyPoseFull skel pose
    -- Rest-pose world-space positions and rotations (identity rotations)
    restPositions = skelRestPositions skel

    skinnedVertices :: [Vertex]
    skinnedVertices =
      zipWith skinVertex (meshVertices mesh) (skinWeights binding)

    skinVertex :: Vertex -> SkinVertex -> Vertex
    skinVertex vtx skinVtx =
      let restPos = vPosition vtx
          restNrm = vNormal vtx
          influences = svWeights skinVtx
          -- Accumulate blended position and normal
          (blendedPos, blendedNrm) =
            foldl' (blendInfluence restPos restNrm) (vzero, vzero) influences
          finalNrm = normalize blendedNrm
       in vtx {vPosition = blendedPos, vNormal = finalNrm}

    blendInfluence ::
      V3 ->
      V3 ->
      (V3, V3) ->
      BoneWeight ->
      (V3, V3)
    blendInfluence restPos restNrm (!accPos, !accNrm) bw =
      let boneId = bwBone bw
          weight = bwWeight bw
          -- Posed transform for this bone
          posePos = IntMap.findWithDefault vzero boneId posedPositions
          poseRot = IntMap.findWithDefault identityQuat boneId posedRotations
          -- Rest transform for this bone (identity rotation at rest)
          restBonePos = IntMap.findWithDefault vzero boneId restPositions
          -- Combined transform: worldTransform * inverseRestTransform
          -- For a point p in rest pose:
          --   1. Remove rest transform: p_local = p - restBonePos
          --   2. Apply posed transform: p_posed = poseRot * p_local + posePos
          localPos = restPos ^-^ restBonePos
          transformedPos = rotateV3 poseRot localPos ^+^ posePos
          -- Normal: rotate only (no translation)
          transformedNrm = rotateV3 poseRot restNrm
       in (accPos ^+^ weight *^ transformedPos, accNrm ^+^ weight *^ transformedNrm)

-- ----------------------------------------------------------------
-- Dual quaternion skinning
-- ----------------------------------------------------------------

-- | Build a dual quaternion from a rotation and translation.
mkDualQuat :: Quaternion -> V3 -> DualQuat
mkDualQuat rot (V3 tx ty tz) =
  DualQuat rot (mulQuat (Quaternion 0 (V3 (0.5 * tx) (0.5 * ty) (0.5 * tz))) rot)

-- | Extract rotation and translation from a dual quaternion.
-- Translation is the vector part of @2 * dual * conjugate(real)@.
fromDualQuat :: DualQuat -> (Quaternion, V3)
fromDualQuat (DualQuat real dual) =
  let Quaternion _ trans = mulQuat (scaleQuat 2 dual) (inverseQuat real)
   in (real, trans)

-- | Apply dual quaternion skinning. Eliminates the \'candy wrapper\'
-- artifact of linear blend skinning for joints that twist significantly.
applySkinDQ :: Skeleton -> Pose -> SkinBinding -> Mesh -> Mesh
applySkinDQ skel pose binding mesh =
  mkMesh skinnedVertices (meshIndices mesh)
  where
    (posedPositions, posedRotations) = applyPoseFull skel pose
    restPositions = skelRestPositions skel

    skinnedVertices :: [Vertex]
    skinnedVertices =
      zipWith skinVertex (meshVertices mesh) (skinWeights binding)

    skinVertex :: Vertex -> SkinVertex -> Vertex
    skinVertex vtx skinVtx =
      let restPos = vPosition vtx
          restNrm = vNormal vtx
          influences = svWeights skinVtx

          -- Build per-bone dual quaternions (relative transform)
          boneDQs :: [(Float, DualQuat)]
          boneDQs =
            [ (bwWeight bw, boneDQ)
            | bw <- influences,
              let boneId = bwBone bw,
              let posePos = IntMap.findWithDefault vzero boneId posedPositions,
              let poseRot = IntMap.findWithDefault identityQuat boneId posedRotations,
              let restBonePos = IntMap.findWithDefault vzero boneId restPositions,
              -- Relative transform: posed * inverse(rest)
              -- rest is identity rotation, so inverse is just negating position
              let relTrans = posePos ^-^ rotateV3 poseRot restBonePos,
              let boneDQ = mkDualQuat poseRot relTrans
            ]

          -- Blend dual quaternions with sign coherence
          blended = blendDualQuats boneDQs
          -- Extract rotation and translation
          (finalRot, finalTrans) = fromDualQuat blended

          -- Apply to vertex
          blendedPos = rotateV3 finalRot restPos ^+^ finalTrans
          blendedNrm = normalize (rotateV3 finalRot restNrm)
       in vtx {vPosition = blendedPos, vNormal = blendedNrm}

-- | Blend a list of weighted dual quaternions with sign coherence.
-- The first dual quaternion is used as the reference for sign checks.
blendDualQuats :: [(Float, DualQuat)] -> DualQuat
blendDualQuats [] = DualQuat identityQuat (Quaternion 0 vzero)
blendDualQuats ((w0, dq0) : rest) =
  normalizeDQ (foldl' blendOne (scaleDQ w0 dq0) rest)
  where
    DualQuat ref0 _ = dq0
    blendOne :: DualQuat -> (Float, DualQuat) -> DualQuat
    blendOne acc (wi, dqi) =
      let -- Sign coherence: negate if real parts point in opposite directions
          dqiCoherent = if dotQuat ref0 (dqReal dqi) < 0 then negateDQ dqi else dqi
       in addDQ acc (scaleDQ wi dqiCoherent)

-- ----------------------------------------------------------------
-- Internal: dual quaternion arithmetic
-- ----------------------------------------------------------------

-- | Extract the real (rotation) part of a dual quaternion.
dqReal :: DualQuat -> Quaternion
dqReal (DualQuat r _) = r

-- | Scale both parts of a dual quaternion by a scalar.
scaleDQ :: Float -> DualQuat -> DualQuat
scaleDQ s (DualQuat (Quaternion rw rv) (Quaternion dw dv)) =
  DualQuat (Quaternion (s * rw) (s *^ rv)) (Quaternion (s * dw) (s *^ dv))

-- | Add two dual quaternions component-wise.
addDQ :: DualQuat -> DualQuat -> DualQuat
addDQ (DualQuat (Quaternion rw1 rv1) (Quaternion dw1 dv1)) (DualQuat (Quaternion rw2 rv2) (Quaternion dw2 dv2)) =
  DualQuat
    (Quaternion (rw1 + rw2) (rv1 ^+^ rv2))
    (Quaternion (dw1 + dw2) (dv1 ^+^ dv2))

-- | Negate both parts of a dual quaternion.
negateDQ :: DualQuat -> DualQuat
negateDQ = scaleDQ (-1)

-- | Normalize a dual quaternion by the length of the real part.
normalizeDQ :: DualQuat -> DualQuat
normalizeDQ dq@(DualQuat real dual)
  | len < nearZeroLength = dq
  | otherwise = DualQuat (scaleQuat invLen real) (scaleQuat invLen dual)
  where
    len = quatLength real
    invLen = 1.0 / len

-- | Dot product of two quaternions (treated as 4D vectors).
dotQuat :: Quaternion -> Quaternion -> Float
dotQuat (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2

-- | Scale a quaternion by a scalar.
scaleQuat :: Float -> Quaternion -> Quaternion
scaleQuat s (Quaternion w (V3 x y z)) =
  Quaternion (s * w) (V3 (s * x) (s * y) (s * z))

-- | Length of a quaternion (treated as a 4D vector).
quatLength :: Quaternion -> Float
quatLength q = sqrt (dotQuat q q)

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Closest point on a line segment to a given point.
closestPointOnSegment :: V3 -> V3 -> V3 -> V3
closestPointOnSegment segA segB point =
  let ab = segB ^-^ segA
      toPoint = point ^-^ segA
      t = clampF 0 1 (dot toPoint ab / max nearZeroLength (dot ab ab))
   in segA ^+^ t *^ ab

-- | Threshold below which a weight sum is considered zero.
nearZeroWeight :: Float
nearZeroWeight = 1.0e-10
