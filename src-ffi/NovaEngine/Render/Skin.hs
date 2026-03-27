-- | GPU skinning types and bone matrix upload.
--
-- 'SkinnedVertex' is the 80-byte GPU vertex type for skeletal
-- animation. Bone matrices are uploaded to an SSBO each frame
-- from the pure Haskell animation system.
module NovaEngine.Render.Skin
  ( -- * Skinned vertex (re-exported from Render.Types)
    SkinnedVertex (..),

    -- * Constants
    maxBones,
    boneSSBOSize,

    -- * Vertex conversion
    toSkinnedVertex,
    toSkinnedVertices,

    -- * Bone matrix computation
    computeBoneMatrices,
    uploadBoneMatrices,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..), pokeElemOff)
import NovaEngine.Animation.Pose (Pose, applyPoseFull)
import NovaEngine.Animation.Skeleton (Skeleton, skelJoints, skelRestPositions)
import NovaEngine.Animation.Skin (BoneWeight (..), SkinVertex (..))
import NovaEngine.Math.Matrix (identity, mulM44, rotation, translation)
import NovaEngine.Math.Quaternion (identityQuat)
import NovaEngine.Math.Types
import NovaEngine.Mesh.Types (Vertex (..))
import NovaEngine.Render.Buffer (Buffer, withMappedBuffer)
import NovaEngine.Render.Types (SkinnedVertex (..))

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Maximum bones supported by the GPU skinning shader.
maxBones :: Int
maxBones = 128

-- | Size in bytes of the bone SSBO (128 mat4 = 8192 bytes).
boneSSBOSize :: Word32
boneSSBOSize = fromIntegral maxBones * 64

-- ----------------------------------------------------------------
-- Vertex conversion
-- ----------------------------------------------------------------

-- | Convert a mesh 'Vertex' and its CPU skin binding into a
-- 'SkinnedVertex'. Pads bone influences to 4 with zero weight.
toSkinnedVertex :: Vertex -> SkinVertex -> SkinnedVertex
toSkinnedVertex vtx sv =
  let weights = take 4 (svWeights sv ++ repeat zeroBoneWeight)
      (w0, w1, w2, w3) = case weights of
        (a : b : c : d : _) -> (a, b, c, d)
        _ -> (zeroBoneWeight, zeroBoneWeight, zeroBoneWeight, zeroBoneWeight)
      indices =
        V4
          (fromIntegral (bwBone w0))
          (fromIntegral (bwBone w1))
          (fromIntegral (bwBone w2))
          (fromIntegral (bwBone w3))
      boneWts =
        V4
          (bwWeight w0)
          (bwWeight w1)
          (bwWeight w2)
          (bwWeight w3)
   in SkinnedVertex
        { skPosition = vPosition vtx,
          skNormal = vNormal vtx,
          skUV = vUV vtx,
          skTangent = vTangent vtx,
          skBoneIndices = indices,
          skBoneWeights = boneWts
        }

-- | Convert a list of mesh vertices and their skin bindings into
-- skinned vertices.
toSkinnedVertices :: [Vertex] -> [SkinVertex] -> [SkinnedVertex]
toSkinnedVertices = zipWith toSkinnedVertex

-- | Zero bone weight for padding.
zeroBoneWeight :: BoneWeight
zeroBoneWeight = BoneWeight 0 0.0

-- ----------------------------------------------------------------
-- Bone matrix computation
-- ----------------------------------------------------------------

-- | Compute the bone matrix array from a skeleton and pose.
--
-- Each bone matrix transforms a rest-pose vertex to its posed
-- position: @M(j) = T(posedPos) * R(posedRot) * T(-restPos)@.
--
-- Returns a list of exactly 'maxBones' matrices, padded with
-- identity for unused slots.
computeBoneMatrices :: Skeleton -> Pose -> [M44]
computeBoneMatrices skel pose =
  let (posedPositions, posedRotations) = applyPoseFull skel pose
      restPositions = skelRestPositions skel
      jointCount = IntMap.size (skelJoints skel)
      matrices =
        [ computeOneMatrix posedPositions posedRotations restPositions j
        | j <- [0 .. jointCount - 1]
        ]
      padding = replicate (maxBones - length matrices) identity
   in take maxBones (matrices ++ padding)

-- | Compute the bone matrix for a single joint.
computeOneMatrix ::
  IntMap.IntMap V3 ->
  IntMap.IntMap Quaternion ->
  IntMap.IntMap V3 ->
  Int ->
  M44
computeOneMatrix posedPos posedRot restPos jointId =
  let pPos = IntMap.findWithDefault vzero jointId posedPos
      pRot = IntMap.findWithDefault identityQuat jointId posedRot
      rPos = IntMap.findWithDefault vzero jointId restPos
   in mulM44
        (translation pPos `mulM44` rotation pRot)
        (translation (negateV rPos))

-- | Upload bone matrices to a mapped SSBO.
--
-- The buffer must have been created with 'createHostStorageBuffer'
-- at size 'boneSSBOSize'. Computes bone matrices from the skeleton
-- and pose, then writes them into the buffer.
uploadBoneMatrices :: Buffer -> Skeleton -> Pose -> IO ()
uploadBoneMatrices buf skel pose =
  withMappedBuffer buf $ \ptr -> do
    let matrices = computeBoneMatrices skel pose
        fp = castPtr ptr :: Ptr M44
    mapM_ (uncurry (pokeElemOff fp)) (zip [0 ..] matrices)
