-- | Joint rotations and forward kinematics.
--
-- A 'Pose' maps joint IDs to local rotations. 'applyPose' propagates
-- those rotations down the skeleton tree to produce world-space
-- positions for every joint.
module NovaEngine.Animation.Pose
  ( -- * Pose type
    Pose (..),

    -- * Construction
    restPose,
    singleJoint,
    fromList,

    -- * Forward kinematics
    applyPose,
    applyPoseFull,

    -- * Interpolation
    lerpPose,

    -- * Composition
    composePose,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import NovaEngine.Animation.Skeleton
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Pose type
-- ----------------------------------------------------------------

-- | A pose is a rotation per joint. Joints absent from the map
-- use the identity quaternion (no rotation).
newtype Pose = Pose {unPose :: IntMap Quaternion}
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Construction
-- ----------------------------------------------------------------

-- | The rest pose: all joints at identity orientation.
restPose :: Pose
restPose = Pose IntMap.empty

-- | Set a single joint's rotation.
singleJoint :: Int -> Quaternion -> Pose
singleJoint jid q = Pose (IntMap.singleton jid q)

-- | Build a pose from a list of @(jointId, rotation)@ pairs.
fromList :: [(Int, Quaternion)] -> Pose
fromList = Pose . IntMap.fromList

-- ----------------------------------------------------------------
-- Forward kinematics
-- ----------------------------------------------------------------

-- | Propagate pose rotations down the skeleton tree to produce
-- world-space positions for every joint.
--
-- Single O(n) pass over the joint tree.
applyPose :: Skeleton -> Pose -> IntMap V3
applyPose skel pose = fst (applyPoseFull skel pose)

-- | Full forward kinematics returning both world-space positions
-- and world-space rotations for every joint. Single O(n) pass.
--
-- For the root joint:
--
-- @
-- worldPos(root)  = localPos(root)
-- worldRot(root)  = poseRot(root)
-- @
--
-- For all other joints:
--
-- @
-- worldRot(j)  = worldRot(parent(j)) * poseRot(j)
-- worldPos(j)  = worldPos(parent(j)) + rotate(worldRot(parent(j)), localPos(j))
-- @
applyPoseFull :: Skeleton -> Pose -> (IntMap V3, IntMap Quaternion)
applyPoseFull skel pose = go (IntMap.empty, IntMap.empty) (skelRoot skel)
  where
    go (!posAcc, !rotAcc) jid =
      let joint = lookupJoint skel jid
          localRot = IntMap.findWithDefault identityQuat jid (unPose pose)
          parentId = jointParent joint
          (parentPos, parentRot) =
            if parentId == rootParent
              then (vzero, identityQuat)
              else
                ( IntMap.findWithDefault vzero parentId posAcc,
                  IntMap.findWithDefault identityQuat parentId rotAcc
                )
          worldRot = mulQuat parentRot localRot
          worldPos = parentPos ^+^ rotateV3 parentRot (jointLocal joint)
          posAccNew = IntMap.insert jid worldPos posAcc
          rotAccNew = IntMap.insert jid worldRot rotAcc
       in foldl' go (posAccNew, rotAccNew) (skelChildren skel jid)

-- ----------------------------------------------------------------
-- Interpolation
-- ----------------------------------------------------------------

-- | Interpolate between two poses. Each joint is interpolated
-- independently via spherical linear interpolation (slerp).
-- Joints present in only one pose interpolate toward/from
-- identity.
lerpPose :: Float -> Pose -> Pose -> Pose
lerpPose t poseA poseB =
  Pose $
    IntMap.mapWithKey
      ( \jid _ ->
          let qa = IntMap.findWithDefault identityQuat jid (unPose poseA)
              qb = IntMap.findWithDefault identityQuat jid (unPose poseB)
           in slerpQuat t qa qb
      )
      allKeys
  where
    allKeys = IntMap.union (unPose poseA) (unPose poseB)

-- ----------------------------------------------------------------
-- Composition
-- ----------------------------------------------------------------

-- | Compose two poses by multiplying local rotations per joint.
-- Used for additive blending: @composePose base additive@ applies
-- the additive layer on top of the base pose.
composePose :: Pose -> Pose -> Pose
composePose (Pose base) (Pose additive) =
  Pose $
    IntMap.mergeWithKey
      (\_ qBase qAdd -> Just (mulQuat qBase qAdd))
      id -- base-only joints keep their rotation
      id -- additive-only joints keep their rotation
      base
      additive
