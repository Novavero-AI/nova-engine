-- | Inverse kinematics solvers — CCD and FABRIK.
--
-- Two complementary IK solvers for articulated skeletons. CCD
-- (Cyclic Coordinate Descent) is simple and robust for real-time
-- use. FABRIK (Forward And Backward Reaching Inverse Kinematics)
-- produces natural-looking results by operating in position space.
module NovaEngine.Animation.IK
  ( -- * IK solvers
    solveCCD,
    solveConstrainedCCD,
    solveFABRIK,

    -- * Constraints
    JointConstraint (..),

    -- * Reachability
    fabrikReachable,

    -- * Helpers
    lookAt,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import NovaEngine.Animation.Pose
import NovaEngine.Animation.Skeleton
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Named constants
-- ----------------------------------------------------------------

-- | Convergence threshold for CCD: squared distance from end
-- effector to target below which we consider the chain converged.
ccdConvergenceThresholdSq :: Float
ccdConvergenceThresholdSq = 1.0e-6

-- | Minimum axis length for cross-product rotation axis. Below
-- this the vectors are nearly parallel or anti-parallel.
minAxisLength :: Float
minAxisLength = 1.0e-7

-- | Dot product is clamped to this range before passing to 'acos'.
dotClampMin :: Float
dotClampMin = -1.0

-- | Upper bound for dot clamp.
dotClampMax :: Float
dotClampMax = 1.0

-- | The +Y unit vector, used as the default forward direction
-- for 'lookAt'.
yAxis :: V3
yAxis = V3 0 1 0

-- ----------------------------------------------------------------
-- Joint constraints
-- ----------------------------------------------------------------

-- | Joint rotation constraint.
data JointConstraint
  = -- | Hinge constraint: rotation limited to a single axis within an angle range.
    HingeConstraint
      -- | Hinge axis (local space, unit length)
      !V3
      -- | Minimum angle (radians)
      !Float
      -- | Maximum angle (radians)
      !Float
  | -- | Cone constraint: rotation limited to a cone around a rest direction.
    ConeConstraint
      -- | Rest direction (local space, unit length)
      !V3
      -- | Half-angle of the cone (radians)
      !Float
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- CCD solver
-- ----------------------------------------------------------------

-- | CCD (Cyclic Coordinate Descent) IK solver.
--
-- Given a skeleton, an initial pose, a chain of joint IDs (ordered
-- from root to end effector), a target position, and a maximum
-- iteration count, iteratively adjusts joint rotations so the end
-- effector approaches the target.
--
-- Each iteration sweeps from the end-effector's parent back to the
-- chain root. For each joint it computes the rotation that aligns
-- the vector from the joint to the current end-effector position
-- with the vector from the joint to the target, then composes that
-- rotation into the joint's local rotation.
--
-- Returns the modified pose. The chain must contain at least two
-- joints (one parent and the end effector).
solveCCD :: Skeleton -> Pose -> [Int] -> V3 -> Int -> Pose
solveCCD _ pose [] _ _ = pose
solveCCD _ pose [_] _ _ = pose
solveCCD skel pose chain target maxIter =
  ccdLoop pose 0
  where
    endEffectorId = lastInt chain

    ccdLoop !currentPose !iter
      | iter >= maxIter = currentPose
      | effectorCloseEnough currentPose = currentPose
      | otherwise =
          let updatedPose = sweepChain skel currentPose chain endEffectorId target
           in ccdLoop updatedPose (iter + 1)

    effectorCloseEnough currentPose =
      let worldPositions = applyPose skel currentPose
          effectorPos = IntMap.findWithDefault vzero endEffectorId worldPositions
          delta = target ^-^ effectorPos
       in vlengthSq delta < ccdConvergenceThresholdSq

-- | One CCD sweep: iterate from end-effector parent back to chain
-- root, adjusting each joint's rotation.
sweepChain :: Skeleton -> Pose -> [Int] -> Int -> V3 -> Pose
sweepChain skel pose chain endEffectorId target =
  foldl' (adjustJoint skel endEffectorId target) pose sweepOrder
  where
    -- Reverse the chain minus the end effector: sweep from
    -- end-effector parent back to chain root.
    sweepOrder = reverse (initSafe chain)

-- | Adjust a single joint's rotation so the end effector moves
-- toward the target.
adjustJoint :: Skeleton -> Int -> V3 -> Pose -> Int -> Pose
adjustJoint skel endEffectorId target pose jointId =
  let (worldPositions, worldRotations) = applyPoseFull skel pose
      jointPos = IntMap.findWithDefault vzero jointId worldPositions
      effectorPos = IntMap.findWithDefault vzero endEffectorId worldPositions
      toEffector = normalize (effectorPos ^-^ jointPos)
      toTarget = normalize (target ^-^ jointPos)
      deltaRot = rotationBetween toEffector toTarget
      -- Transform the world-space delta rotation into the joint's
      -- local space: localDelta = inverse(worldRot) * deltaRot * worldRot
      worldRot = IntMap.findWithDefault identityQuat jointId worldRotations
      localDelta = mulQuat (inverseQuat worldRot) (mulQuat deltaRot worldRot)
      currentLocal = IntMap.findWithDefault identityQuat jointId (unPose pose)
      newLocal = mulQuat currentLocal localDelta
   in Pose (IntMap.insert jointId newLocal (unPose pose))

-- ----------------------------------------------------------------
-- Constrained CCD solver
-- ----------------------------------------------------------------

-- | CCD solver with per-joint rotation constraints.
--
-- Works like 'solveCCD' but after adjusting each joint applies the
-- constraint for that joint (if one exists in the constraint map).
--
-- Supported constraints:
--
-- * 'HingeConstraint': projects the rotation onto the hinge axis and
--   clamps the angle to @[minAngle, maxAngle]@.
-- * 'ConeConstraint': if the rotated rest direction exceeds the cone
--   half-angle, clamps it back to the cone boundary.
solveConstrainedCCD ::
  Skeleton -> Pose -> [Int] -> IntMap JointConstraint -> V3 -> Int -> Pose
solveConstrainedCCD _ pose [] _ _ _ = pose
solveConstrainedCCD _ pose [_] _ _ _ = pose
solveConstrainedCCD skel pose chain constraints target maxIter =
  constrainedCCDLoop pose 0
  where
    endEffectorId = lastInt chain

    constrainedCCDLoop !currentPose !iter
      | iter >= maxIter = currentPose
      | effectorCloseEnough currentPose = currentPose
      | otherwise =
          let updatedPose = sweepChainConstrained skel currentPose chain endEffectorId target constraints
           in constrainedCCDLoop updatedPose (iter + 1)

    effectorCloseEnough currentPose =
      let worldPositions = applyPose skel currentPose
          effectorPos = IntMap.findWithDefault vzero endEffectorId worldPositions
          delta = target ^-^ effectorPos
       in vlengthSq delta < ccdConvergenceThresholdSq

-- | One constrained CCD sweep: iterate from end-effector parent back
-- to chain root, adjusting and constraining each joint's rotation.
sweepChainConstrained :: Skeleton -> Pose -> [Int] -> Int -> V3 -> IntMap JointConstraint -> Pose
sweepChainConstrained skel pose chain endEffectorId target constraints =
  foldl' (adjustJointConstrained skel endEffectorId target constraints) pose sweepOrder
  where
    sweepOrder = reverse (initSafe chain)

-- | Adjust a single joint's rotation toward the target, then apply
-- the constraint for that joint if one exists.
adjustJointConstrained :: Skeleton -> Int -> V3 -> IntMap JointConstraint -> Pose -> Int -> Pose
adjustJointConstrained skel endEffectorId target constraints pose jointId =
  let unconstrained = adjustJoint skel endEffectorId target pose jointId
      localRot = IntMap.findWithDefault identityQuat jointId (unPose unconstrained)
   in case IntMap.lookup jointId constraints of
        Nothing -> unconstrained
        Just constraint ->
          let constrained = applyConstraint constraint localRot
           in Pose (IntMap.insert jointId constrained (unPose unconstrained))

-- | Apply a joint constraint to a local rotation quaternion.
applyConstraint :: JointConstraint -> Quaternion -> Quaternion
applyConstraint (HingeConstraint axis minAngle maxAngle) rot =
  let -- Project the rotation onto the hinge axis.
      -- Extract the rotation angle about the hinge axis using
      -- atan2 of the projected sine/cosine components.
      (Quaternion qw (V3 qx qy qz)) = rot
      (V3 ax ay az) = axis
      -- Component of the quaternion vector part along the axis
      projectedSin = qx * ax + qy * ay + qz * az
      -- The angle encoded in the quaternion is half the rotation angle
      halfAngle = atan2 projectedSin qw
      angle = halfAngle * 2.0
      clampedAngle = clampF minAngle maxAngle angle
   in axisAngle axis clampedAngle
applyConstraint (ConeConstraint restDir halfAngle) rot =
  let -- Rotate the rest direction by the current rotation
      rotatedDir = rotateV3 rot restDir
      -- Compute the angle between the rest direction and the rotated direction
      d = clampF dotClampMin dotClampMax (dot restDir rotatedDir)
      currentAngle = acos d
   in if currentAngle <= halfAngle
        then rot -- Within the cone, no clamping needed
        else -- Clamp: find the rotation axis (from rest to rotated) and
        -- limit the angle to halfAngle
          let axis = cross restDir rotatedDir
              axisLen = vlength axis
           in if axisLen < minAxisLength
                then rot -- Parallel or anti-parallel, keep as-is
                else
                  let normalizedAxis = normalize axis
                   in axisAngle normalizedAxis halfAngle

-- ----------------------------------------------------------------
-- FABRIK solver
-- ----------------------------------------------------------------

-- | Check whether a target position is reachable by the IK chain.
--
-- Computes the total chain length (sum of bone lengths between
-- consecutive joints) and compares it to the distance from the
-- chain root to the target. Returns 'True' when the target is
-- within reach.
fabrikReachable :: Skeleton -> Pose -> [Int] -> V3 -> Bool
fabrikReachable _ _ [] _ = False
fabrikReachable _ _ [_] _ = False
fabrikReachable skel pose chain target =
  let worldPositions = applyPose skel pose
      chainPositions = map (\jid -> IntMap.findWithDefault vzero jid worldPositions) chain
      boneLengths = zipWith (\posA posB -> vlength (posB ^-^ posA)) chainPositions (drop 1 chainPositions)
      totalLength = sum boneLengths
      rootPos = case chainPositions of
        (p : _) -> p
        [] -> vzero -- unreachable: chain has >= 2 elements
      distanceToTarget = vlength (target ^-^ rootPos)
   in distanceToTarget <= totalLength

-- | FABRIK (Forward And Backward Reaching Inverse Kinematics) solver.
--
-- Operates in position space: alternately pulls the chain toward
-- the target (forward reaching) and anchors the root back to its
-- original position (backward reaching). After convergence, converts
-- the resulting world positions back to local rotations.
--
-- If the target is unreachable (distance from root to target exceeds
-- the total chain length), the chain is stretched directly toward the
-- target rather than iterating uselessly.
--
-- Parameters: skeleton, initial pose, chain joint IDs (root to end
-- effector), target position, distance tolerance, max iterations.
--
-- The chain must contain at least two joints.
solveFABRIK :: Skeleton -> Pose -> [Int] -> V3 -> Float -> Int -> Pose
solveFABRIK _ pose [] _ _ _ = pose
solveFABRIK _ pose [_] _ _ _ = pose
solveFABRIK skel pose chain target tolerance maxIter =
  let worldPositions = applyPose skel pose
      chainPositions = map (\jid -> IntMap.findWithDefault vzero jid worldPositions) chain
      boneLengths = zipWith (\posA posB -> vlength (posB ^-^ posA)) chainPositions (drop 1 chainPositions)
      rootPos = case chainPositions of
        (p : _) -> p
        [] -> vzero -- unreachable: chain has >= 2 elements
      totalLength = sum boneLengths
      distanceToTarget = vlength (target ^-^ rootPos)
      finalPositions =
        if distanceToTarget > totalLength
          then stretchTowardTarget rootPos boneLengths target
          else fabrikLoop chainPositions boneLengths rootPos 0
   in positionsToLocalRotations skel pose chain finalPositions
  where
    fabrikLoop !positions !lengths !anchorPos !iter
      | iter >= maxIter = positions
      | converged positions = positions
      | otherwise =
          let forwardPositions = forwardReach positions lengths target
              backwardPositions = backwardReach forwardPositions lengths anchorPos
           in fabrikLoop backwardPositions lengths anchorPos (iter + 1)

    converged chainPos =
      let effectorPos = lastV3 chainPos
          delta = target ^-^ effectorPos
       in vlength delta < tolerance

-- | Stretch a chain toward the target when it is unreachable.
-- Places each joint along the root-to-target direction at successive
-- bone-length intervals.
stretchTowardTarget :: V3 -> [Float] -> V3 -> [V3]
stretchTowardTarget rootPos boneLengths target =
  reverse (foldl' step [rootPos] boneLengths)
  where
    step :: [V3] -> Float -> [V3]
    step [] _ = [] -- unreachable
    step acc@(prevPos : _) boneLen =
      let direction = normalize (target ^-^ prevPos)
          adjustedDir =
            if vlengthSq direction < minAxisLength
              then yAxis
              else direction
          nextPos = prevPos ^+^ boneLen *^ adjustedDir
       in nextPos : acc

-- | Forward reaching pass: set end effector to target, then walk
-- backward placing each joint at bone-length distance from the next.
forwardReach :: [V3] -> [Float] -> V3 -> [V3]
forwardReach positions lengths target =
  reverse (foldl' step [target] (zip (reverse (initSafe positions)) (reverse lengths)))
  where
    step :: [V3] -> (V3, Float) -> [V3]
    step [] _ = [] -- unreachable
    step acc@(nextPos : _) (currentPos, boneLen) =
      let direction = normalize (currentPos ^-^ nextPos)
          adjustedDir =
            if vlengthSq direction < minAxisLength
              then yAxis
              else direction
          newPos = nextPos ^+^ boneLen *^ adjustedDir
       in newPos : acc

-- | Backward reaching pass: set root to original position, then walk
-- forward placing each joint at bone-length distance from the previous.
backwardReach :: [V3] -> [Float] -> V3 -> [V3]
backwardReach positions lengths anchorPos =
  reverse (foldl' step [anchorPos] (zip (drop 1 positions) lengths))
  where
    step :: [V3] -> (V3, Float) -> [V3]
    step [] _ = [] -- unreachable
    step acc@(prevPos : _) (currentPos, boneLen) =
      let direction = normalize (currentPos ^-^ prevPos)
          adjustedDir =
            if vlengthSq direction < minAxisLength
              then yAxis
              else direction
          newPos = prevPos ^+^ boneLen *^ adjustedDir
       in newPos : acc

-- | Convert FABRIK's world-space chain positions back to local
-- rotations. For each consecutive pair of joints in the chain,
-- compute the rotation that maps the original bone direction to
-- the new bone direction in the parent's local frame.
positionsToLocalRotations :: Skeleton -> Pose -> [Int] -> [V3] -> Pose
positionsToLocalRotations skel pose chain newPositions =
  fst (foldl' stepJoint (pose, origWorldRotations) bonePairs)
  where
    (origWorldPositions, origWorldRotations) = applyPoseFull skel pose
    -- Build (jointId, newPos) pairs for the chain
    chainPairs = zip chain newPositions
    -- Consecutive pairs represent bones from parent to child
    bonePairs = zip chainPairs (drop 1 chainPairs)

    stepJoint :: (Pose, IntMap Quaternion) -> ((Int, V3), (Int, V3)) -> (Pose, IntMap Quaternion)
    stepJoint (!currentPose, !accWorldRots) ((parentJid, parentNewPos), (childJid, childNewPos)) =
      let -- Original bone direction in world space
          origParentPos = IntMap.findWithDefault vzero parentJid origWorldPositions
          origChildPos = IntMap.findWithDefault vzero childJid origWorldPositions
          origDir = normalize (origChildPos ^-^ origParentPos)
          -- New bone direction in world space
          newDir = normalize (childNewPos ^-^ parentNewPos)
          -- World-space delta rotation from original to new direction
          worldDelta = rotationBetween origDir newDir
          -- Current accumulated world rotation for this joint
          currentWorldRot = IntMap.findWithDefault identityQuat parentJid accWorldRots
          -- Apply the delta in world space to get the new world rotation
          updatedWorldRot = mulQuat worldDelta currentWorldRot
          -- Convert to local: localRot = inverse(parentWorldRot) * updatedWorldRot
          joint = IntMap.findWithDefault (Joint parentJid rootParent vzero) parentJid (skelJoints skel)
          parentOfParentWorldRot =
            if jointParent joint == rootParent
              then identityQuat
              else IntMap.findWithDefault identityQuat (jointParent joint) accWorldRots
          newLocalRot = mulQuat (inverseQuat parentOfParentWorldRot) updatedWorldRot
          updatedPose = Pose (IntMap.insert parentJid newLocalRot (unPose currentPose))
          -- Cascade: also compute the child's updated world rotation
          -- so subsequent iterations see the effect of this rotation change
          childLocalRot = IntMap.findWithDefault identityQuat childJid (unPose currentPose)
          childWorldRot = mulQuat updatedWorldRot childLocalRot
          updatedWorldRots =
            IntMap.insert childJid childWorldRot
              . IntMap.insert parentJid updatedWorldRot
              $ accWorldRots
       in (updatedPose, updatedWorldRots)

-- ----------------------------------------------------------------
-- lookAt
-- ----------------------------------------------------------------

-- | Compute the quaternion that rotates the +Y direction to point
-- from @source@ toward @destination@.
--
-- If the two points coincide (distance below threshold), returns
-- the identity quaternion.
lookAt :: V3 -> V3 -> Quaternion
lookAt source destination =
  let direction = normalize (destination ^-^ source)
   in if vlengthSq direction < minAxisLength
        then identityQuat
        else rotationBetween yAxis direction

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Compute the quaternion that rotates unit vector @from@ to align
-- with unit vector @to@. Uses the cross product as rotation axis
-- and acos(dot) as the angle. Returns identity for near-parallel
-- vectors and a 180-degree rotation for near-antiparallel vectors.
rotationBetween :: V3 -> V3 -> Quaternion
rotationBetween from to =
  let d = clampF dotClampMin dotClampMax (dot from to)
      axis = cross from to
      axisLen = vlength axis
   in if axisLen < minAxisLength
        then
          if d > 0
            then identityQuat
            else -- Nearly opposite: pick an arbitrary perpendicular axis
              let perp = pickPerpendicular from
               in axisAngle perp pi
        else
          let angle = acos d
           in axisAngle (normalize axis) angle

-- | Safe last element for 'V3' lists. Returns 'vzero' for empty
-- lists. Only used for position lists that are guaranteed non-empty
-- by the caller guards.
lastV3 :: [V3] -> V3
lastV3 [] = vzero
lastV3 [x] = x
lastV3 (_ : xs) = lastV3 xs

-- | Safe last element for 'Int' lists. Returns @0@ for empty lists.
-- Only used for chain ID lists that are guaranteed non-empty by
-- the caller guards.
lastInt :: [Int] -> Int
lastInt [] = 0
lastInt [x] = x
lastInt (_ : xs) = lastInt xs

-- | Safe init: all elements except the last. Returns empty for
-- empty or singleton lists.
initSafe :: [a] -> [a]
initSafe [] = []
initSafe [_] = []
initSafe (x : xs) = x : initSafe xs
