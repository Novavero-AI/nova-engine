-- | Generic joint tree for articulated figures.
--
-- Topology-agnostic skeleton — humanoids, quadrupeds, insects, trees,
-- robots, tentacles. Pure structure with no geometry opinions. Mesh
-- generation is a separate concern handled by composing with SDF,
-- Loft, Subdivision, and Deform.
module NovaEngine.Animation.Skeleton
  ( -- * Types
    Joint (..),
    Skeleton,

    -- * Construction
    mkSkeleton,

    -- * Queries
    skelJoints,
    skelRoot,
    skelChildren,
    skelBones,
    skelJointCount,

    -- * Rest pose
    skelRestPositions,

    -- * Joint lookup
    lookupJoint,

    -- * Convenience builders
    humanoid,
    quadruped,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | A joint in a skeleton. Each joint has a unique integer ID,
-- a parent ID (@-1@ for the root), and a local-space offset from
-- its parent.
data Joint = Joint
  { jointId :: !Int,
    jointParent :: !Int,
    jointLocal :: !V3
  }
  deriving (Show, Eq)

-- | A validated skeleton: a tree of joints with exactly one root.
data Skeleton = Skeleton
  { skelJointMap :: !(IntMap Joint),
    skelRootId :: !Int,
    skelChildMap :: !(IntMap [Int])
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Construction
-- ----------------------------------------------------------------

-- | Build a skeleton from a list of joints. Returns 'Nothing' if:
--
-- * The joint list is empty
-- * There is not exactly one root (parent == -1)
-- * Any non-root joint references a parent that does not exist
-- * Any joint ID is duplicated
mkSkeleton :: [Joint] -> Maybe Skeleton
mkSkeleton [] = Nothing
mkSkeleton joints
  | hasDuplicateIds = Nothing
  | not allParentsExist = Nothing
  | otherwise = case roots of
      [root] ->
        Just
          Skeleton
            { skelJointMap = jointMap,
              skelRootId = jointId root,
              skelChildMap = childMap
            }
      _ -> Nothing
  where
    jointMap = IntMap.fromList [(jointId j, j) | j <- joints]
    roots = filter (\j -> jointParent j == rootParent) joints
    hasDuplicateIds = IntMap.size jointMap /= length joints
    allParentsExist =
      all
        ( \j ->
            jointParent j == rootParent
              || IntMap.member (jointParent j) jointMap
        )
        joints

    childMap =
      foldl'
        ( \acc j ->
            if jointParent j == rootParent
              then acc
              else
                IntMap.insertWith
                  (++)
                  (jointParent j)
                  [jointId j]
                  acc
        )
        IntMap.empty
        joints

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | The joint map of a skeleton.
skelJoints :: Skeleton -> IntMap Joint
skelJoints = skelJointMap

-- | The root joint ID.
skelRoot :: Skeleton -> Int
skelRoot = skelRootId

-- | Direct children of a joint.
skelChildren :: Skeleton -> Int -> [Int]
skelChildren skel jid =
  IntMap.findWithDefault [] jid (skelChildMap skel)

-- | All (parent, child) bone pairs in the skeleton.
skelBones :: Skeleton -> [(Int, Int)]
skelBones skel =
  [ (jointParent j, jointId j)
  | j <- IntMap.elems (skelJointMap skel),
    jointParent j /= rootParent
  ]

-- | Total number of joints.
skelJointCount :: Skeleton -> Int
skelJointCount = IntMap.size . skelJointMap

-- ----------------------------------------------------------------
-- Rest pose
-- ----------------------------------------------------------------

-- | Compute world-space positions for all joints at rest pose
-- (identity rotations). Propagates local offsets down the tree
-- from the root.
skelRestPositions :: Skeleton -> IntMap V3
skelRestPositions skel = go IntMap.empty (skelRootId skel)
  where
    go !acc jid =
      let joint = IntMap.findWithDefault defaultJoint jid (skelJointMap skel)
          parentPos =
            if jointParent joint == rootParent
              then vzero
              else IntMap.findWithDefault vzero (jointParent joint) acc
          worldPos = parentPos ^+^ jointLocal joint
          accWithSelf = IntMap.insert jid worldPos acc
       in foldl' go accWithSelf (skelChildren skel jid)

-- ----------------------------------------------------------------
-- Convenience builders
-- ----------------------------------------------------------------

-- | Build a humanoid skeleton with proportions derived from the
-- given total height. ~15 joints: hips (root), spine, chest,
-- neck, head, 2x (shoulder, elbow, wrist), 2x (hip, knee, ankle).
--
-- Proportions follow the classical 7.5-head model used in
-- figure drawing.
humanoid :: Float -> Maybe Skeleton
humanoid height
  | height <= 0 = Nothing
  | otherwise = mkSkeleton joints
  where
    headSize = height / humanoidHeadRatio
    joints =
      [ Joint 0 rootParent (V3 0 (hipHeight * height) 0), -- hips (root)
        Joint 1 0 (V3 0 (spineLength * headSize) 0), -- spine
        Joint 2 1 (V3 0 (chestLength * headSize) 0), -- chest
        Joint 3 2 (V3 0 (neckLength * headSize) 0), -- neck
        Joint 4 3 (V3 0 (headLength * headSize) 0), -- head
        -- Left arm
        Joint 5 2 (V3 (shoulderWidth * headSize) 0 0), -- L shoulder
        Joint 6 5 (V3 0 (negate (upperArmLength * headSize)) 0), -- L elbow
        Joint 7 6 (V3 0 (negate (forearmLength * headSize)) 0), -- L wrist
        -- Right arm
        Joint 8 2 (V3 (negate (shoulderWidth * headSize)) 0 0), -- R shoulder
        Joint 9 8 (V3 0 (negate (upperArmLength * headSize)) 0), -- R elbow
        Joint 10 9 (V3 0 (negate (forearmLength * headSize)) 0), -- R wrist
        -- Left leg
        Joint 11 0 (V3 (hipWidth * headSize) 0 0), -- L hip
        Joint 12 11 (V3 0 (negate (thighLength * headSize)) 0), -- L knee
        Joint 13 12 (V3 0 (negate (shinLength * headSize)) 0), -- L ankle
        -- Right leg
        Joint 14 0 (V3 (negate (hipWidth * headSize)) 0 0), -- R hip
        Joint 15 14 (V3 0 (negate (thighLength * headSize)) 0), -- R knee
        Joint 16 15 (V3 0 (negate (shinLength * headSize)) 0) -- R ankle
      ]

-- | Build a quadruped skeleton. @bodyLength@ is nose-to-tail,
-- @height@ is ground to top of back. ~13 joints: spine chain
-- (root at center), 4 legs (hip/shoulder, knee/elbow, hoof/paw),
-- neck, head.
quadruped :: Float -> Float -> Maybe Skeleton
quadruped bodyLength height
  | bodyLength <= 0 = Nothing
  | height <= 0 = Nothing
  | otherwise = mkSkeleton joints
  where
    halfBody = bodyLength * halfBodyRatio
    legLength = height * quadLegRatio
    upperLeg = legLength * upperLegRatio
    lowerLeg = legLength * lowerLegRatio
    legSpread = bodyLength * quadLegSpread
    neckLen = bodyLength * quadNeckRatio
    headLen = bodyLength * quadHeadRatio

    joints =
      [ Joint 0 rootParent (V3 0 height 0), -- body center (root)
        Joint 1 0 (V3 halfBody 0 0), -- front spine
        Joint 2 0 (V3 (negate halfBody) 0 0), -- rear spine
        Joint 3 1 (V3 (neckLen * quadNeckForward) (neckLen * quadNeckUp) 0), -- neck
        Joint 4 3 (V3 headLen 0 0), -- head
        -- Front left leg
        Joint 5 1 (V3 0 0 legSpread), -- FL shoulder
        Joint 6 5 (V3 0 (negate upperLeg) 0), -- FL elbow
        Joint 7 6 (V3 0 (negate lowerLeg) 0), -- FL hoof
        -- Front right leg
        Joint 8 1 (V3 0 0 (negate legSpread)), -- FR shoulder
        Joint 9 8 (V3 0 (negate upperLeg) 0), -- FR elbow
        Joint 10 9 (V3 0 (negate lowerLeg) 0), -- FR hoof
        -- Rear left leg
        Joint 11 2 (V3 0 0 legSpread), -- RL hip
        Joint 12 11 (V3 0 (negate upperLeg) 0), -- RL knee
        Joint 13 12 (V3 0 (negate lowerLeg) 0), -- RL hoof
        -- Rear right leg
        Joint 14 2 (V3 0 0 (negate legSpread)), -- RR hip
        Joint 15 14 (V3 0 (negate upperLeg) 0), -- RR knee
        Joint 16 15 (V3 0 (negate lowerLeg) 0) -- RR hoof
      ]

-- ----------------------------------------------------------------
-- Named constants
-- ----------------------------------------------------------------

-- | Look up a joint by ID, falling back to a default. On validated
-- skeletons the fallback is unreachable.
lookupJoint :: Skeleton -> Int -> Joint
lookupJoint skel jid =
  IntMap.findWithDefault defaultJoint jid (skelJointMap skel)

-- | Default joint used when lookup fails (should not happen with
-- validated skeletons).
defaultJoint :: Joint
defaultJoint = Joint 0 rootParent vzero

-- | Classical 7.5-head figure proportion ratio.
humanoidHeadRatio :: Float
humanoidHeadRatio = 7.5

-- | Hips at ~53% of total height (navel height).
hipHeight :: Float
hipHeight = 0.53

-- | Spine: hips to chest base, ~1.2 head units.
spineLength :: Float
spineLength = 1.2

-- | Chest: chest base to shoulder line, ~0.8 head units.
chestLength :: Float
chestLength = 0.8

-- | Neck: shoulder line to chin, ~0.4 head units.
neckLength :: Float
neckLength = 0.4

-- | Head: chin to crown, ~1.0 head unit.
headLength :: Float
headLength = 1.0

-- | Shoulder offset from spine center, ~0.9 head units.
shoulderWidth :: Float
shoulderWidth = 0.9

-- | Upper arm: shoulder to elbow, ~1.3 head units.
upperArmLength :: Float
upperArmLength = 1.3

-- | Forearm: elbow to wrist, ~1.1 head units.
forearmLength :: Float
forearmLength = 1.1

-- | Hip joint offset from spine center, ~0.45 head units.
hipWidth :: Float
hipWidth = 0.45

-- | Thigh: hip to knee, ~1.8 head units.
thighLength :: Float
thighLength = 1.8

-- | Shin: knee to ankle, ~1.7 head units.
shinLength :: Float
shinLength = 1.7

-- | Quadruped leg length as ratio of body height.
quadLegRatio :: Float
quadLegRatio = 0.65

-- | Quadruped leg spread as ratio of body length.
quadLegSpread :: Float
quadLegSpread = 0.15

-- | Quadruped neck length as ratio of body length.
quadNeckRatio :: Float
quadNeckRatio = 0.25

-- | Quadruped head length as ratio of body length.
quadHeadRatio :: Float
quadHeadRatio = 0.2

-- | Quadruped neck forward component (cos ~60 deg).
quadNeckForward :: Float
quadNeckForward = 0.5

-- | Quadruped neck upward component (sin ~60 deg).
quadNeckUp :: Float
quadNeckUp = 0.866

-- | Half-body ratio for quadruped spine offset.
halfBodyRatio :: Float
halfBodyRatio = 0.5

-- | Upper leg ratio for quadruped leg split.
upperLegRatio :: Float
upperLegRatio = 0.55

-- | Lower leg ratio for quadruped leg split.
lowerLegRatio :: Float
lowerLegRatio = 0.45
