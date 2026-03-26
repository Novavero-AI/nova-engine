-- | Scene graph with transform hierarchy.
--
-- Pure Haskell entity system backed by 'IntMap'.  Each node has a
-- local 'Transform' (position, rotation, scale) and a parent link.
-- World-space transforms are computed via a single O(n) DFS pass,
-- following the same pattern as 'NovaEngine.Animation.Pose.applyPoseFull'.
--
-- @
-- let scene0 = emptyScene
--     (childId, scene1) = addNode (sceneRoot scene0) myTransform scene0
--     matrices = worldMatrices scene1
-- @
module NovaEngine.Scene
  ( -- * Transform
    Transform (..),
    identityTransform,
    composeTransform,
    transformToM44,

    -- * Node
    Node (..),

    -- * Scene
    Scene,
    emptyScene,
    addNode,
    removeNode,
    setTransform,
    getTransform,
    reparent,
    sceneNodeCount,
    sceneRoot,

    -- * World-space
    worldTransforms,
    worldMatrices,
    worldTransformOf,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import NovaEngine.Math.Matrix (mulM44, rotation, scaling, translation)
import NovaEngine.Math.Quaternion (identityQuat, mulQuat, rotateV3)
import NovaEngine.Math.Types

-- ----------------------------------------------------------------
-- Transform
-- ----------------------------------------------------------------

-- | Translation + rotation + scale (TRS).
--
-- The canonical transform representation for scene nodes.
-- Compose with 'composeTransform', bake to a model matrix with
-- 'transformToM44'.
data Transform = Transform
  { transformPosition :: !V3,
    transformRotation :: !Quaternion,
    transformScale :: !V3
  }
  deriving (Show, Eq)

-- | Identity transform: origin, no rotation, unit scale.
identityTransform :: Transform
identityTransform = Transform vzero identityQuat (V3 1 1 1)

-- | Compose two transforms: @parent * child@.
--
-- @
-- worldScale = parentScale * childScale  (component-wise)
-- worldRot   = parentRot * childRot
-- worldPos   = parentPos + rotate(parentRot, parentScale * childPos)
-- @
composeTransform :: Transform -> Transform -> Transform
composeTransform parent child =
  Transform
    { transformPosition =
        transformPosition parent
          ^+^ rotateV3
            (transformRotation parent)
            (mulV3 (transformScale parent) (transformPosition child)),
      transformRotation =
        mulQuat (transformRotation parent) (transformRotation child),
      transformScale =
        mulV3 (transformScale parent) (transformScale child)
    }

-- | Bake to a 4×4 model matrix: @translation * rotation * scaling@.
transformToM44 :: Transform -> M44
transformToM44 (Transform pos rot sca) =
  translation pos `mulM44` rotation rot `mulM44` scaling sca

-- ----------------------------------------------------------------
-- Node
-- ----------------------------------------------------------------

-- | A scene node with a parent link and local transform.
--
-- Parent ID @-1@ indicates the root node.
data Node = Node
  { nodeParent :: !Int,
    nodeTransform :: !Transform
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Scene
-- ----------------------------------------------------------------

-- | Scene graph: an 'IntMap' of nodes with parent-child links.
data Scene = Scene
  { sceneNodes :: !(IntMap Node),
    sceneChildMap :: !(IntMap [Int]),
    sceneRootId :: !Int,
    sceneNextId :: !Int
  }
  deriving (Show, Eq)

-- | Empty scene with a single root node at the origin.
emptyScene :: Scene
emptyScene =
  Scene
    { sceneNodes = IntMap.singleton 0 (Node (-1) identityTransform),
      sceneChildMap = IntMap.empty,
      sceneRootId = 0,
      sceneNextId = 1
    }

-- | Root node ID.
sceneRoot :: Scene -> Int
sceneRoot = sceneRootId

-- | Number of nodes in the scene.
sceneNodeCount :: Scene -> Int
sceneNodeCount = IntMap.size . sceneNodes

-- | Add a child node under the given parent.
--
-- Returns @(newNodeId, updatedScene)@.  If the parent does not
-- exist, returns @(-1, scene)@ unchanged.
addNode :: Int -> Transform -> Scene -> (Int, Scene)
addNode parentId xform scene
  | not (IntMap.member parentId (sceneNodes scene)) = (-1, scene)
  | otherwise =
      let nid = sceneNextId scene
          node = Node parentId xform
          nodes' = IntMap.insert nid node (sceneNodes scene)
          children' =
            IntMap.insertWith (++) parentId [nid] (sceneChildMap scene)
       in ( nid,
            scene
              { sceneNodes = nodes',
                sceneChildMap = children',
                sceneNextId = nid + 1
              }
          )

-- | Remove a node and all its descendants.  The root cannot be
-- removed.
removeNode :: Int -> Scene -> Scene
removeNode nodeId scene
  | nodeId == sceneRootId scene = scene
  | otherwise =
      case IntMap.lookup nodeId (sceneNodes scene) of
        Nothing -> scene
        Just node ->
          let allToRemove = nodeId : collectDescendants nodeId scene
              nodes' =
                foldl' (flip IntMap.delete) (sceneNodes scene) allToRemove
              children' =
                foldl'
                  (flip IntMap.delete)
                  (sceneChildMap scene)
                  allToRemove
              children'' =
                IntMap.adjust
                  (filter (/= nodeId))
                  (nodeParent node)
                  children'
           in scene {sceneNodes = nodes', sceneChildMap = children''}

-- | Set the local transform of a node.
setTransform :: Int -> Transform -> Scene -> Scene
setTransform nodeId xform scene =
  case IntMap.lookup nodeId (sceneNodes scene) of
    Nothing -> scene
    Just node ->
      scene
        { sceneNodes =
            IntMap.insert
              nodeId
              node {nodeTransform = xform}
              (sceneNodes scene)
        }

-- | Get the local transform of a node.
getTransform :: Int -> Scene -> Maybe Transform
getTransform nodeId scene =
  nodeTransform <$> IntMap.lookup nodeId (sceneNodes scene)

-- | Move a node under a new parent.
--
-- Fails silently (returns the scene unchanged) if the node is the
-- root, either ID is invalid, or reparenting would create a cycle.
reparent :: Int -> Int -> Scene -> Scene
reparent nodeId newParentId scene
  | nodeId == sceneRootId scene = scene
  | nodeId == newParentId = scene
  | not (IntMap.member nodeId (sceneNodes scene)) = scene
  | not (IntMap.member newParentId (sceneNodes scene)) = scene
  | newParentId `elem` collectDescendants nodeId scene = scene
  | otherwise =
      case IntMap.lookup nodeId (sceneNodes scene) of
        Nothing -> scene
        Just node ->
          let oldParent = nodeParent node
              node' = node {nodeParent = newParentId}
              nodes' = IntMap.insert nodeId node' (sceneNodes scene)
              children' =
                IntMap.adjust
                  (filter (/= nodeId))
                  oldParent
                  (sceneChildMap scene)
              children'' =
                IntMap.insertWith (++) newParentId [nodeId] children'
           in scene {sceneNodes = nodes', sceneChildMap = children''}

-- ----------------------------------------------------------------
-- World-space
-- ----------------------------------------------------------------

-- | Compute world-space transforms for all nodes.
--
-- Single O(n) DFS pass from the root, composing parent and child
-- transforms at each level.
worldTransforms :: Scene -> IntMap Transform
worldTransforms scene =
  go IntMap.empty (sceneRootId scene) identityTransform
  where
    go !acc nodeId parentWorld =
      case IntMap.lookup nodeId (sceneNodes scene) of
        Nothing -> acc
        Just node ->
          let world = composeTransform parentWorld (nodeTransform node)
              acc' = IntMap.insert nodeId world acc
              children =
                IntMap.findWithDefault [] nodeId (sceneChildMap scene)
           in foldl' (\a c -> go a c world) acc' children

-- | Compute world-space 4×4 model matrices for all nodes.
worldMatrices :: Scene -> IntMap M44
worldMatrices = fmap transformToM44 . worldTransforms

-- | Look up a single node's world transform by walking up to root.
--
-- O(depth) — use 'worldTransforms' if you need many or all of them.
worldTransformOf :: Int -> Scene -> Maybe Transform
worldTransformOf nodeId scene
  | not (IntMap.member nodeId (sceneNodes scene)) = Nothing
  | otherwise =
      Just (foldl' composeTransform identityTransform (buildChain nodeId []))
  where
    buildChain nid acc =
      case IntMap.lookup nid (sceneNodes scene) of
        Nothing -> acc
        Just n ->
          if nodeParent n == -1
            then nodeTransform n : acc
            else buildChain (nodeParent n) (nodeTransform n : acc)

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Component-wise V3 multiply (for scale composition).
mulV3 :: V3 -> V3 -> V3
mulV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)

-- | Collect all descendant node IDs.
collectDescendants :: Int -> Scene -> [Int]
collectDescendants nodeId scene =
  let kids = IntMap.findWithDefault [] nodeId (sceneChildMap scene)
   in kids ++ concatMap (`collectDescendants` scene) kids
