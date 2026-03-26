-- | Camera with projection and view matrix computation.
--
-- Pure Haskell camera built on the existing 'Math.Matrix' projection
-- and view utilities.  Stores position, rotation (quaternion), and
-- projection parameters.  View matrix is computed analytically —
-- no full 4×4 inverse needed.
--
-- @
-- let cam = lookAtCamera (V3 0 3 10) (V3 0 0 0) (V3 0 1 0)
--             (Perspective (pi/4) (16/9) 0.1 1000)
--     vp  = viewProjectionMatrix cam
-- @
module NovaEngine.Scene.Camera
  ( -- * Types
    Projection (..),
    Camera (..),

    -- * Construction
    defaultCamera,
    lookAtCamera,

    -- * Matrices
    viewMatrix,
    projectionMatrix,
    viewProjectionMatrix,
  )
where

import NovaEngine.Math.Matrix
  ( lookAt,
    mulM44,
    ortho,
    perspective,
    rotation,
    translation,
  )
import NovaEngine.Math.Quaternion (identityQuat, inverseQuat)
import NovaEngine.Math.Types

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | Projection mode.
data Projection
  = -- | Perspective projection: FOV-Y (radians), aspect, near, far.
    Perspective !Float !Float !Float !Float
  | -- | Orthographic projection: left, right, bottom, top, near, far.
    Orthographic !Float !Float !Float !Float !Float !Float
  deriving (Show, Eq)

-- | Camera with position, orientation, and projection.
data Camera = Camera
  { cameraPosition :: !V3,
    cameraRotation :: !Quaternion,
    cameraProjection :: !Projection
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Construction
-- ----------------------------------------------------------------

-- | Default perspective camera at the origin looking down @-Z@.
--
-- Near plane 0.1, far plane 1000.
defaultCamera ::
  -- | Vertical field of view in radians
  Float ->
  -- | Aspect ratio (width / height)
  Float ->
  Camera
defaultCamera fov aspect =
  Camera vzero identityQuat (Perspective fov aspect 0.1 1000.0)

-- | Construct a camera looking from @eye@ toward @target@.
--
-- Uses 'Math.Matrix.lookAt' internally to derive the camera
-- rotation quaternion.
lookAtCamera :: V3 -> V3 -> V3 -> Projection -> Camera
lookAtCamera eye target up proj =
  let vm = lookAt eye target up
      viewRot = matToQuat vm
      camRot = inverseQuat viewRot
   in Camera eye camRot proj

-- ----------------------------------------------------------------
-- Matrices
-- ----------------------------------------------------------------

-- | View matrix from camera position and rotation.
--
-- Computed as @rotation(inverseQuat rot) * translation(-pos)@.
viewMatrix :: Camera -> M44
viewMatrix (Camera pos rot _) =
  rotation (inverseQuat rot) `mulM44` translation ((-1.0) *^ pos)

-- | Projection matrix (delegates to 'Math.Matrix.perspective' or
-- 'Math.Matrix.ortho').
projectionMatrix :: Camera -> M44
projectionMatrix (Camera _ _ proj) = case proj of
  Perspective fov aspect near far -> perspective fov aspect near far
  Orthographic l r b t near far -> ortho l r b t near far

-- | Combined view-projection matrix: @projection * view@.
viewProjectionMatrix :: Camera -> M44
viewProjectionMatrix cam =
  projectionMatrix cam `mulM44` viewMatrix cam

-- ----------------------------------------------------------------
-- Internal: rotation matrix → quaternion (Shepperd's method)
-- ----------------------------------------------------------------

-- | Extract a quaternion from the 3×3 rotation part of a 4×4 matrix.
matToQuat :: M44 -> Quaternion
matToQuat (M44 (V4 m00 m10 m20 _) (V4 m01 m11 m21 _) (V4 m02 m12 m22 _) _) =
  let tr = m00 + m11 + m22
   in if tr > 0
        then
          let s = 2.0 * sqrt (tr + 1.0)
           in Quaternion
                (0.25 * s)
                (V3 ((m21 - m12) / s) ((m02 - m20) / s) ((m10 - m01) / s))
        else
          if m00 > m11 && m00 > m22
            then
              let s = 2.0 * sqrt (1.0 + m00 - m11 - m22)
               in Quaternion
                    ((m21 - m12) / s)
                    (V3 (0.25 * s) ((m01 + m10) / s) ((m02 + m20) / s))
            else
              if m11 > m22
                then
                  let s = 2.0 * sqrt (1.0 + m11 - m00 - m22)
                   in Quaternion
                        ((m02 - m20) / s)
                        (V3 ((m01 + m10) / s) (0.25 * s) ((m12 + m21) / s))
                else
                  let s = 2.0 * sqrt (1.0 + m22 - m00 - m11)
                   in Quaternion
                        ((m10 - m01) / s)
                        (V3 ((m02 + m20) / s) ((m12 + m21) / s) (0.25 * s))
