-- | 4×4 matrix operations for view, projection, and model transforms.
--
-- All matrices are column-major to match Vulkan and OpenGL conventions.
-- Vulkan uses a clip space with Y pointing down and depth [0,1], so
-- 'perspective' and 'ortho' apply the Vulkan correction.
module NovaEngine.Math.Matrix
  ( -- * Construction
    identity,
    translation,
    scaling,
    uniformScaling,
    rotation,

    -- * Projection
    perspective,
    ortho,

    -- * View
    lookAt,

    -- * Operations
    mulM44,
    mulM44V4,
    transpose,
  )
where

import NovaEngine.Math.Types
import NovaEngine.Math.Vector (cross, dot, normalize)

-- | Identity matrix.
identity :: M44
identity =
  M44
    (V4 1 0 0 0)
    (V4 0 1 0 0)
    (V4 0 0 1 0)
    (V4 0 0 0 1)

-- | Translation matrix.
translation :: V3 -> M44
translation (V3 tx ty tz) =
  M44
    (V4 1 0 0 0)
    (V4 0 1 0 0)
    (V4 0 0 1 0)
    (V4 tx ty tz 1)

-- | Non-uniform scaling matrix.
scaling :: V3 -> M44
scaling (V3 sx sy sz) =
  M44
    (V4 sx 0 0 0)
    (V4 0 sy 0 0)
    (V4 0 0 sz 0)
    (V4 0 0 0 1)

-- | Uniform scaling matrix.
uniformScaling :: Float -> M44
uniformScaling s = scaling (V3 s s s)

-- | Rotation matrix from a quaternion.
rotation :: Quaternion -> M44
rotation (Quaternion w (V3 x y z)) =
  let xx = x * x
      yy = y * y
      zz = z * z
      xy = x * y
      xz = x * z
      yz = y * z
      wx = w * x
      wy = w * y
      wz = w * z
   in M44
        (V4 (1 - 2 * (yy + zz)) (2 * (xy + wz)) (2 * (xz - wy)) 0)
        (V4 (2 * (xy - wz)) (1 - 2 * (xx + zz)) (2 * (yz + wx)) 0)
        (V4 (2 * (xz + wy)) (2 * (yz - wx)) (1 - 2 * (xx + yy)) 0)
        (V4 0 0 0 1)

-- | Perspective projection matrix for Vulkan clip space.
--
-- Y is flipped (Vulkan Y points down) and depth maps to [0,1].
perspective ::
  -- | Vertical field of view in radians
  Float ->
  -- | Aspect ratio (width / height)
  Float ->
  -- | Near plane distance
  Float ->
  -- | Far plane distance
  Float ->
  M44
perspective fovY aspect near far =
  let tanHalf = tan (fovY * 0.5)
      m00 = 1.0 / (aspect * tanHalf)
      m11 = -(1.0 / tanHalf) -- Vulkan Y-flip
      m22 = far / (near - far) -- Vulkan depth [0,1]
      m23 = -1.0
      m32 = (near * far) / (near - far)
   in M44
        (V4 m00 0 0 0)
        (V4 0 m11 0 0)
        (V4 0 0 m22 m23)
        (V4 0 0 m32 0)

-- | Orthographic projection matrix for Vulkan clip space.
ortho ::
  -- | Left
  Float ->
  -- | Right
  Float ->
  -- | Bottom
  Float ->
  -- | Top
  Float ->
  -- | Near
  Float ->
  -- | Far
  Float ->
  M44
ortho l r b t near far =
  let m00 = 2.0 / (r - l)
      m11 = -(2.0 / (t - b)) -- Vulkan Y-flip
      m22 = 1.0 / (near - far) -- Vulkan depth [0,1]
      m30 = -((r + l) / (r - l))
      m31 = -((t + b) / (t - b))
      m32 = near / (near - far)
   in M44
        (V4 m00 0 0 0)
        (V4 0 m11 0 0)
        (V4 0 0 m22 0)
        (V4 m30 m31 m32 1)

-- | View matrix from eye position, target, and up vector.
lookAt :: V3 -> V3 -> V3 -> M44
lookAt eye target up =
  let fwd = normalize (eye ^-^ target)
      right = normalize (cross up fwd)
      camUp = cross fwd right
      V3 rx ry rz = right
      V3 ux uy uz = camUp
      V3 fx fy fz = fwd
   in M44
        (V4 rx ux fx 0)
        (V4 ry uy fy 0)
        (V4 rz uz fz 0)
        (V4 (-(dot right eye)) (-(dot camUp eye)) (-(dot fwd eye)) 1)

-- | Multiply two 4×4 matrices.
mulM44 :: M44 -> M44 -> M44
mulM44 a (M44 b0 b1 b2 b3) =
  M44
    (mulM44V4 a b0)
    (mulM44V4 a b1)
    (mulM44V4 a b2)
    (mulM44V4 a b3)

-- | Multiply a 4×4 matrix by a column vector.
mulM44V4 :: M44 -> V4 -> V4
mulM44V4 (M44 (V4 a00 a10 a20 a30) (V4 a01 a11 a21 a31) (V4 a02 a12 a22 a32) (V4 a03 a13 a23 a33)) (V4 x y z w) =
  V4
    (a00 * x + a01 * y + a02 * z + a03 * w)
    (a10 * x + a11 * y + a12 * z + a13 * w)
    (a20 * x + a21 * y + a22 * z + a23 * w)
    (a30 * x + a31 * y + a32 * z + a33 * w)

-- | Transpose a 4×4 matrix.
transpose :: M44 -> M44
transpose (M44 (V4 a00 a10 a20 a30) (V4 a01 a11 a21 a31) (V4 a02 a12 a22 a32) (V4 a03 a13 a23 a33)) =
  M44
    (V4 a00 a01 a02 a03)
    (V4 a10 a11 a12 a13)
    (V4 a20 a21 a22 a23)
    (V4 a30 a31 a32 a33)
