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
    inverse,
    mkTransformM44,
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

-- | Inverse of a 4×4 matrix via cofactor expansion.
-- Returns 'identity' for singular matrices.
inverse :: M44 -> M44
inverse (M44 (V4 m00 m10 m20 m30) (V4 m01 m11 m21 m31) (V4 m02 m12 m22 m32) (V4 m03 m13 m23 m33)) =
  let s0 = m00 * m11 - m10 * m01
      s1 = m00 * m12 - m10 * m02
      s2 = m00 * m13 - m10 * m03
      s3 = m01 * m12 - m11 * m02
      s4 = m01 * m13 - m11 * m03
      s5 = m02 * m13 - m12 * m03
      c5 = m22 * m33 - m32 * m23
      c4 = m21 * m33 - m31 * m23
      c3 = m21 * m32 - m31 * m22
      c2 = m20 * m33 - m30 * m23
      c1 = m20 * m32 - m30 * m22
      c0 = m20 * m31 - m30 * m21
      det = s0 * c5 - s1 * c4 + s2 * c3 + s3 * c2 - s4 * c1 + s5 * c0
   in if abs det < 1.0e-12
        then identity
        else
          let invDet = 1.0 / det
           in M44
                ( V4
                    ((m11 * c5 - m12 * c4 + m13 * c3) * invDet)
                    ((negate m10 * c5 + m12 * c2 - m13 * c1) * invDet)
                    ((m10 * c4 - m11 * c2 + m13 * c0) * invDet)
                    ((negate m10 * c3 + m11 * c1 - m12 * c0) * invDet)
                )
                ( V4
                    ((negate m01 * c5 + m02 * c4 - m03 * c3) * invDet)
                    ((m00 * c5 - m02 * c2 + m03 * c1) * invDet)
                    ((negate m00 * c4 + m01 * c2 - m03 * c0) * invDet)
                    ((m00 * c3 - m01 * c1 + m02 * c0) * invDet)
                )
                ( V4
                    ((m31 * s5 - m32 * s4 + m33 * s3) * invDet)
                    ((negate m30 * s5 + m32 * s2 - m33 * s1) * invDet)
                    ((m30 * s4 - m31 * s2 + m33 * s0) * invDet)
                    ((negate m30 * s3 + m31 * s1 - m32 * s0) * invDet)
                )
                ( V4
                    ((negate m21 * s5 + m22 * s4 - m23 * s3) * invDet)
                    ((m20 * s5 - m22 * s2 + m23 * s1) * invDet)
                    ((negate m20 * s4 + m21 * s2 - m23 * s0) * invDet)
                    ((m20 * s3 - m21 * s1 + m22 * s0) * invDet)
                )

-- | Build a model matrix from rotation and translation.
-- Equivalent to @translation t * rotation q@.
mkTransformM44 :: Quaternion -> V3 -> M44
mkTransformM44 q (V3 tx ty tz) =
  let M44 (V4 r00 r10 r20 _) (V4 r01 r11 r21 _) (V4 r02 r12 r22 _) _ = rotation q
   in M44
        (V4 r00 r10 r20 0)
        (V4 r01 r11 r21 0)
        (V4 r02 r12 r22 0)
        (V4 tx ty tz 1)
