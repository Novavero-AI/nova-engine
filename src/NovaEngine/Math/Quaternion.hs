-- | Quaternion operations for rotations.
module NovaEngine.Math.Quaternion
  ( -- * Construction
    axisAngle,
    identityQuat,

    -- * Operations
    mulQuat,
    inverseQuat,
    rotateV3,

    -- * Interpolation
    slerpQuat,
  )
where

import NovaEngine.Math.Types

-- | Construct a quaternion from a unit axis and angle in radians.
axisAngle :: V3 -> Float -> Quaternion
axisAngle axis angle = Quaternion (cos halfAngle) (sin halfAngle *^ axis)
  where
    halfAngle = angle * 0.5

-- | Identity quaternion (no rotation).
identityQuat :: Quaternion
identityQuat = Quaternion 1 (V3 0 0 0)

-- | Rotate a 'V3' by a quaternion using the optimized Rodrigues
-- formula: @p + 2w(v × p) + 2(v × (v × p))@.
rotateV3 :: Quaternion -> V3 -> V3
rotateV3 (Quaternion w v) p =
  p ^+^ (2.0 * w) *^ vCrossP ^+^ 2.0 *^ cross v vCrossP
  where
    vCrossP = cross v p
    cross (V3 x1 y1 z1) (V3 x2 y2 z2) =
      V3
        (y1 * z2 - z1 * y2)
        (z1 * x2 - x1 * z2)
        (x1 * y2 - y1 * x2)

-- | Multiply two quaternions (Hamilton product).
-- @mulQuat a b@ applies rotation @b@ first, then @a@
-- (right-to-left, like matrix multiplication).
mulQuat :: Quaternion -> Quaternion -> Quaternion
mulQuat (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  Quaternion
    (w1 * w2 - x1 * x2 - y1 * y2 - z1 * z2)
    ( V3
        (w1 * x2 + x1 * w2 + y1 * z2 - z1 * y2)
        (w1 * y2 - x1 * z2 + y1 * w2 + z1 * x2)
        (w1 * z2 + x1 * y2 - y1 * x2 + z1 * w2)
    )

-- | Conjugate of a unit quaternion, which is also its inverse.
inverseQuat :: Quaternion -> Quaternion
inverseQuat (Quaternion w (V3 x y z)) =
  Quaternion w (V3 (negate x) (negate y) (negate z))

-- | Spherical linear interpolation between two quaternions.
-- Takes the shortest path (negates if dot product is negative).
slerpQuat :: Float -> Quaternion -> Quaternion -> Quaternion
slerpQuat t (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  let rawDot = w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2
      (cosTheta, w2a, x2a, y2a, z2a) =
        if rawDot < 0
          then (negate rawDot, negate w2, negate x2, negate y2, negate z2)
          else (rawDot, w2, x2, y2, z2)
   in if cosTheta > slerpThreshold
        then
          let w = w1 + t * (w2a - w1)
              x = x1 + t * (x2a - x1)
              y = y1 + t * (y2a - y1)
              z = z1 + t * (z2a - z1)
              invLen = 1.0 / sqrt (w * w + x * x + y * y + z * z)
           in Quaternion (w * invLen) (V3 (x * invLen) (y * invLen) (z * invLen))
        else
          let theta = acos (min 1.0 (max (-1.0) cosTheta))
              sinTheta = sin theta
              scaleA = sin ((1 - t) * theta) / sinTheta
              scaleB = sin (t * theta) / sinTheta
              w = scaleA * w1 + scaleB * w2a
              x = scaleA * x1 + scaleB * x2a
              y = scaleA * y1 + scaleB * y2a
              z = scaleA * z1 + scaleB * z2a
           in Quaternion w (V3 x y z)

slerpThreshold :: Float
slerpThreshold = 0.9995
