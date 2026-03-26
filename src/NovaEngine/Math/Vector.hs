-- | Vector operations for 'V2' and 'V3'.
module NovaEngine.Math.Vector
  ( -- * V2 operations
    dot2,
    vlength2,
    vlengthSq2,
    normalize2,
    vlerp2,

    -- * V3 operations
    dot,
    cross,
    vlength,
    vlengthSq,
    normalize,
    vlerp,
    distanceSq,

    -- * Safe operations
    safeNormalize,
    pickPerpendicular,

    -- * Generic interpolation
    lerp,
    lerpFloat,
    pairwiseLerp,

    -- * Constants
    nearZeroLength,
  )
where

import NovaEngine.Math.Types

-- | Dot product of two 'V2' vectors.
dot2 :: V2 -> V2 -> Float
dot2 (V2 x1 y1) (V2 x2 y2) = x1 * x2 + y1 * y2

-- | Length of a 'V2' vector.
vlength2 :: V2 -> Float
vlength2 v = sqrt (dot2 v v)

-- | Squared length of a 'V2' vector.
vlengthSq2 :: V2 -> Float
vlengthSq2 v = dot2 v v

-- | Normalize a 'V2' to unit length. Returns zero vector if input
-- length is below 'nearZeroLength'.
normalize2 :: V2 -> V2
normalize2 v
  | len < nearZeroLength = vzero
  | otherwise = (1.0 / len) *^ v
  where
    len = vlength2 v

-- | Linear interpolation between two 'V2' values.
vlerp2 :: Float -> V2 -> V2 -> V2
vlerp2 t from to = (1.0 - t) *^ from ^+^ t *^ to

-- | Dot product of two 'V3' vectors.
dot :: V3 -> V3 -> Float
dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Cross product of two 'V3' vectors.
cross :: V3 -> V3 -> V3
cross (V3 x1 y1 z1) (V3 x2 y2 z2) =
  V3
    (y1 * z2 - z1 * y2)
    (z1 * x2 - x1 * z2)
    (x1 * y2 - y1 * x2)

-- | Length of a 'V3' vector.
vlength :: V3 -> Float
vlength v = sqrt (dot v v)

-- | Squared length of a 'V3' vector. Avoids the sqrt in 'vlength',
-- useful for distance comparisons.
vlengthSq :: V3 -> Float
vlengthSq v = dot v v

-- | Normalize a 'V3' to unit length. Returns zero vector if input
-- length is below 'nearZeroLength'.
normalize :: V3 -> V3
normalize v
  | len < nearZeroLength = vzero
  | otherwise = (1.0 / len) *^ v
  where
    len = vlength v

-- | Linear interpolation between two 'V3' values.
vlerp :: Float -> V3 -> V3 -> V3
vlerp t from to = (1.0 - t) *^ from ^+^ t *^ to

-- | Squared Euclidean distance between two 'V3' vectors.
distanceSq :: V3 -> V3 -> Float
distanceSq a b = vlengthSq (a ^-^ b)

-- | Normalize with an explicit fallback vector for near-zero inputs.
safeNormalize :: V3 -> V3 -> V3
safeNormalize fallback v
  | len < nearZeroLength = fallback
  | otherwise = (1.0 / len) *^ v
  where
    len = vlength v

-- | Choose a vector perpendicular to the input. The input must be
-- normalized. Picks the cardinal axis least aligned with the input
-- to maximize numerical stability.
pickPerpendicular :: V3 -> V3
pickPerpendicular v@(V3 vx vy vz)
  | abs vx <= abs vy && abs vx <= abs vz = normalize (cross v (V3 1 0 0))
  | abs vy <= abs vz = normalize (cross v (V3 0 1 0))
  | otherwise = normalize (cross v (V3 0 0 1))

-- | Generic linear interpolation for any 'VecSpace'.
lerp :: (VecSpace a) => Float -> a -> a -> a
lerp t from to = (1.0 - t) *^ from ^+^ t *^ to

-- | Linear interpolation between two 'Float' values.
lerpFloat :: Float -> Float -> Float -> Float
lerpFloat t from to = from + t * (to - from)

-- | Pairwise lerp across adjacent list elements.
-- Used by De Casteljau and other subdivision algorithms.
pairwiseLerp :: (VecSpace a) => Float -> [a] -> [a]
pairwiseLerp t pts = zipWith (lerp t) pts (drop 1 pts)

-- | Threshold below which a vector is considered zero-length.
nearZeroLength :: Float
nearZeroLength = 1.0e-6
