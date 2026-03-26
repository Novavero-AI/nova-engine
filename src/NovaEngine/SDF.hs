-- | Signed distance field primitives and combinators.
--
-- Primitive SDFs, CSG boolean operations, smooth blending, and
-- domain operations (repetition, twist, bend, taper).
module NovaEngine.SDF
  ( -- * SDF type
    SDF (..),

    -- * Primitive SDFs
    sdfSphere,
    sdfBox,
    sdfCylinder,
    sdfTorus,
    sdfCapsule,
    sdfPlane,

    -- * CSG Boolean operations
    sdfUnion,
    sdfIntersection,
    sdfDifference,

    -- * Smooth blending
    smoothUnion,
    smoothIntersection,
    smoothDifference,

    -- * Domain operations
    sdfTranslate,
    sdfRotate,
    sdfScale,
    sdfRepetition,
    sdfTwist,
    sdfBend,
    sdfTaper,
    sdfElongate,

    -- * Shape modifiers
    sdfRound,
    sdfShell,

    -- * Normals
    sdfNormal,
    sdfGradient,

    -- * Edge helpers
    interpolateEdge,

    -- * Triplanar projection
    triplanarUV,
    triplanarTangent,

    -- * Sign
    signOf,
  )
where

import NovaEngine.Mesh.Types
  ( Quaternion,
    V2 (..),
    V3 (..),
    V4 (..),
    VecSpace (..),
    clampF,
    cross,
    dot,
    inverseQuat,
    lerp,
    normalize,
    rotateV3,
    vlength,
    vlerp,
  )

-- ----------------------------------------------------------------
-- SDF type
-- ----------------------------------------------------------------

-- | A signed distance function maps a point in 3D space to the
-- signed distance to the nearest surface. Negative values are
-- inside the shape, positive values are outside, and zero is on
-- the surface boundary.
newtype SDF = SDF {runSDF :: V3 -> Float}

-- ----------------------------------------------------------------
-- Primitive SDFs
-- ----------------------------------------------------------------

-- | Sphere centered at the origin.
--
-- @f(p) = ||p|| - r@
sdfSphere :: Float -> SDF
sdfSphere radius = SDF $ \p ->
  vlength p - radius

-- | Axis-aligned box centered at the origin with the given
-- half-extents.
--
-- @q = (|px| - bx, |py| - by, |pz| - bz)@
-- @f = ||max(q, 0)|| + min(max(qx, qy, qz), 0)@
sdfBox :: V3 -> SDF
sdfBox (V3 bx by bz) = SDF $ \(V3 px py pz) ->
  let qx = abs px - bx
      qy = abs py - by
      qz = abs pz - bz
      outsideDist = vlength (V3 (max qx 0) (max qy 0) (max qz 0))
      insideDist = min (max qx (max qy qz)) 0
   in outsideDist + insideDist

-- | Cylinder along the Y axis, centered at the origin.
--
-- @d = (sqrt(px^2 + pz^2) - r, |py| - h)@
-- @f = ||max(d, 0)|| + min(max(dx, dy), 0)@
sdfCylinder :: Float -> Float -> SDF
sdfCylinder radius halfHeight = SDF $ \(V3 px py pz) ->
  let dx = sqrt (px * px + pz * pz) - radius
      dy = abs py - halfHeight
      outsideDist = vlength2D (max dx 0) (max dy 0)
      insideDist = min (max dx dy) 0
   in outsideDist + insideDist

-- | Torus centered at the origin, lying in the XZ plane.
--
-- @f = ||(sqrt(px^2 + pz^2) - R, py)|| - r@
sdfTorus :: Float -> Float -> SDF
sdfTorus majorRadius minorRadius = SDF $ \(V3 px py pz) ->
  let ringDist = sqrt (px * px + pz * pz) - majorRadius
      tubeDist = vlength2D ringDist py
   in tubeDist - minorRadius

-- | Capsule (line segment with radius) between two endpoints.
--
-- Projects point onto the line segment, then measures distance
-- to the projection minus the radius.
sdfCapsule :: V3 -> V3 -> Float -> SDF
sdfCapsule pointA pointB radius = SDF $ \p ->
  let ab = pointB ^-^ pointA
      ap = p ^-^ pointA
      segmentLengthSq = dot ab ab
      t = clampUnit (dot ap ab / max segmentLengthSq nearZeroThreshold)
      projection = pointA ^+^ t *^ ab
   in vlength (p ^-^ projection) - radius

-- | Infinite plane defined by a unit normal and signed offset
-- from the origin.
--
-- @f = dot(p, n) - d@
sdfPlane :: V3 -> Float -> SDF
sdfPlane normal offset = SDF $ \p ->
  dot p normal - offset

-- ----------------------------------------------------------------
-- CSG Boolean operations
-- ----------------------------------------------------------------

-- | Union of two SDFs. The resulting surface is the outer
-- boundary of both shapes combined.
sdfUnion :: SDF -> SDF -> SDF
sdfUnion (SDF fa) (SDF fb) = SDF $ \p ->
  min (fa p) (fb p)

-- | Intersection of two SDFs. The resulting surface is the
-- region inside both shapes.
sdfIntersection :: SDF -> SDF -> SDF
sdfIntersection (SDF fa) (SDF fb) = SDF $ \p ->
  max (fa p) (fb p)

-- | Difference of two SDFs. Subtracts the second shape from
-- the first.
sdfDifference :: SDF -> SDF -> SDF
sdfDifference (SDF fa) (SDF fb) = SDF $ \p ->
  max (fa p) (negate (fb p))

-- ----------------------------------------------------------------
-- Smooth blending
-- ----------------------------------------------------------------

-- | Smooth union using polynomial smooth minimum.
--
-- Blends the two surfaces together with a smooth transition
-- region of width @k@.
smoothUnion :: Float -> SDF -> SDF -> SDF
smoothUnion k (SDF fa) (SDF fb) = SDF $ \p ->
  let distA = fa p
      distB = fb p
      h = clampUnit (blendHalfPlusHalf + blendScale * (distB - distA) / max k nearZeroThreshold)
   in lerp h distB distA - k * h * (1 - h)

-- | Smooth intersection using polynomial smooth maximum.
--
-- Rounds the intersection edge with a blend radius of @k@.
smoothIntersection :: Float -> SDF -> SDF -> SDF
smoothIntersection k (SDF fa) (SDF fb) = SDF $ \p ->
  let distA = fa p
      distB = fb p
      h = clampUnit (blendHalfPlusHalf - blendScale * (distB - distA) / max k nearZeroThreshold)
   in lerp h distB distA + k * h * (1 - h)

-- | Smooth difference using polynomial smooth maximum.
--
-- Rounds the subtraction edge with a blend radius of @k@.
smoothDifference :: Float -> SDF -> SDF -> SDF
smoothDifference k (SDF fa) (SDF fb) = SDF $ \p ->
  let distA = fa p
      distB = fb p
      h = clampUnit (blendHalfPlusHalf + blendScale * (distB + distA) / max k nearZeroThreshold)
   in lerp h distA (negate distB) + k * h * (1 - h)

-- ----------------------------------------------------------------
-- Domain operations
-- ----------------------------------------------------------------

-- | Translate (shift) an SDF by a displacement vector.
--
-- The input point is shifted by the negation of the displacement
-- before evaluating the SDF.
sdfTranslate :: V3 -> SDF -> SDF
sdfTranslate offset (SDF field) = SDF $ \p ->
  field (p ^-^ offset)

-- | Rotate an SDF by a quaternion.
--
-- The input point is rotated by the inverse of the quaternion
-- before evaluating the SDF, so the shape appears rotated by
-- the quaternion in world space.
sdfRotate :: Quaternion -> SDF -> SDF
sdfRotate q (SDF field) = SDF $ \p ->
  field (rotateV3 (inverseQuat q) p)

-- | Uniformly scale an SDF.
--
-- The input point is divided by the scale factor, and the result
-- is multiplied back. This preserves the Euclidean distance
-- property of the SDF.
sdfScale :: Float -> SDF -> SDF
sdfScale factor (SDF field) = SDF $ \(V3 px py pz) ->
  let invFactor = 1.0 / max factor nearZeroThreshold
   in field (V3 (px * invFactor) (py * invFactor) (pz * invFactor)) * factor

-- | Round the edges of an SDF by a given radius.
--
-- Subtracts the radius from the distance, effectively expanding
-- the shape and rounding all edges and corners.
--
-- @sdfRound r sdf@ is equivalent to @\\p -> runSDF sdf p - r@.
sdfRound :: Float -> SDF -> SDF
sdfRound radius (SDF field) = SDF $ \p ->
  field p - radius

-- | Create a hollow shell from an SDF.
--
-- The shell has the given wall thickness, centered on the
-- original surface. Points within @thickness\/2@ of the original
-- surface are inside the shell.
--
-- @sdfShell t sdf@ is equivalent to @\\p -> abs (runSDF sdf p) - t\/2@.
sdfShell :: Float -> SDF -> SDF
sdfShell thickness (SDF field) = SDF $ \p ->
  abs (field p) - thickness * shellHalfFactor

-- | Infinite repetition of an SDF with the given period along
-- each axis.
--
-- @p' = mod(p + period/2, period) - period/2@
sdfRepetition :: V3 -> SDF -> SDF
sdfRepetition (V3 periodX periodY periodZ) (SDF field) = SDF $ \(V3 px py pz) ->
  let rx = modRepeat px periodX
      ry = modRepeat py periodY
      rz = modRepeat pz periodZ
   in field (V3 rx ry rz)

-- | Twist an SDF around the Y axis. The twist angle is
-- proportional to the Y coordinate: @angle = strength * py@.
sdfTwist :: Float -> SDF -> SDF
sdfTwist strength (SDF field) = SDF $ \(V3 px py pz) ->
  let angle = strength * py
      cosA = cos angle
      sinA = sin angle
      twistedX = px * cosA - pz * sinA
      twistedZ = px * sinA + pz * cosA
   in field (V3 twistedX py twistedZ)

-- | Bend an SDF by rotating the XY plane proportional to X.
-- The bend angle at each point is @strength * px@, rotating
-- in the XY plane around the Z axis.
sdfBend :: Float -> SDF -> SDF
sdfBend strength (SDF field) = SDF $ \(V3 px py pz) ->
  let angle = strength * px
      cosA = cos angle
      sinA = sin angle
      bentX = cosA * px - sinA * py
      bentY = sinA * px + cosA * py
   in field (V3 bentX bentY pz)

-- | Taper an SDF by scaling the XZ plane as a function of Y.
--
-- The scale factor linearly interpolates from 1 at Y=0 to
-- @endScale@ at Y=@height@. Below Y=0 and above Y=height
-- the scale is clamped.
sdfTaper :: Float -> Float -> SDF -> SDF
sdfTaper height endScale (SDF field) = SDF $ \(V3 px py pz) ->
  let t = clampUnit (py / max height nearZeroThreshold)
      scale = lerp t 1.0 endScale
      invScale = 1.0 / max scale nearZeroThreshold
   in field (V3 (px * invScale) py (pz * invScale)) * scale

-- | Elongate an SDF by clamping coordinates to the given
-- half-extents. Stretches the zero-isosurface region.
sdfElongate :: V3 -> SDF -> SDF
sdfElongate (V3 ex ey ez) (SDF field) = SDF $ \(V3 px py pz) ->
  let qx = px - clampF (negate ex) ex px
      qy = py - clampF (negate ey) ey py
      qz = pz - clampF (negate ez) ez pz
   in field (V3 qx qy qz)

-- ----------------------------------------------------------------
-- Normals
-- ----------------------------------------------------------------

-- | Estimate the surface normal at a point using central
-- differences with six SDF evaluations.
sdfNormal :: SDF -> V3 -> V3
sdfNormal (SDF field) (V3 px py pz) =
  let nx = field (V3 (px + normalEpsilon) py pz) - field (V3 (px - normalEpsilon) py pz)
      ny = field (V3 px (py + normalEpsilon) pz) - field (V3 px (py - normalEpsilon) pz)
      nz = field (V3 px py (pz + normalEpsilon)) - field (V3 px py (pz - normalEpsilon))
   in normalize (V3 nx ny nz)

-- | Estimate the SDF gradient at a point using central differences.
-- Like 'sdfNormal' but takes a raw @V3 -> Float@ function instead
-- of the 'SDF' newtype. The result is normalized to a unit normal.
sdfGradient :: (V3 -> Float) -> V3 -> V3
sdfGradient sdf (V3 px py pz) =
  let eps = normalEpsilon
      dx = sdf (V3 (px + eps) py pz) - sdf (V3 (px - eps) py pz)
      dy = sdf (V3 px (py + eps) pz) - sdf (V3 px (py - eps) pz)
      dz = sdf (V3 px py (pz + eps)) - sdf (V3 px py (pz - eps))
   in normalize (V3 dx dy dz)

-- ----------------------------------------------------------------
-- Edge helpers
-- ----------------------------------------------------------------

-- | Linearly interpolate along an edge to find the zero crossing.
interpolateEdge :: V3 -> V3 -> Float -> Float -> V3
interpolateEdge posA posB valA valB
  | abs (valA - valB) < nearZeroThreshold = posA
  | otherwise =
      let paramT = valA / (valA - valB)
       in vlerp paramT posA posB

-- ----------------------------------------------------------------
-- Triplanar UV and tangent generation
-- ----------------------------------------------------------------

-- | Default UV tiling factor.
triplanarTilingFactor :: Float
triplanarTilingFactor = 1.0

-- | Compute UV coordinates using triplanar projection.
-- Selects the projection plane based on the dominant normal axis.
triplanarUV :: V3 -> V3 -> V2
triplanarUV (V3 px py pz) (V3 nx ny nz)
  | absNx > absNy && absNx > absNz = V2 (py * triplanarTilingFactor) (pz * triplanarTilingFactor)
  | absNy > absNz = V2 (px * triplanarTilingFactor) (pz * triplanarTilingFactor)
  | otherwise = V2 (px * triplanarTilingFactor) (py * triplanarTilingFactor)
  where
    absNx = abs nx
    absNy = abs ny
    absNz = abs nz

-- | Compute tangent vector using triplanar projection.
-- The tangent is the U-axis of the chosen projection plane.
-- The w component encodes bitangent handedness.
triplanarTangent :: V3 -> V4
triplanarTangent (V3 nx ny nz)
  | absNx > absNy && absNx > absNz =
      let tangentDir = V3 0 1 0
          biAxis = V3 0 0 1
          handedness = signOf (dot (cross nVec tangentDir) biAxis)
       in V4 0 1 0 handedness
  | absNy > absNz =
      let tangentDir = V3 1 0 0
          biAxis = V3 0 0 1
          handedness = signOf (dot (cross nVec tangentDir) biAxis)
       in V4 1 0 0 handedness
  | otherwise =
      let tangentDir = V3 1 0 0
          biAxis = V3 0 1 0
          handedness = signOf (dot (cross nVec tangentDir) biAxis)
       in V4 1 0 0 handedness
  where
    absNx = abs nx
    absNy = abs ny
    absNz = abs nz
    nVec = V3 nx ny nz

-- | Return +1 or -1 based on sign. Zero maps to +1.
signOf :: Float -> Float
signOf val
  | val < 0.0 = -1.0
  | otherwise = 1.0

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Epsilon used for central-difference normal estimation.
normalEpsilon :: Float
normalEpsilon = 0.001

-- | Threshold below which a denominator is considered zero.
nearZeroThreshold :: Float
nearZeroThreshold = 1.0e-10

-- | Constant for the half offset in smooth blend formulas.
blendHalfPlusHalf :: Float
blendHalfPlusHalf = 0.5

-- | Constant for the scale factor in smooth blend formulas.
blendScale :: Float
blendScale = 0.5

-- | Half factor for shell thickness (0.5).
shellHalfFactor :: Float
shellHalfFactor = 0.5

-- | Clamp a value to the unit interval @[0, 1]@.
clampUnit :: Float -> Float
clampUnit x = max 0 (min 1 x)

-- | 2D vector length from two components.
vlength2D :: Float -> Float -> Float
vlength2D x y = sqrt (x * x + y * y)

-- | Signed modular repetition. Maps a coordinate into the range
-- @[-period/2, period/2]@.
modRepeat :: Float -> Float -> Float
modRepeat x period
  | period <= 0 = x
  | otherwise =
      let halfPeriod = period * blendHalfPlusHalf
       in modF (x + halfPeriod) period - halfPeriod

-- | Floating-point modulo that always returns a non-negative
-- result (matching GLSL @mod@).
modF :: Float -> Float -> Float
modF x y = x - y * fromIntegral (floor (x / y) :: Int)
