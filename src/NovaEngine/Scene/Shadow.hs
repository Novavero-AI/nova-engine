-- | Cascaded shadow map computation.
--
-- Pure math for cascade splits, frustum extraction, and light-space
-- orthographic fitting with texel snapping. No IO, no FFI.
--
-- @
-- let cfg = defaultShadowConfig
--     cascades = computeCascades cfg viewMat projMat lightDir
-- @
module NovaEngine.Scene.Shadow
  ( -- * Configuration
    ShadowConfig (..),
    defaultShadowConfig,

    -- * Cascade computation
    CascadeData (..),
    computeCascades,

    -- * Internals (exported for testing)
    cascadeSplitDistances,
    frustumCornersWorld,
    fitLightOrtho,
  )
where

import NovaEngine.Math.Matrix
  ( inverse,
    lookAt,
    mulM44,
    mulM44V4,
    ortho,
  )
import NovaEngine.Math.Types
import NovaEngine.Math.Vector (cross, normalize)

-- ----------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------

-- | Shadow map configuration.
data ShadowConfig = ShadowConfig
  { shadowMapResolution :: !Int,
    shadowSplitLambda :: !Float
  }
  deriving (Show, Eq)

-- | Default config: 2048 resolution, lambda 0.5.
defaultShadowConfig :: ShadowConfig
defaultShadowConfig =
  ShadowConfig
    { shadowMapResolution = 2048,
      shadowSplitLambda = 0.5
    }

-- ----------------------------------------------------------------
-- Cascade data
-- ----------------------------------------------------------------

-- | Per-frame cascade output: 4 light-space VP matrices and the
-- view-space split far distances.
data CascadeData = CascadeData
  { cascadeMatrix0 :: !M44,
    cascadeMatrix1 :: !M44,
    cascadeMatrix2 :: !M44,
    cascadeMatrix3 :: !M44,
    cascadeSplits :: !V4
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Cascade splits (practical split scheme)
-- ----------------------------------------------------------------

-- | Compute 4 cascade split far distances using the practical
-- split scheme from GPU Gems 3.
--
-- Blends logarithmic and linear splits by @lambda@:
--
-- @z_i = lambda * n * (f\/n)^(i\/C) + (1 - lambda) * (n + (f - n) * i\/C)@
--
-- Returns the 4 far distances as a 'V4'.
cascadeSplitDistances ::
  -- | Near plane
  Float ->
  -- | Far plane
  Float ->
  -- | Lambda blend (0 = linear, 1 = logarithmic)
  Float ->
  V4
cascadeSplitDistances near far lambda =
  V4
    (splitAt' 1)
    (splitAt' 2)
    (splitAt' 3)
    (splitAt' cascadeCount)
  where
    cascadeCount = 4 :: Int

    splitAt' :: Int -> Float
    splitAt' i =
      let t = fromIntegral i / fromIntegral cascadeCount
          zLog = near * (far / near) ** t
          zLin = near + (far - near) * t
       in lambda * zLog + (1.0 - lambda) * zLin

-- ----------------------------------------------------------------
-- Frustum corner extraction
-- ----------------------------------------------------------------

-- | Extract the 8 world-space corners of a frustum slice between
-- two NDC depth values.
--
-- For Vulkan, NDC Z is [0, 1]. The 8 corners are the combinations
-- of X in {-1, 1}, Y in {-1, 1}, Z in {zNear, zFar}.
frustumCornersWorld ::
  -- | Inverse of (projection * view)
  M44 ->
  -- | NDC Z near (0.0 for full frustum near)
  Float ->
  -- | NDC Z far (1.0 for full frustum far)
  Float ->
  [V3]
frustumCornersWorld invVP zNear zFar =
  [ toV3 (mulM44V4 invVP (V4 x y z 1.0))
  | z <- [zNear, zFar],
    y <- [-1, 1],
    x <- [-1, 1]
  ]
  where
    toV3 (V4 px py pz pw) =
      let invW = 1.0 / pw
       in V3 (px * invW) (py * invW) (pz * invW)

-- | Compute the centroid of a list of 'V3' points.
centroid :: [V3] -> V3
centroid [] = V3 0 0 0
centroid pts =
  let count = length pts
      V3 sx sy sz = foldl1 (^+^) pts
      inv = 1.0 / fromIntegral count
   in V3 (sx * inv) (sy * inv) (sz * inv)

-- ----------------------------------------------------------------
-- Light-space ortho fitting
-- ----------------------------------------------------------------

-- | Fit a tight orthographic light-space VP matrix around the given
-- frustum corners, with texel snapping to prevent shadow shimmer.
fitLightOrtho ::
  -- | Light direction (toward light, normalised)
  V3 ->
  -- | Frustum corners for this cascade (world space)
  [V3] ->
  -- | Shadow map resolution (for texel snapping)
  Int ->
  M44
fitLightOrtho lightDir corners resolution =
  let -- Light view matrix: look from centroid along light direction
      center = centroid corners
      lightPos = center ^+^ lightDir
      up = pickUp lightDir
      lightView = lookAt lightPos center up

      -- Transform corners into light-view space
      cornersLight =
        [ let V4 lx ly lz lw = mulM44V4 lightView (V4 cx cy cz 1.0)
              invW = 1.0 / lw
           in V3 (lx * invW) (ly * invW) (lz * invW)
        | V3 cx cy cz <- corners
        ]

      -- Find AABB in light space
      (V3 minX minY minZ, V3 maxX maxY maxZ) = aabbFromPoints cornersLight

      -- Texel snapping: round min/max to shadow-map texel boundaries
      worldPerTexelX = (maxX - minX) / fromIntegral resolution
      worldPerTexelY = (maxY - minY) / fromIntegral resolution

      snappedMinX = snapFloor worldPerTexelX minX
      snappedMaxX = snapCeil worldPerTexelX maxX
      snappedMinY = snapFloor worldPerTexelY minY
      snappedMaxY = snapCeil worldPerTexelY maxY

      -- Build ortho projection (Vulkan conventions)
      lightProj =
        ortho
          snappedMinX
          snappedMaxX
          snappedMinY
          snappedMaxY
          minZ
          maxZ
   in lightProj `mulM44` lightView

-- | Pick an up vector that isn't parallel to the light direction.
pickUp :: V3 -> V3
pickUp dir@(V3 dx _ dz)
  | abs dx < 0.9 && abs dz < 0.9 = V3 0 0 1
  | otherwise = normalize (cross dir (V3 1 0 0))

-- | Compute the axis-aligned bounding box of a list of points.
aabbFromPoints :: [V3] -> (V3, V3)
aabbFromPoints [] = (V3 0 0 0, V3 0 0 0)
aabbFromPoints (V3 x0 y0 z0 : rest) =
  foldl'
    ( \(V3 mnx mny mnz, V3 mxx mxy mxz) (V3 px py pz) ->
        ( V3 (min mnx px) (min mny py) (min mnz pz),
          V3 (max mxx px) (max mxy py) (max mxz pz)
        )
    )
    (V3 x0 y0 z0, V3 x0 y0 z0)
    rest
  where
    foldl' _ acc [] = acc
    foldl' f !acc (h : t) = foldl' f (f acc h) t

-- | Snap a value down to the nearest multiple of a step.
snapFloor :: Float -> Float -> Float
snapFloor step val
  | step <= 0 = val
  | otherwise = fromIntegral (floor (val / step) :: Int) * step

-- | Snap a value up to the nearest multiple of a step.
snapCeil :: Float -> Float -> Float
snapCeil step val
  | step <= 0 = val
  | otherwise = fromIntegral (ceiling (val / step) :: Int) * step

-- ----------------------------------------------------------------
-- Main entry point
-- ----------------------------------------------------------------

-- | Compute cascade data for shadow mapping.
--
-- Given the camera matrices, light direction, and shadow config,
-- computes the 4 cascade light-space VP matrices and split distances.
computeCascades ::
  ShadowConfig ->
  -- | Camera view matrix
  M44 ->
  -- | Camera projection matrix
  M44 ->
  -- | Light direction (toward light, normalised)
  V3 ->
  -- | Camera near plane
  Float ->
  -- | Camera far plane
  Float ->
  CascadeData
computeCascades config viewMat projMat lightDir near far =
  let lambda = shadowSplitLambda config
      res = shadowMapResolution config
      splits@(V4 s0 s1 s2 s3) = cascadeSplitDistances near far lambda
      invVP = inverse (projMat `mulM44` viewMat)

      -- For each cascade, build a sub-frustum and fit the light ortho.
      -- Cascade 0: [near, s0]
      -- Cascade 1: [s0, s1]
      -- Cascade 2: [s1, s2]
      -- Cascade 3: [s2, s3]
      sliceNears = [near, s0, s1, s2]
      sliceFars = [s0, s1, s2, s3]

      buildCascade sliceNear sliceFar =
        let -- Convert view-space distances to NDC Z [0,1]
            -- For a perspective matrix: ndc_z = (far * near / (near - far)) / (-z) + far / (far - near)
            -- But it's simpler to build a sub-frustum VP and extract corners.
            -- Build a sub-perspective from near/far slice:
            ndcNear = depthToNDC projMat sliceNear
            ndcFar = depthToNDC projMat sliceFar
            corners = frustumCornersWorld invVP ndcNear ndcFar
         in fitLightOrtho lightDir corners res

      matrices = zipWith buildCascade sliceNears sliceFars
   in case matrices of
        [m0, m1, m2, m3] ->
          CascadeData m0 m1 m2 m3 splits
        _ ->
          CascadeData identity identity identity identity splits
  where
    identity =
      M44
        (V4 1 0 0 0)
        (V4 0 1 0 0)
        (V4 0 0 1 0)
        (V4 0 0 0 1)

-- | Convert a view-space distance to Vulkan NDC Z [0,1].
--
-- For a Vulkan perspective matrix with depth [0,1]:
-- NDC_z = (z + near) / (near - far)
-- where z = -distance (eye space z is negative for objects in front).
depthToNDC :: M44 -> Float -> Float
depthToNDC (M44 _ _ (V4 _ _ m22 _) (V4 _ _ m32 _)) viewDist =
  let z = negate viewDist
   in (m22 * z + m32) / negate z
