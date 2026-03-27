-- | Convex support functions for common primitive shapes.
--
-- Each shape provides a 'Support' function for use with GJK/EPA.
-- Pure Haskell, no external dependencies.
module NovaEngine.Physics.Shapes
  ( -- * Primitive support functions
    sphereSupport,
    boxSupport,
    capsuleSupport,
    hullSupport,
  )
where

import Data.List (maximumBy)
import Data.Ord (comparing)
import NovaEngine.Math.Types
import NovaEngine.Math.Vector (dot, normalize)
import NovaEngine.Physics.GJK (Support (..))

-- | Sphere centered at a position with a radius.
sphereSupport :: V3 -> Float -> Support
sphereSupport center radius = Support $ \dir ->
  let n = normalize dir
   in center ^+^ radius *^ n

-- | Axis-aligned box defined by center and half-extents.
boxSupport :: V3 -> V3 -> Support
boxSupport (V3 cx cy cz) (V3 hx hy hz) = Support $ \(V3 dx dy dz) ->
  V3
    (cx + if dx >= 0 then hx else negate hx)
    (cy + if dy >= 0 then hy else negate hy)
    (cz + if dz >= 0 then hz else negate hz)

-- | Capsule (line segment + radius) between two endpoints.
capsuleSupport :: V3 -> V3 -> Float -> Support
capsuleSupport pointA pointB radius = Support $ \dir ->
  let n = normalize dir
      projA = dot pointA dir
      projB = dot pointB dir
      base = if projA >= projB then pointA else pointB
   in base ^+^ radius *^ n

-- | Convex hull from a list of vertices.
-- Falls back to origin for empty lists.
hullSupport :: [V3] -> Support
hullSupport [] = Support $ \_ -> V3 0 0 0
hullSupport verts = Support $ \dir ->
  maximumBy (comparing (dot dir)) verts
