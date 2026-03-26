-- | UV projection and transformation operations.
--
-- Planar, cylindrical, spherical, and box projections for generating
-- texture coordinates, plus scale, offset, and rotation transforms.
module NovaEngine.Mesh.UV
  ( -- * Projection planes
    ProjectionPlane (..),

    -- * UV projections
    projectPlanar,
    projectCylindrical,
    projectSpherical,
    projectBox,

    -- * UV transforms
    scaleUV,
    offsetUV,
    rotateUV,
  )
where

import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Two times pi.
twoPi :: Float
twoPi = 2.0 * pi

-- ----------------------------------------------------------------
-- Projection planes
-- ----------------------------------------------------------------

-- | Axis-aligned plane for planar UV projection.
data ProjectionPlane
  = -- | Project onto the XY plane (u = x, v = y).
    XYPlane
  | -- | Project onto the XZ plane (u = x, v = z).
    XZPlane
  | -- | Project onto the YZ plane (u = y, v = z).
    YZPlane
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- UV projections
-- ----------------------------------------------------------------

-- | Map world coordinates to UVs based on a chosen axis-aligned plane.
--
-- Multiplies the projected coordinates by the given scale factor.
projectPlanar ::
  -- | Projection plane
  ProjectionPlane ->
  -- | UV scale
  Float ->
  Mesh ->
  Mesh
projectPlanar plane scale (Mesh vertices indices count) =
  Mesh (map projectVertex vertices) indices count
  where
    projectVertex vtx =
      let V3 x y z = vPosition vtx
          uv = case plane of
            XYPlane -> V2 (x * scale) (y * scale)
            XZPlane -> V2 (x * scale) (z * scale)
            YZPlane -> V2 (y * scale) (z * scale)
       in vtx {vUV = uv}

-- | Cylindrical UV unwrap around the Y axis.
--
-- U is derived from the angle around Y (normalized to [0,1]),
-- V is the Y coordinate multiplied by the given scale factor.
projectCylindrical ::
  -- | V scale (controls vertical UV density)
  Float ->
  Mesh ->
  Mesh
projectCylindrical scale (Mesh vertices indices count) =
  Mesh (map projectVertex vertices) indices count
  where
    projectVertex vtx =
      let V3 x y z = vPosition vtx
          u = atan2 z x / twoPi + 0.5
          v = y * scale
       in vtx {vUV = V2 u v}

-- | Spherical latitude/longitude UV mapping from the origin.
--
-- For vertices at the origin (length below 'nearZeroLength'),
-- UVs default to (0.5, 0.5).
projectSpherical :: Mesh -> Mesh
projectSpherical (Mesh vertices indices count) =
  Mesh (map projectVertex vertices) indices count
  where
    projectVertex vtx =
      let pos@(V3 x y z) = vPosition vtx
          r = vlength pos
       in if r < nearZeroLength
            then vtx {vUV = V2 0.5 0.5}
            else
              let u = atan2 z x / twoPi + 0.5
                  v = asin (clampF (-1.0) 1.0 (y / r)) / pi + 0.5
               in vtx {vUV = V2 u v}

-- | Triplanar box projection.
--
-- For each vertex the dominant normal axis determines which
-- axis-aligned plane is used. The given scale factor is applied
-- to the projected coordinates.
projectBox ::
  -- | UV scale
  Float ->
  Mesh ->
  Mesh
projectBox scale (Mesh vertices indices count) =
  Mesh (map projectVertex vertices) indices count
  where
    projectVertex vtx =
      let V3 x y z = vPosition vtx
          V3 nx ny nz = vNormal vtx
          ax = abs nx
          ay = abs ny
          az = abs nz
          uv
            | ax >= ay && ax >= az = V2 (y * scale) (z * scale)
            | ay >= az = V2 (x * scale) (z * scale)
            | otherwise = V2 (x * scale) (y * scale)
       in vtx {vUV = uv}

-- ----------------------------------------------------------------
-- UV transforms
-- ----------------------------------------------------------------

-- | Scale UVs by independent factors along U and V.
scaleUV ::
  -- | U scale factor
  Float ->
  -- | V scale factor
  Float ->
  Mesh ->
  Mesh
scaleUV su sv (Mesh vertices indices count) =
  Mesh (map scaleVertex vertices) indices count
  where
    scaleVertex vtx =
      let V2 u v = vUV vtx
       in vtx {vUV = V2 (u * su) (v * sv)}

-- | Offset UVs by adding constant values to U and V.
offsetUV ::
  -- | U offset
  Float ->
  -- | V offset
  Float ->
  Mesh ->
  Mesh
offsetUV du dv (Mesh vertices indices count) =
  Mesh (map offsetVertex vertices) indices count
  where
    offsetVertex vtx =
      let V2 u v = vUV vtx
       in vtx {vUV = V2 (u + du) (v + dv)}

-- | Rotate UVs around the center point (0.5, 0.5) by an angle
-- in radians.
rotateUV ::
  -- | Rotation angle (radians)
  Float ->
  Mesh ->
  Mesh
rotateUV angle (Mesh vertices indices count) =
  Mesh (map rotateVertex vertices) indices count
  where
    cosA = cos angle
    sinA = sin angle

    rotateVertex vtx =
      let V2 u v = vUV vtx
          cu = u - uvCenter
          cv = v - uvCenter
          uRot = cosA * cu - sinA * cv + uvCenter
          vRot = sinA * cu + cosA * cv + uvCenter
       in vtx {vUV = V2 uRot vRot}

-- | Center of the UV coordinate space.
uvCenter :: Float
uvCenter = 0.5
