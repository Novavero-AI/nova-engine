-- | Parametric 3D mesh primitives with analytical normals and UVs.
--
-- Sphere, capsule, cylinder, cone, torus, box, plane, and tapered
-- cylinder. Every shape takes segment\/ring count parameters for
-- tessellation control.
module NovaEngine.Mesh.Primitives
  ( -- * Primitives
    sphere,
    capsule,
    cylinder,
    cone,
    torus,
    box,
    plane,
    taperedCylinder,
  )
where

import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Two times pi.
twoPi :: Float
twoPi = 2.0 * pi

-- | Minimum tessellation segments around circumference.
minSlices :: Int
minSlices = 3

-- | Minimum tessellation stacks along height/axis.
minStacks :: Int
minStacks = 1

-- | Minimum stacks for a sphere (needs at least 2 for a body row between poles).
minSphereStacks :: Int
minSphereStacks = 2

-- ----------------------------------------------------------------
-- Sphere
-- ----------------------------------------------------------------

-- | Generate a UV sphere centered at the origin.
--
-- Returns 'Nothing' if the radius is not positive.
sphere ::
  -- | Radius
  Float ->
  -- | Slices (around equator, clamped to >= 3)
  Int ->
  -- | Stacks (pole to pole, clamped to >= 1)
  Int ->
  Maybe Mesh
sphere radius slicesRaw stacksRaw
  | radius <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    slices = max minSlices slicesRaw
    stacks = max minSphereStacks stacksRaw

    -- North pole: one vertex per triangle, u centered on each slice
    northPoleVerts =
      [ let u = (fromIntegral j + 0.5) / fromIntegral slices
         in vertex
              (V3 0 radius 0)
              (V3 0 1 0)
              (V2 u 0)
              (V4 1 0 0 1)
      | j <- [0 .. slices - 1]
      ]

    -- Body rows: stacks - 1 rows, each with slices + 1 vertices (seam)
    bodyVerts =
      [ let theta = fromIntegral i * pi / fromIntegral stacks
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinTheta = sin theta
            cosTheta = cos theta
            sinPhi = sin phi
            cosPhi = cos phi
            px = radius * sinTheta * cosPhi
            py = radius * cosTheta
            pz = radius * sinTheta * sinPhi
            nx = sinTheta * cosPhi
            ny = cosTheta
            nz = sinTheta * sinPhi
            u = fromIntegral j / fromIntegral slices
            v = fromIntegral i / fromIntegral stacks
            tx = -sinPhi
            tz = cosPhi
         in vertex
              (V3 px py pz)
              (V3 nx ny nz)
              (V2 u v)
              (V4 tx 0 tz 1)
      | i <- [1 .. stacks - 1],
        j <- [0 .. slices]
      ]

    -- South pole: one vertex per triangle, u centered on each slice
    southPoleVerts =
      [ let u = (fromIntegral j + 0.5) / fromIntegral slices
         in vertex
              (V3 0 (-radius) 0)
              (V3 0 (-1) 0)
              (V2 u 1)
              (V4 1 0 0 1)
      | j <- [0 .. slices - 1]
      ]

    vertices = northPoleVerts ++ bodyVerts ++ southPoleVerts

    -- North pole fan: connect each pole vertex to first body row
    northPoleIndices =
      [ idx
      | j <- [0 .. slices - 1],
        let poleIdx = fromIntegral j
            bodyBase = fromIntegral slices
            bodyJ = fromIntegral j
            bodyRow = bodyBase + bodyJ,
        idx <- [poleIdx, bodyRow + 1, bodyRow]
      ]

    -- Body quad bands
    bodyIndices =
      [ idx
      | i <- [0 .. stacks - 3],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            bodyBase = fromIntegral slices
            a = bodyBase + fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

    -- South pole fan: connect last body row to each pole vertex
    southPoleIndices =
      [ idx
      | j <- [0 .. slices - 1],
        let poleBase = fromIntegral slices + fromIntegral (stacks - 1) * fromIntegral (slices + 1)
            poleIdx = poleBase + fromIntegral j
            lastBodyRow = fromIntegral slices + fromIntegral (stacks - 2) * fromIntegral (slices + 1)
            bodyJ = lastBodyRow + fromIntegral j,
        idx <- [bodyJ + 1, bodyJ, poleIdx]
      ]

    indices = northPoleIndices ++ bodyIndices ++ southPoleIndices

-- ----------------------------------------------------------------
-- Capsule
-- ----------------------------------------------------------------

-- | Generate a capsule (two hemispheres + cylinder body) centered
-- at the origin, aligned along the Y axis.
--
-- Returns 'Nothing' if the radius or height is not positive.
capsule ::
  -- | Radius
  Float ->
  -- | Height (cylinder body, excluding hemispheres)
  Float ->
  -- | Slices (around circumference, clamped to >= 3)
  Int ->
  -- | Hemisphere rings (clamped to >= 1)
  Int ->
  -- | Body rings (clamped to >= 1)
  Int ->
  Maybe Mesh
capsule radius height slicesRaw hemiRingsRaw bodyRingsRaw
  | radius <= 0 = Nothing
  | height <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    slices = max minSlices slicesRaw
    hemiRings = max minStacks hemiRingsRaw
    bodyRings = max minStacks bodyRingsRaw

    halfHeight = height * 0.5
    totalBands = 2 * hemiRings + bodyRings - 1
    totalRings = totalBands + 1

    -- V parameterization: distribute across total arc length
    -- Top hemi arc = pi/2 * radius, body = height, bottom hemi = pi/2 * radius
    hemiArc = (pi / 2.0) * radius
    totalArcLength = 2.0 * hemiArc + height

    -- Ring index to (y, sinTheta, cosTheta, v).
    -- Only called for ringIdx in [1 .. totalRings - 1].
    ringData ringIdx
      -- Top hemisphere rings (1 .. hemiRings) including equator
      | ringIdx <= hemiRings =
          let theta = fromIntegral ringIdx * (pi / 2.0) / fromIntegral hemiRings
              sinTheta = sin theta
              cosTheta = cos theta
              arcSoFar = theta * radius
              v = arcSoFar / totalArcLength
           in (halfHeight + radius * cosTheta, sinTheta, cosTheta, v)
      -- Body rings
      | ringIdx <= hemiRings + bodyRings =
          let bodyIdx = ringIdx - hemiRings
              t = fromIntegral bodyIdx / fromIntegral bodyRings
              y = halfHeight - t * height
              arcSoFar = hemiArc + t * height
              v = arcSoFar / totalArcLength
           in (y, 1.0, 0.0, v)
      -- Bottom hemisphere rings
      | otherwise =
          let bottomIdx = ringIdx - hemiRings - bodyRings
              theta = (pi / 2.0) + fromIntegral bottomIdx * (pi / 2.0) / fromIntegral hemiRings
              sinTheta = sin theta
              cosTheta = cos theta
              arcSoFar = hemiArc + height + (theta - pi / 2.0) * radius
              v = arcSoFar / totalArcLength
           in (-halfHeight + radius * cosTheta, sinTheta, cosTheta, v)

    -- North pole vertices
    northPoleVerts =
      [ let u = (fromIntegral j + 0.5) / fromIntegral slices
         in vertex
              (V3 0 (halfHeight + radius) 0)
              (V3 0 1 0)
              (V2 u 0)
              (V4 1 0 0 1)
      | j <- [0 .. slices - 1]
      ]

    -- Ring vertices (all rings between poles)
    ringVerts =
      [ let (y, sinT, cosT, v) = ringData ringIdx
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinPhi = sin phi
            cosPhi = cos phi
            px = radius * sinT * cosPhi
            pz = radius * sinT * sinPhi
            nx = sinT * cosPhi
            ny = cosT
            nz = sinT * sinPhi
            u = fromIntegral j / fromIntegral slices
            tx = -sinPhi
            tz = cosPhi
         in vertex (V3 px y pz) (V3 nx ny nz) (V2 u v) (V4 tx 0 tz 1)
      | ringIdx <- [1 .. totalRings - 1],
        j <- [0 .. slices]
      ]

    -- South pole vertices
    southPoleVerts =
      [ let u = (fromIntegral j + 0.5) / fromIntegral slices
         in vertex
              (V3 0 (-halfHeight - radius) 0)
              (V3 0 (-1) 0)
              (V2 u 1)
              (V4 1 0 0 1)
      | j <- [0 .. slices - 1]
      ]

    vertices = northPoleVerts ++ ringVerts ++ southPoleVerts

    -- North pole fan
    northPoleIndices =
      [ idx
      | j <- [0 .. slices - 1],
        let poleIdx = fromIntegral j
            ringBase = fromIntegral slices
            rj = ringBase + fromIntegral j,
        idx <- [poleIdx, rj + 1, rj]
      ]

    -- Body quad bands between ring rows
    bodyBandIndices =
      [ idx
      | i <- [0 .. totalBands - 2],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            ringBase = fromIntegral slices
            a = ringBase + fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

    -- South pole fan
    southPoleIndices =
      [ idx
      | j <- [0 .. slices - 1],
        let poleBase = fromIntegral slices + fromIntegral (totalRings - 1) * fromIntegral (slices + 1)
            poleIdx = poleBase + fromIntegral j
            lastRingBase = fromIntegral slices + fromIntegral (totalRings - 2) * fromIntegral (slices + 1)
            rj = lastRingBase + fromIntegral j,
        idx <- [rj + 1, rj, poleIdx]
      ]

    indices = northPoleIndices ++ bodyBandIndices ++ southPoleIndices

-- ----------------------------------------------------------------
-- Cylinder
-- ----------------------------------------------------------------

-- | Generate a cylinder centered at the origin, aligned along Y.
--
-- Returns 'Nothing' if the radius or height is not positive.
cylinder ::
  -- | Radius
  Float ->
  -- | Height
  Float ->
  -- | Slices (around circumference, clamped to >= 3)
  Int ->
  -- | Height segments (clamped to >= 1)
  Int ->
  -- | Top cap
  Bool ->
  -- | Bottom cap
  Bool ->
  Maybe Mesh
cylinder radius height slicesRaw heightSegsRaw topCap bottomCap
  | radius <= 0 = Nothing
  | height <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    slices = max minSlices slicesRaw
    heightSegs = max minStacks heightSegsRaw
    halfHeight = height * 0.5

    -- Barrel vertices
    barrelVerts =
      [ let t = fromIntegral i / fromIntegral heightSegs
            y = halfHeight - t * height
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinPhi = sin phi
            cosPhi = cos phi
            px = radius * cosPhi
            pz = radius * sinPhi
            u = fromIntegral j / fromIntegral slices
            v = t
         in vertex
              (V3 px y pz)
              (V3 cosPhi 0 sinPhi)
              (V2 u v)
              (V4 (-sinPhi) 0 cosPhi 1)
      | i <- [0 .. heightSegs],
        j <- [0 .. slices]
      ]

    -- Barrel indices
    barrelIndices =
      [ idx
      | i <- [0 .. heightSegs - 1],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            a = fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

    barrelVertCount = (heightSegs + 1) * (slices + 1)

    -- Cap helper
    capVerts capY capNY =
      let centerVert =
            vertex
              (V3 0 capY 0)
              (V3 0 capNY 0)
              (V2 0.5 0.5)
              (V4 1 0 0 1)
          rimVerts =
            [ let phi = fromIntegral j * twoPi / fromIntegral slices
                  sinPhi = sin phi
                  cosPhi = cos phi
                  u = 0.5 + 0.5 * cosPhi
                  v = 0.5 + 0.5 * sinPhi
               in vertex
                    (V3 (radius * cosPhi) capY (radius * sinPhi))
                    (V3 0 capNY 0)
                    (V2 u v)
                    (V4 1 0 0 (if capNY > 0 then 1 else -1))
            | j <- [0 .. slices - 1]
            ]
       in centerVert : rimVerts

    capIndices capBase isTop =
      [ idx
      | j <- [0 .. slices - 1],
        let center = capBase
            rim0 = capBase + 1 + fromIntegral j
            rim1 = capBase + 1 + fromIntegral ((j + 1) `mod` slices),
        idx <-
          if isTop
            then [center, rim1, rim0]
            else [center, rim0, rim1]
      ]

    topCapVerts = if topCap then capVerts halfHeight 1.0 else []
    topCapIndices = if topCap then capIndices (fromIntegral barrelVertCount) True else []

    topCapCount = if topCap then slices + 1 else 0

    bottomCapVerts = if bottomCap then capVerts (-halfHeight) (-1.0) else []
    bottomCapIndices =
      if bottomCap
        then capIndices (fromIntegral (barrelVertCount + topCapCount)) False
        else []

    vertices = barrelVerts ++ topCapVerts ++ bottomCapVerts
    indices = barrelIndices ++ topCapIndices ++ bottomCapIndices

-- ----------------------------------------------------------------
-- Cone
-- ----------------------------------------------------------------

-- | Generate a cone with apex at top, base at bottom, aligned along Y.
--
-- Returns 'Nothing' if the radius or height is not positive.
cone ::
  -- | Base radius
  Float ->
  -- | Height
  Float ->
  -- | Slices (around circumference, clamped to >= 3)
  Int ->
  -- | Stacks (apex to base, clamped to >= 1)
  Int ->
  -- | Base cap
  Bool ->
  Maybe Mesh
cone radius height slicesRaw stacksRaw baseCap
  | radius <= 0 = Nothing
  | height <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    slices = max minSlices slicesRaw
    stacks = max minStacks stacksRaw
    halfHeight = height * 0.5
    slantLength = sqrt (height * height + radius * radius)
    nY = radius / slantLength
    nRadial = height / slantLength

    -- Apex vertices: one per triangle
    apexVerts =
      [ let phi = (fromIntegral j + 0.5) * twoPi / fromIntegral slices
            sinPhi = sin phi
            cosPhi = cos phi
            u = (fromIntegral j + 0.5) / fromIntegral slices
         in vertex
              (V3 0 halfHeight 0)
              (V3 (nRadial * cosPhi) nY (nRadial * sinPhi))
              (V2 u 0)
              (V4 (-sinPhi) 0 cosPhi 1)
      | j <- [0 .. slices - 1]
      ]

    -- Body rows: stacks - 1 interior rows plus base row
    bodyVerts =
      [ let t = fromIntegral i / fromIntegral stacks
            y = halfHeight - t * height
            r = t * radius
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinPhi = sin phi
            cosPhi = cos phi
            u = fromIntegral j / fromIntegral slices
            v = t
         in vertex
              (V3 (r * cosPhi) y (r * sinPhi))
              (V3 (nRadial * cosPhi) nY (nRadial * sinPhi))
              (V2 u v)
              (V4 (-sinPhi) 0 cosPhi 1)
      | i <- [1 .. stacks],
        j <- [0 .. slices]
      ]

    bodyVertCount = stacks * (slices + 1)

    -- Apex fan indices
    apexIndices =
      [ idx
      | j <- [0 .. slices - 1],
        let apexIdx = fromIntegral j
            bodyBase = fromIntegral slices
            bj = bodyBase + fromIntegral j,
        idx <- [apexIdx, bj + 1, bj]
      ]

    -- Body quad bands
    bodyBandIndices =
      [ idx
      | i <- [0 .. stacks - 2],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            bodyBase = fromIntegral slices
            a = bodyBase + fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

    -- Base cap
    capBase = fromIntegral (slices + bodyVertCount)
    baseCapVerts =
      if baseCap
        then
          let centerVert =
                vertex
                  (V3 0 (-halfHeight) 0)
                  (V3 0 (-1) 0)
                  (V2 0.5 0.5)
                  (V4 1 0 0 (-1))
              rimVerts =
                [ let phi = fromIntegral j * twoPi / fromIntegral slices
                      sinPhi = sin phi
                      cosPhi = cos phi
                      u = 0.5 + 0.5 * cosPhi
                      v = 0.5 + 0.5 * sinPhi
                   in vertex
                        (V3 (radius * cosPhi) (-halfHeight) (radius * sinPhi))
                        (V3 0 (-1) 0)
                        (V2 u v)
                        (V4 1 0 0 (-1))
                | j <- [0 .. slices - 1]
                ]
           in centerVert : rimVerts
        else []

    baseCapIndices =
      if baseCap
        then
          [ idx
          | j <- [0 .. slices - 1],
            let center = capBase
                rim0 = capBase + 1 + fromIntegral j
                rim1 = capBase + 1 + fromIntegral ((j + 1) `mod` slices),
            idx <- [center, rim0, rim1]
          ]
        else []

    vertices = apexVerts ++ bodyVerts ++ baseCapVerts
    indices = apexIndices ++ bodyBandIndices ++ baseCapIndices

-- ----------------------------------------------------------------
-- Torus
-- ----------------------------------------------------------------

-- | Generate a torus centered at the origin in the XZ plane.
--
-- Returns 'Nothing' if either radius is not positive, or if the
-- minor radius is greater than the major radius.
torus ::
  -- | Major radius (center to tube center)
  Float ->
  -- | Minor radius (tube radius)
  Float ->
  -- | Rings (around major axis, clamped to >= 3)
  Int ->
  -- | Slices (around tube, clamped to >= 3)
  Int ->
  Maybe Mesh
torus majorR minorR ringsRaw slicesRaw
  | majorR <= 0 = Nothing
  | minorR <= 0 = Nothing
  | minorR > majorR = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    rings = max minSlices ringsRaw
    slices = max minSlices slicesRaw

    vertices =
      [ let theta = fromIntegral i * twoPi / fromIntegral rings
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinTheta = sin theta
            cosTheta = cos theta
            sinPhi = sin phi
            cosPhi = cos phi
            px = (majorR + minorR * cosPhi) * cosTheta
            py = minorR * sinPhi
            pz = (majorR + minorR * cosPhi) * sinTheta
            nx = cosPhi * cosTheta
            ny = sinPhi
            nz = cosPhi * sinTheta
            u = fromIntegral i / fromIntegral rings
            v = fromIntegral j / fromIntegral slices
            -- Tangent: dS/dtheta direction (around major axis)
            tx = -sinTheta
            tz = cosTheta
         in vertex (V3 px py pz) (V3 nx ny nz) (V2 u v) (V4 tx 0 tz 1)
      | i <- [0 .. rings],
        j <- [0 .. slices]
      ]

    indices =
      [ idx
      | i <- [0 .. rings - 1],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            a = fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

-- ----------------------------------------------------------------
-- Box
-- ----------------------------------------------------------------

-- | Generate an axis-aligned box centered at the origin.
--
-- Returns 'Nothing' if any dimension is not positive.
box ::
  -- | Width (X extent)
  Float ->
  -- | Height (Y extent)
  Float ->
  -- | Depth (Z extent)
  Float ->
  -- | X segments (clamped to >= 1)
  Int ->
  -- | Y segments (clamped to >= 1)
  Int ->
  -- | Z segments (clamped to >= 1)
  Int ->
  Maybe Mesh
box width height depth segsXRaw segsYRaw segsZRaw
  | width <= 0 = Nothing
  | height <= 0 = Nothing
  | depth <= 0 = Nothing
  | otherwise = Just (mconcat faces)
  where
    segsX = max minStacks segsXRaw
    segsY = max minStacks segsYRaw
    segsZ = max minStacks segsZRaw
    hw = width * 0.5
    hh = height * 0.5
    hd = depth * 0.5

    -- Build one face as a subdivided grid
    buildFace normal tangent segsU segsV cornerFn =
      mkMesh faceVerts faceIndices
      where
        faceVerts =
          [ let u = fromIntegral ju / fromIntegral segsU
                v = fromIntegral jv / fromIntegral segsV
                pos = cornerFn u v
                V3 tx ty tz = tangent
             in vertex pos normal (V2 u v) (V4 tx ty tz 1)
          | jv <- [0 .. segsV],
            ju <- [0 .. segsU]
          ]

        faceIndices =
          [ idx
          | iv <- [0 .. segsV - 1],
            iu <- [0 .. segsU - 1],
            let rowWidth = fromIntegral (segsU + 1)
                a = fromIntegral iv * rowWidth + fromIntegral iu
                b = a + 1
                c = a + rowWidth
                d = c + 1,
            idx <- [a, b, c, b, d, c]
          ]

    faces =
      [ -- +Z face (front)
        buildFace (V3 0 0 1) (V3 1 0 0) segsX segsY $
          \u v -> V3 (lerp u (-hw) hw) (lerp v (-hh) hh) hd,
        -- -Z face (back)
        buildFace (V3 0 0 (-1)) (V3 (-1) 0 0) segsX segsY $
          \u v -> V3 (lerp u hw (-hw)) (lerp v (-hh) hh) (-hd),
        -- +X face (right)
        buildFace (V3 1 0 0) (V3 0 0 (-1)) segsZ segsY $
          \u v -> V3 hw (lerp v (-hh) hh) (lerp u hd (-hd)),
        -- -X face (left)
        buildFace (V3 (-1) 0 0) (V3 0 0 1) segsZ segsY $
          \u v -> V3 (-hw) (lerp v (-hh) hh) (lerp u (-hd) hd),
        -- +Y face (top)
        buildFace (V3 0 1 0) (V3 1 0 0) segsX segsZ $
          \u v -> V3 (lerp u (-hw) hw) hh (lerp v hd (-hd)),
        -- -Y face (bottom)
        buildFace (V3 0 (-1) 0) (V3 1 0 0) segsX segsZ $
          \u v -> V3 (lerp u (-hw) hw) (-hh) (lerp v (-hd) hd)
      ]

-- ----------------------------------------------------------------
-- Plane
-- ----------------------------------------------------------------

-- | Generate an XZ plane centered at the origin at Y = 0.
--
-- Returns 'Nothing' if either dimension is not positive.
plane ::
  -- | Width (X extent)
  Float ->
  -- | Depth (Z extent)
  Float ->
  -- | X segments (clamped to >= 1)
  Int ->
  -- | Z segments (clamped to >= 1)
  Int ->
  Maybe Mesh
plane width depth segsXRaw segsZRaw
  | width <= 0 = Nothing
  | depth <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    segsX = max minStacks segsXRaw
    segsZ = max minStacks segsZRaw
    hw = width * 0.5
    hd = depth * 0.5

    vertices =
      [ let u = fromIntegral ix / fromIntegral segsX
            v = fromIntegral iz / fromIntegral segsZ
            px = lerp u (-hw) hw
            pz = lerp v (-hd) hd
         in vertex (V3 px 0 pz) (V3 0 1 0) (V2 u v) (V4 1 0 0 1)
      | ix <- [0 .. segsX],
        iz <- [0 .. segsZ]
      ]

    indices =
      [ idx
      | ix <- [0 .. segsX - 1],
        iz <- [0 .. segsZ - 1],
        let rowWidth = fromIntegral (segsZ + 1)
            a = fromIntegral ix * rowWidth + fromIntegral iz
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

-- ----------------------------------------------------------------
-- Tapered Cylinder
-- ----------------------------------------------------------------

-- | Generate a tapered cylinder (different top and bottom radii)
-- centered at the origin, aligned along Y.
--
-- Returns 'Nothing' if the height is not positive or both radii
-- are not positive.
taperedCylinder ::
  -- | Top radius
  Float ->
  -- | Bottom radius
  Float ->
  -- | Height
  Float ->
  -- | Slices (around circumference, clamped to >= 3)
  Int ->
  -- | Height segments (clamped to >= 1)
  Int ->
  -- | Top cap
  Bool ->
  -- | Bottom cap
  Bool ->
  Maybe Mesh
taperedCylinder topR bottomR height slicesRaw heightSegsRaw topCap bottomCap
  | topR < 0 = Nothing
  | bottomR < 0 = Nothing
  | topR <= 0 && bottomR <= 0 = Nothing
  | height <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    slices = max minSlices slicesRaw
    heightSegs = max minStacks heightSegsRaw
    halfHeight = height * 0.5
    radiusDiff = bottomR - topR
    slantLength = sqrt (height * height + radiusDiff * radiusDiff)
    nY = if slantLength > 0 then radiusDiff / slantLength else 0
    nRadial = if slantLength > 0 then height / slantLength else 1

    -- Barrel vertices
    barrelVerts =
      [ let t = fromIntegral i / fromIntegral heightSegs
            y = halfHeight - t * height
            r = lerp t topR bottomR
            phi = fromIntegral j * twoPi / fromIntegral slices
            sinPhi = sin phi
            cosPhi = cos phi
            u = fromIntegral j / fromIntegral slices
            v = t
         in vertex
              (V3 (r * cosPhi) y (r * sinPhi))
              (V3 (nRadial * cosPhi) nY (nRadial * sinPhi))
              (V2 u v)
              (V4 (-sinPhi) 0 cosPhi 1)
      | i <- [0 .. heightSegs],
        j <- [0 .. slices]
      ]

    barrelIndices =
      [ idx
      | i <- [0 .. heightSegs - 1],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral (slices + 1)
            a = fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

    barrelVertCount = (heightSegs + 1) * (slices + 1)

    -- Cap helper
    capVerts capY capNY capR =
      if capR <= 0
        then []
        else
          let centerVert =
                vertex
                  (V3 0 capY 0)
                  (V3 0 capNY 0)
                  (V2 0.5 0.5)
                  (V4 1 0 0 (if capNY > 0 then 1 else -1))
              rimVerts =
                [ let phi = fromIntegral j * twoPi / fromIntegral slices
                      sinPhi = sin phi
                      cosPhi = cos phi
                      u = 0.5 + 0.5 * cosPhi
                      v = 0.5 + 0.5 * sinPhi
                   in vertex
                        (V3 (capR * cosPhi) capY (capR * sinPhi))
                        (V3 0 capNY 0)
                        (V2 u v)
                        (V4 1 0 0 (if capNY > 0 then 1 else -1))
                | j <- [0 .. slices - 1]
                ]
           in centerVert : rimVerts

    capIndicesFn capBase isTop capR =
      if capR <= 0
        then []
        else
          [ idx
          | j <- [0 .. slices - 1],
            let center = capBase
                rim0 = capBase + 1 + fromIntegral j
                rim1 = capBase + 1 + fromIntegral ((j + 1) `mod` slices),
            idx <-
              if isTop
                then [center, rim1, rim0]
                else [center, rim0, rim1]
          ]

    topCapVerts = if topCap then capVerts halfHeight 1.0 topR else []
    topCapIndices = if topCap then capIndicesFn (fromIntegral barrelVertCount) True topR else []
    topCapVertCount = if topCap && topR > 0 then slices + 1 else 0

    bottomCapVerts = if bottomCap then capVerts (-halfHeight) (-1.0) bottomR else []
    bottomCapIndices =
      if bottomCap
        then capIndicesFn (fromIntegral (barrelVertCount + topCapVertCount)) False bottomR
        else []

    vertices = barrelVerts ++ topCapVerts ++ bottomCapVerts
    indices = barrelIndices ++ topCapIndices ++ bottomCapIndices
