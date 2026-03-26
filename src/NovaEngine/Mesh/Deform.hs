-- | Mesh deformation operators.
--
-- Twist, bend, taper, free-form deformation (FFD), and
-- displacement mapping. All deformations are @Mesh -> Mesh@.
-- Normals are NOT recomputed — use 'NovaEngine.Mesh.Combine.recomputeNormals'
-- after deforming if needed.
module NovaEngine.Mesh.Deform
  ( -- * Twist
    twist,

    -- * Bend
    bend,

    -- * Taper
    taper,
    taperFn,

    -- * Free-Form Deformation
    FFDLattice (..),
    defaultLattice,
    ffd,

    -- * Displacement
    displace,
  )
where

import Data.List (foldl')
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Twist
-- ----------------------------------------------------------------

-- | Rotate each vertex around an axis by an angle proportional to
-- its projected position along that axis.
--
-- @axis@ must be normalized. @twistRate@ is radians per unit of
-- distance along the axis.
twist :: V3 -> Float -> Mesh -> Mesh
twist axis twistRate (Mesh vertices indices count) =
  Mesh (map twistVertex vertices) indices count
  where
    twistVertex vtx =
      let pos = vPosition vtx
          angle = twistRate * dot pos axis
          rotated = rotateV3 (axisAngle axis angle) pos
       in vtx {vPosition = rotated}

-- ----------------------------------------------------------------
-- Bend
-- ----------------------------------------------------------------

-- | Bend geometry around an axis (Barr 1984).
--
-- The axis is curved along a circular arc whose curvature is
-- @bendAmount@ (radians per unit of distance). A vertex at axial
-- distance @h@ is placed on an arc of radius @1 / bendAmount@ at
-- angle @h * bendAmount@, preserving its radial offset from the axis.
--
-- @axis@ must be normalized. Zero bend returns the mesh unchanged.
bend :: V3 -> Float -> Mesh -> Mesh
bend _ bendAmount mesh
  | abs bendAmount < nearZeroLength = mesh
bend axis bendAmount (Mesh vertices indices count) =
  Mesh (map bendVertex vertices) indices count
  where
    radialU = pickPerpendicular axis
    radialV = cross axis radialU
    bendRadius = 1.0 / bendAmount

    bendVertex vtx =
      let pos = vPosition vtx
          nrm = vNormal vtx
          axialCoord = dot pos axis
          planeComponent = pos ^-^ (axialCoord *^ axis)
          coordU = dot planeComponent radialU
          coordV = dot planeComponent radialV
          -- Barr bend: place vertex on a circular arc
          angle = axialCoord * bendAmount
          cosA = cos angle
          sinA = sin angle
          arcAxial = bendRadius * sinA
          arcRadial = bendRadius * (1.0 - cosA)
          bentPos =
            arcAxial
              *^ axis
              ^+^ (coordU + arcRadial)
              *^ radialU
              ^+^ coordV
              *^ radialV
          -- Rotate normal by the same angle in the axis-radialU plane
          nrmAxial = dot nrm axis
          nrmU = dot nrm radialU
          nrmV = dot nrm radialV
          bentNrm =
            normalize
              ( (nrmAxial * cosA - nrmU * sinA)
                  *^ axis
                  ^+^ (nrmAxial * sinA + nrmU * cosA)
                  *^ radialU
                  ^+^ nrmV
                  *^ radialV
              )
       in vtx {vPosition = bentPos, vNormal = bentNrm}

-- ----------------------------------------------------------------
-- Taper
-- ----------------------------------------------------------------

-- | Scale the cross-section of a mesh linearly along an axis.
--
-- @axis@ must be normalized. @startScale@ is the scale factor at
-- the minimum axial extent; @endScale@ at the maximum. Vertices
-- are interpolated between the two based on their normalized axial
-- position.
taper :: V3 -> Float -> Float -> Mesh -> Mesh
taper axis startScale endScale =
  taperFn axis (linearTaperFn startScale endScale)

-- | Scale the cross-section of a mesh along an axis using an
-- arbitrary function. The function receives the normalized axial
-- position in @[0, 1]@ and returns a scale factor.
--
-- @axis@ must be normalized.
taperFn :: V3 -> (Float -> Float) -> Mesh -> Mesh
taperFn axis scaleFn (Mesh vertices indices count) =
  Mesh (map taperVertex vertices) indices count
  where
    -- Find axial extent across all vertices
    axialPositions = map (\vtx -> dot (vPosition vtx) axis) vertices
    (axialMin, axialMax) = axialExtent axialPositions

    axialRange = axialMax - axialMin

    taperVertex vtx =
      let pos = vPosition vtx
          axialCoord = dot pos axis
          normalizedT =
            if abs axialRange < axialRangeEpsilon
              then 0.5
              else (axialCoord - axialMin) / axialRange
          scale = scaleFn normalizedT
          -- Decompose into axial and radial components
          axialComponent = axialCoord *^ axis
          radialComponent = pos ^-^ axialComponent
          -- Scale only the radial component
          scaledPos = axialComponent ^+^ scale *^ radialComponent
       in vtx {vPosition = scaledPos}

-- | Linear interpolation function for taper.
linearTaperFn :: Float -> Float -> Float -> Float
linearTaperFn startScale endScale t = startScale + t * (endScale - startScale)

-- | Find the minimum and maximum of a list of floats. Returns
-- @(0, 0)@ for an empty list.
axialExtent :: [Float] -> (Float, Float)
axialExtent [] = (0, 0)
axialExtent (first : rest) =
  foldl' (\(!lo, !hi) val -> (min lo val, max hi val)) (first, first) rest

-- | Threshold below which the axial range is considered zero,
-- preventing division by zero in normalization.
axialRangeEpsilon :: Float
axialRangeEpsilon = 1.0e-10

-- ----------------------------------------------------------------
-- Free-Form Deformation (FFD)
-- ----------------------------------------------------------------

-- | A Sederberg–Parry free-form deformation lattice.
--
-- The lattice is defined by an origin, three axis vectors spanning
-- the parallelepiped, dimensions @(l, m, n)@ giving the number of
-- control points along each axis (minimum 2), and a flat list of
-- control points stored in row-major order: @i@ varies fastest,
-- then @j@, then @k@.
data FFDLattice = FFDLattice
  { ffdOrigin :: !V3,
    ffdAxes :: !(V3, V3, V3),
    ffdDims :: !(Int, Int, Int),
    ffdControlPoints :: ![V3]
  }
  deriving (Show, Eq)

-- | Create an identity FFD lattice — control points are placed at
-- their rest positions so the deformation is initially a no-op.
--
-- The lattice spans from @origin@ to @origin + extent@ with the
-- given number of divisions along each axis (minimum clamped to 2).
defaultLattice :: V3 -> V3 -> Int -> Int -> Int -> FFDLattice
defaultLattice origin (V3 ex ey ez) dimI dimJ dimK =
  FFDLattice
    { ffdOrigin = origin,
      ffdAxes = (V3 ex 0 0, V3 0 ey 0, V3 0 0 ez),
      ffdDims = (safeDimI, safeDimJ, safeDimK),
      ffdControlPoints = controlPoints
    }
  where
    safeDimI = max minLatticeDim dimI
    safeDimJ = max minLatticeDim dimJ
    safeDimK = max minLatticeDim dimK

    controlPoints =
      [ origin
          ^+^ (fromIntegral ci / fromIntegral (safeDimI - 1))
          *^ V3 ex 0 0
          ^+^ (fromIntegral cj / fromIntegral (safeDimJ - 1))
          *^ V3 0 ey 0
          ^+^ (fromIntegral ck / fromIntegral (safeDimK - 1))
          *^ V3 0 0 ez
      | ck <- [0 .. safeDimK - 1],
        cj <- [0 .. safeDimJ - 1],
        ci <- [0 .. safeDimI - 1]
      ]

-- | Minimum number of control points along any lattice axis.
minLatticeDim :: Int
minLatticeDim = 2

-- | Apply free-form deformation to a mesh.
--
-- Each vertex position is mapped to local lattice coordinates
-- @(s, t, u)@, then evaluated via a trivariate Bernstein
-- polynomial over the lattice control points.
ffd :: FFDLattice -> Mesh -> Mesh
ffd lattice (Mesh vertices indices count) =
  Mesh (map deformVertex vertices) indices count
  where
    FFDLattice latticeOrigin (axisS, axisT, axisU) (dimS, dimT, dimU) cps = lattice

    -- Precompute cross products and inverse determinant for the
    -- lattice-to-parameter mapping via Cramer's rule. This handles
    -- arbitrary (non-orthogonal) parallelepiped axes correctly.
    crossTU = cross axisT axisU
    invDet = recipSafe (dot axisS crossTU)

    orderS = dimS - 1
    orderT = dimT - 1
    orderU = dimU - 1

    deformVertex vtx =
      let pos = vPosition vtx
          localPos = pos ^-^ latticeOrigin
          -- Compute parametric coordinates via Cramer's rule
          paramS = dot localPos crossTU * invDet
          paramT = dot axisS (cross localPos axisU) * invDet
          paramU = dot axisS (cross axisT localPos) * invDet
          -- Evaluate trivariate Bernstein polynomial
          deformedPos = evaluateBernstein paramS paramT paramU
       in vtx {vPosition = deformedPos}

    -- Evaluate the trivariate Bernstein polynomial:
    -- sum over i,j,k of B_i,n(s) * B_j,m(t) * B_k,l(u) * P_ijk
    evaluateBernstein paramS paramT paramU =
      let basisS = bernsteinBasis orderS paramS
          basisT = bernsteinBasis orderT paramT
          basisU = bernsteinBasis orderU paramU
       in foldl'
            ( \(!acc) ck ->
                let bU = indexWithDefault zeroBasis basisU ck
                 in foldl'
                      ( \(!innerAcc) cj ->
                          let bT = indexWithDefault zeroBasis basisT cj
                           in foldl'
                                ( \(!deepAcc) ci ->
                                    let bS = indexWithDefault zeroBasis basisS ci
                                        cpIdx = ck * (dimT * dimS) + cj * dimS + ci
                                        cp = indexWithDefault vzero cps cpIdx
                                        weight = bS * bT * bU
                                     in deepAcc ^+^ weight *^ cp
                                )
                                innerAcc
                                [0 .. orderS]
                      )
                      acc
                      [0 .. orderT]
            )
            vzero
            [0 .. orderU]

-- | Compute all Bernstein basis values @B_{i,n}(t)@ for
-- @i = 0 .. n@, returned as a list.
--
-- Uses direct evaluation:
-- @B_{i,n}(t) = C(n,i) * t^i * (1-t)^(n-i)@
bernsteinBasis :: Int -> Float -> [Float]
bernsteinBasis order t =
  [ bernsteinValue order idx t
  | idx <- [0 .. order]
  ]

-- | Evaluate a single Bernstein basis function.
-- @B_{i,n}(t) = C(n,i) * t^i * (1-t)^(n-i)@
bernsteinValue :: Int -> Int -> Float -> Float
bernsteinValue order idx t =
  fromIntegral (binomial order idx) * (t ^ idx) * ((1 - t) ^ (order - idx))

-- | Binomial coefficient @C(n, k)@ computed via multiplicative
-- formula. Returns 0 for invalid inputs.
binomial :: Int -> Int -> Int
binomial n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | otherwise =
      let safeK = min k (n - k)
       in foldl' (\(!acc) idx -> (acc * (n - safeK + idx)) `div` idx) 1 [1 .. safeK]

-- | Safe reciprocal that returns 0 for near-zero values.
recipSafe :: Float -> Float
recipSafe val
  | abs val < recipEpsilon = 0
  | otherwise = 1.0 / val

-- | Threshold for safe reciprocal.
recipEpsilon :: Float
recipEpsilon = 1.0e-10

-- | Zero-valued Bernstein basis default for out-of-bounds access.
zeroBasis :: Float
zeroBasis = 0

-- | Index into a list with a fallback for out-of-bounds access.
-- Used internally for FFD basis and control point lookup where
-- lists are always non-empty by construction.
indexWithDefault :: a -> [a] -> Int -> a
indexWithDefault fallback = go
  where
    go (y : _) 0 = y
    go (_ : ys) n = go ys (n - 1)
    go [] _ = fallback

-- ----------------------------------------------------------------
-- Displacement
-- ----------------------------------------------------------------

-- | Offset each vertex along its normal by a scalar function of
-- position.
--
-- @position' = position + f(position) * normal@
displace :: (V3 -> Float) -> Mesh -> Mesh
displace displaceFn (Mesh vertices indices count) =
  Mesh (map displaceVertex vertices) indices count
  where
    displaceVertex vtx =
      let pos = vPosition vtx
          normal = vNormal vtx
          offset = displaceFn pos
          displacedPos = pos ^+^ offset *^ normal
       in vtx {vPosition = displacedPos}
