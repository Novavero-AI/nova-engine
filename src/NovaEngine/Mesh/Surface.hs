-- | Parametric surface evaluation and tessellation.
--
-- Bezier patches, B-spline surfaces, and NURBS surfaces with
-- nested De Casteljau / De Boor evaluation and uniform tessellation
-- into indexed triangle meshes.
module NovaEngine.Mesh.Surface
  ( -- * Bezier patches
    BezierPatch (..),
    evalBezierPatch,
    tessellateBezierPatch,

    -- * B-spline surfaces
    BSplineSurface (..),
    evalBSplineSurface,
    tessellateBSplineSurface,

    -- * NURBS surfaces
    NURBSSurface (..),
    evalNURBSSurface,
    tessellateNURBSSurface,
  )
where

import Data.Array (Array, bounds, (!))
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Data types
-- ----------------------------------------------------------------

-- | A Bezier surface patch defined by a row-major grid of control
-- points. Rows run along v, columns along u.
data BezierPatch a = BezierPatch
  { -- | Number of rows in the control point grid
    patchRows :: !Int,
    -- | Number of columns in the control point grid
    patchCols :: !Int,
    -- | Control points stored in row-major order
    patchControlPoints :: ![a]
  }
  deriving (Show, Eq)

-- | A B-spline surface defined by degrees, knot vectors, and a
-- row-major grid of control points.
data BSplineSurface a = BSplineSurface
  { -- | Degree in the u direction
    bsurfDegreeU :: !Int,
    -- | Degree in the v direction
    bsurfDegreeV :: !Int,
    -- | Knot vector in u (Array for O(1) lookup)
    bsurfKnotsU :: !(Array Int Float),
    -- | Knot vector in v (Array for O(1) lookup)
    bsurfKnotsV :: !(Array Int Float),
    -- | Control points in row-major order (nRows x nCols)
    bsurfControlPoints :: ![a]
  }
  deriving (Show, Eq)

-- | A NURBS surface: a weighted B-spline surface.
data NURBSSurface a = NURBSSurface
  { -- | Underlying B-spline surface
    nsurfBSpline :: !(BSplineSurface a),
    -- | Weight for each control point (same length as control points)
    nsurfWeights :: ![Float]
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Minimum tessellation segments per direction.
minSegments :: Int
minSegments = 1

-- | Threshold below which a vector length is considered zero for
-- normal/tangent computation.
degenerateThreshold :: Float
degenerateThreshold = 1.0e-10

-- | Default up-vector used when the surface normal is degenerate.
defaultNormal :: V3
defaultNormal = V3 0 1 0

-- | Default tangent handedness when bitangent is degenerate.
defaultHandedness :: Float
defaultHandedness = 1.0

-- | Finite difference step for numerical partial derivatives on
-- B-spline and NURBS surfaces.
finiteDiffEpsilon :: Float
finiteDiffEpsilon = 1.0e-4

-- ----------------------------------------------------------------
-- Bezier patch evaluation
-- ----------------------------------------------------------------

-- | Evaluate a Bezier patch at parameters (u, v) using nested
-- De Casteljau interpolation. Applies De Casteljau in u across each
-- row, then in v across the resulting column.
evalBezierPatch :: (VecSpace a) => BezierPatch a -> Float -> Float -> a
evalBezierPatch patch u v =
  deCasteljau v columnPoints
  where
    rows = patchRows patch
    cols = patchCols patch
    points = patchControlPoints patch
    rowSlice rowIdx = take cols (drop (rowIdx * cols) points)
    columnPoints = [deCasteljau u (rowSlice rowIdx) | rowIdx <- [0 .. rows - 1]]

-- | De Casteljau algorithm for a list of control points at
-- parameter t. Returns 'vzero' if the list is empty.
deCasteljau :: (VecSpace a) => Float -> [a] -> a
deCasteljau _ [] = vzero
deCasteljau _ [pt] = pt
deCasteljau t pts = deCasteljau t (pairwiseLerp t pts)

-- ----------------------------------------------------------------
-- Bezier patch derivatives
-- ----------------------------------------------------------------

-- | Compute the partial derivative patch dS/du. The resulting patch
-- has (rows) rows and (cols - 1) columns, with control points
-- scaled by (cols - 1).
bezierDerivU :: (VecSpace a) => BezierPatch a -> BezierPatch a
bezierDerivU patch =
  BezierPatch rows (cols - 1) derivPoints
  where
    rows = patchRows patch
    cols = patchCols patch
    points = patchControlPoints patch
    degree = cols - 1
    scaleFactor = fromIntegral degree
    rowSlice rowIdx = take cols (drop (rowIdx * cols) points)
    rowDeriv row =
      zipWith
        (\pCurr pNext -> scaleFactor *^ (pNext ^-^ pCurr))
        row
        (drop 1 row)
    derivPoints = concatMap (rowDeriv . rowSlice) [0 .. rows - 1]

-- | Compute the partial derivative patch dS/dv. The resulting patch
-- has (rows - 1) rows and (cols) columns, with control points
-- scaled by (rows - 1).
bezierDerivV :: (VecSpace a) => BezierPatch a -> BezierPatch a
bezierDerivV patch =
  BezierPatch (rows - 1) cols derivPoints
  where
    rows = patchRows patch
    cols = patchCols patch
    points = patchControlPoints patch
    degree = rows - 1
    scaleFactor = fromIntegral degree
    rowSlice rowIdx = take cols (drop (rowIdx * cols) points)
    derivPoints =
      concat
        [ zipWith
            (\pCurr pNext -> scaleFactor *^ (pNext ^-^ pCurr))
            (rowSlice rowIdx)
            (rowSlice (rowIdx + 1))
        | rowIdx <- [0 .. rows - 2]
        ]

-- ----------------------------------------------------------------
-- Bezier patch tessellation
-- ----------------------------------------------------------------

-- | Tessellate a Bezier patch into a triangle mesh with uniform
-- sampling at the given segment counts in u and v.
tessellateBezierPatch :: BezierPatch V3 -> Int -> Int -> Mesh
tessellateBezierPatch patch segsURaw segsVRaw =
  mkMesh vertices indices
  where
    segsU = max minSegments segsURaw
    segsV = max minSegments segsVRaw

    derivPatchU = bezierDerivU patch
    derivPatchV = bezierDerivV patch

    vertices =
      [ buildBezierVertex patch derivPatchU derivPatchV u v
      | rowIdx <- [0 .. segsV],
        colIdx <- [0 .. segsU],
        let u = fromIntegral colIdx / fromIntegral segsU
            v = fromIntegral rowIdx / fromIntegral segsV
      ]

    indices = gridIndices segsU segsV

-- | Build a single vertex for a Bezier patch at given (u, v)
-- parameters, including position, normal, UV, and tangent.
buildBezierVertex ::
  BezierPatch V3 ->
  BezierPatch V3 ->
  BezierPatch V3 ->
  Float ->
  Float ->
  Vertex
buildBezierVertex patch derivPatchU derivPatchV u v =
  vertex position normal uv tangent
  where
    position = evalBezierPatch patch u v
    dSdu = evalBezierPatch derivPatchU u v
    dSdv = evalBezierPatch derivPatchV u v
    rawNormal = cross dSdu dSdv
    normal = safeNormalize defaultNormal rawNormal
    tangentDir = safeNormalize defaultNormal dSdu
    uv = V2 u v
    handedness = computeHandedness normal tangentDir dSdv
    V3 tx ty tz = tangentDir
    tangent = V4 tx ty tz handedness

-- ----------------------------------------------------------------
-- B-spline surface evaluation
-- ----------------------------------------------------------------

-- | Evaluate a B-spline surface at (u, v) using nested De Boor
-- evaluation. Returns 'Nothing' if the knot vectors or control
-- point grid are inconsistent, or if (u, v) is outside the valid
-- parameter domain.
evalBSplineSurface :: (VecSpace a) => BSplineSurface a -> Float -> Float -> Maybe a
evalBSplineSurface surf u v = do
  let degU = bsurfDegreeU surf
      degV = bsurfDegreeV surf
      knotsU = bsurfKnotsU surf
      knotsV = bsurfKnotsV surf
      points = bsurfControlPoints surf
      nColsU = arrayLen knotsU - degU - 1
      nRowsV = arrayLen knotsV - degV - 1
  -- Validate grid dimensions
  if nColsU <= 0 || nRowsV <= 0 || length points /= nRowsV * nColsU
    then Nothing
    else do
      let rowSlice rowIdx = take nColsU (drop (rowIdx * nColsU) points)
      -- Evaluate De Boor in u for each row
      columnResults <- traverse (deBoor degU knotsU u . rowSlice) [0 .. nRowsV - 1]
      -- Evaluate De Boor in v across the column
      deBoor degV knotsV v columnResults

-- | De Boor algorithm: evaluate a B-spline at parameter t given
-- degree, knot vector, and control points. Returns 'Nothing' if
-- the parameter is outside the valid knot span or the inputs are
-- inconsistent.
deBoor :: (VecSpace a) => Int -> Array Int Float -> Float -> [a] -> Maybe a
deBoor degree knots t points
  | numPoints <= 0 = Nothing
  | numPoints /= arrayLen knots - degree - 1 = Nothing
  | otherwise = do
      spanIdx <- findKnotSpan degree knots numPoints tClamped
      let relevant = take (degree + 1) (drop (spanIdx - degree) points)
      Just (deBoorRecurse degree degree spanIdx knots tClamped relevant)
  where
    numPoints = length points
    -- Clamp t to the valid parameter domain
    knotStart = knots `safeIndexOr` degree
    knotEnd = knots `safeIndexOr` (arrayLen knots - degree - 1)
    tClamped = clampF knotStart knotEnd t

-- | Recursive De Boor evaluation. At each level, linearly
-- interpolate adjacent control points using the appropriate knot
-- intervals until one point remains.
deBoorRecurse :: (VecSpace a) => Int -> Int -> Int -> Array Int Float -> Float -> [a] -> a
deBoorRecurse _ 0 _ _ _ pts = case pts of
  (pt : _) -> pt
  [] -> vzero
deBoorRecurse degree level spanIdx knots t pts =
  deBoorRecurse degree (level - 1) spanIdx knots t interpolated
  where
    startIdx = spanIdx - level + 1
    interpolated =
      [ let leftIdx = startIdx + idx
            rightIdx = leftIdx + level
            knotLeft = knots `safeIndexOr` leftIdx
            knotRight = knots `safeIndexOr` rightIdx
            denom = knotRight - knotLeft
            alpha =
              if abs denom < degenerateThreshold
                then 0
                else (t - knotLeft) / denom
         in lerp alpha ptCurr ptNext
      | (idx, ptCurr, ptNext) <- zip3 [0 ..] pts (drop 1 pts)
      ]

-- | Find the knot span index for parameter t using linear search.
-- Returns 'Nothing' if t is outside the valid domain.
findKnotSpan :: Int -> Array Int Float -> Int -> Float -> Maybe Int
findKnotSpan degree knots numPoints t
  | numPoints <= 0 = Nothing
  | t >= knotEnd = Just (numPoints - 1)
  | otherwise = Just (findSpanLinear degree knots t)
  where
    knotEnd = knots `safeIndexOr` numPoints

-- | Linear search for the knot span containing t.
findSpanLinear :: Int -> Array Int Float -> Float -> Int
findSpanLinear degree knots t =
  go degree
  where
    maxIdx = arrayLen knots - degree - 2
    go idx
      | idx >= maxIdx = maxIdx
      | knots `safeIndexOr` (idx + 1) > t = idx
      | otherwise = go (idx + 1)

-- ----------------------------------------------------------------
-- B-spline surface tessellation
-- ----------------------------------------------------------------

-- | Tessellate a B-spline surface into a triangle mesh. Returns
-- 'Nothing' if the surface parameters are invalid.
tessellateBSplineSurface :: BSplineSurface V3 -> Int -> Int -> Maybe Mesh
tessellateBSplineSurface surf segsURaw segsVRaw = do
  let segsU = max minSegments segsURaw
      segsV = max minSegments segsVRaw
      degU = bsurfDegreeU surf
      degV = bsurfDegreeV surf
      knotsU = bsurfKnotsU surf
      knotsV = bsurfKnotsV surf
      uMin = knotsU `safeIndexOr` degU
      uMax = knotsU `safeIndexOr` (arrayLen knotsU - degU - 1)
      vMin = knotsV `safeIndexOr` degV
      vMax = knotsV `safeIndexOr` (arrayLen knotsV - degV - 1)
  -- Validate by evaluating a single point
  _ <- evalBSplineSurface surf uMin vMin
  let vertexData =
        [ buildBSplineVertex surf u v uMin uMax vMin vMax
        | rowIdx <- [0 .. segsV],
          colIdx <- [0 .. segsU],
          let u = lerpFloat (fromIntegral colIdx / fromIntegral segsU) uMin uMax,
          let v = lerpFloat (fromIntegral rowIdx / fromIntegral segsV) vMin vMax
        ]
  vertices <- sequenceA vertexData
  Just (mkMesh vertices (gridIndices segsU segsV))

-- | Build a vertex for a B-spline surface at (u, v) using finite
-- differences for partial derivatives.
buildBSplineVertex ::
  BSplineSurface V3 ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Maybe Vertex
buildBSplineVertex surf u v uMin uMax vMin vMax = do
  position <- evalBSplineSurface surf u v
  let uPlus = min uMax (u + finiteDiffEpsilon)
      uMinus = max uMin (u - finiteDiffEpsilon)
      vPlus = min vMax (v + finiteDiffEpsilon)
      vMinus = max vMin (v - finiteDiffEpsilon)
  posUPlus <- evalBSplineSurface surf uPlus v
  posUMinus <- evalBSplineSurface surf uMinus v
  posVPlus <- evalBSplineSurface surf u vPlus
  posVMinus <- evalBSplineSurface surf u vMinus
  let duSpan = uPlus - uMinus
      dvSpan = vPlus - vMinus
      dSdu =
        if abs duSpan < degenerateThreshold
          then V3 1 0 0
          else (1.0 / duSpan) *^ (posUPlus ^-^ posUMinus)
      dSdv =
        if abs dvSpan < degenerateThreshold
          then V3 0 0 1
          else (1.0 / dvSpan) *^ (posVPlus ^-^ posVMinus)
      rawNormal = cross dSdu dSdv
      normal = safeNormalize defaultNormal rawNormal
      tangentDir = safeNormalize defaultNormal dSdu
      handedness = computeHandedness normal tangentDir dSdv
      uNorm = safeDivide (u - uMin) (uMax - uMin)
      vNorm = safeDivide (v - vMin) (vMax - vMin)
      V3 tx ty tz = tangentDir
  Just (vertex position normal (V2 uNorm vNorm) (V4 tx ty tz handedness))

-- ----------------------------------------------------------------
-- NURBS surface evaluation
-- ----------------------------------------------------------------

-- | Evaluate a NURBS surface at (u, v) by projecting weighted
-- control points into homogeneous space, evaluating as a B-spline,
-- then dividing by the weight. Returns 'Nothing' if the underlying
-- B-spline is invalid or if the homogeneous weight is near zero.
evalNURBSSurface :: NURBSSurface V3 -> Float -> Float -> Maybe V3
evalNURBSSurface nsurf u v = do
  let bsurf = nsurfBSpline nsurf
      weights = nsurfWeights nsurf
      points = bsurfControlPoints bsurf
      weightedPoints = zipWith toWeighted weights points
      homogSurf =
        bsurf {bsurfControlPoints = weightedPoints}
  V4 wx wy wz ww <- evalBSplineSurface homogSurf u v
  if abs ww < degenerateThreshold
    then Nothing
    else Just (V3 (wx / ww) (wy / ww) (wz / ww))
  where
    toWeighted w (V3 px py pz) = V4 (w * px) (w * py) (w * pz) w

-- | Tessellate a NURBS surface into a triangle mesh. Returns
-- 'Nothing' if the surface parameters are invalid.
tessellateNURBSSurface :: NURBSSurface V3 -> Int -> Int -> Maybe Mesh
tessellateNURBSSurface nsurf segsURaw segsVRaw = do
  let segsU = max minSegments segsURaw
      segsV = max minSegments segsVRaw
      bsurf = nsurfBSpline nsurf
      degU = bsurfDegreeU bsurf
      degV = bsurfDegreeV bsurf
      knotsU = bsurfKnotsU bsurf
      knotsV = bsurfKnotsV bsurf
      uMin = knotsU `safeIndexOr` degU
      uMax = knotsU `safeIndexOr` (arrayLen knotsU - degU - 1)
      vMin = knotsV `safeIndexOr` degV
      vMax = knotsV `safeIndexOr` (arrayLen knotsV - degV - 1)
  -- Validate by evaluating a single point
  _ <- evalNURBSSurface nsurf uMin vMin
  let vertexData =
        [ buildNURBSVertex nsurf u v uMin uMax vMin vMax
        | rowIdx <- [0 .. segsV],
          colIdx <- [0 .. segsU],
          let u = lerpFloat (fromIntegral colIdx / fromIntegral segsU) uMin uMax,
          let v = lerpFloat (fromIntegral rowIdx / fromIntegral segsV) vMin vMax
        ]
  vertices <- sequenceA vertexData
  Just (mkMesh vertices (gridIndices segsU segsV))

-- | Build a vertex for a NURBS surface at (u, v) using finite
-- differences for partial derivatives.
buildNURBSVertex ::
  NURBSSurface V3 ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Maybe Vertex
buildNURBSVertex nsurf u v uMin uMax vMin vMax = do
  position <- evalNURBSSurface nsurf u v
  let uPlus = min uMax (u + finiteDiffEpsilon)
      uMinus = max uMin (u - finiteDiffEpsilon)
      vPlus = min vMax (v + finiteDiffEpsilon)
      vMinus = max vMin (v - finiteDiffEpsilon)
  posUPlus <- evalNURBSSurface nsurf uPlus v
  posUMinus <- evalNURBSSurface nsurf uMinus v
  posVPlus <- evalNURBSSurface nsurf u vPlus
  posVMinus <- evalNURBSSurface nsurf u vMinus
  let duSpan = uPlus - uMinus
      dvSpan = vPlus - vMinus
      dSdu =
        if abs duSpan < degenerateThreshold
          then V3 1 0 0
          else (1.0 / duSpan) *^ (posUPlus ^-^ posUMinus)
      dSdv =
        if abs dvSpan < degenerateThreshold
          then V3 0 0 1
          else (1.0 / dvSpan) *^ (posVPlus ^-^ posVMinus)
      rawNormal = cross dSdu dSdv
      normal = safeNormalize defaultNormal rawNormal
      tangentDir = safeNormalize defaultNormal dSdu
      handedness = computeHandedness normal tangentDir dSdv
      uNorm = safeDivide (u - uMin) (uMax - uMin)
      vNorm = safeDivide (v - vMin) (vMax - vMin)
      V3 tx ty tz = tangentDir
  Just (vertex position normal (V2 uNorm vNorm) (V4 tx ty tz handedness))

-- ----------------------------------------------------------------
-- Shared tessellation helpers
-- ----------------------------------------------------------------

-- | Generate the index buffer for a uniform grid of (segsU + 1) by
-- (segsV + 1) vertices, producing two CCW triangles per quad cell.
gridIndices :: Int -> Int -> [Word32]
gridIndices segsU segsV =
  [ idx
  | rowIdx <- [0 .. segsV - 1],
    colIdx <- [0 .. segsU - 1],
    let rowWidth = fromIntegral (segsU + 1)
        a = fromIntegral rowIdx * rowWidth + fromIntegral colIdx
        b = a + 1
        c = a + rowWidth
        d = c + 1,
    idx <- [a, b, c, b, d, c]
  ]

-- | Compute tangent handedness from normal, tangent, and dS/dv.
computeHandedness :: V3 -> V3 -> V3 -> Float
computeHandedness normal tangentDir dSdv =
  if dot (cross normal tangentDir) dSdv >= 0
    then defaultHandedness
    else negate defaultHandedness

-- ----------------------------------------------------------------
-- Numeric helpers
-- ----------------------------------------------------------------

-- | Safe array indexing with a default of 0 for out-of-range indices.
safeIndexOr :: Array Int Float -> Int -> Float
safeIndexOr arr idx
  | idx < lo || idx > hi = 0
  | otherwise = arr ! idx
  where
    (lo, hi) = bounds arr

-- | Length of an array (number of elements).
arrayLen :: Array Int a -> Int
arrayLen arr = hi - lo + 1
  where
    (lo, hi) = bounds arr

-- | Safe division that returns 0 when the denominator is near zero.
safeDivide :: Float -> Float -> Float
safeDivide _ denom | abs denom < degenerateThreshold = 0
safeDivide numer denom = numer / denom
