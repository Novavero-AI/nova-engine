-- | Parametric curve evaluation for Bezier, B-spline, and NURBS curves.
--
-- De Casteljau evaluation and splitting for Bezier curves, De Boor
-- evaluation for B-splines, basis-function NURBS evaluation, and
-- arc-length parameterization via Gaussian quadrature.
module NovaEngine.Mesh.Curve
  ( -- * Bezier curves
    BezierCurve (..),
    evalBezier,
    splitBezier,
    bezierDerivative,
    evalBezierDerivative,

    -- * B-spline curves
    BSplineCurve (..),
    knotArray,
    evalBSpline,
    bsplineDerivative,

    -- * NURBS curves
    NURBSCurve (..),
    evalNURBS,

    -- * Arc-length parameterization
    ArcLengthTable,
    buildArcLengthTable,
    arcLengthToParam,
    totalArcLength,
  )
where

import Data.Array (Array, bounds, listArray, (!))
import Data.List (foldl')
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Curve data types
-- ----------------------------------------------------------------

-- | A Bezier curve defined by its control points. The degree of the
-- curve is @length controlPoints - 1@.
newtype BezierCurve a = BezierCurve {bezierControlPoints :: [a]}
  deriving (Show, Eq)

-- | A B-spline curve defined by degree, knot vector, and control points.
-- The knot vector must have @length controlPoints + degree + 1@ entries.
-- Knots are stored as an 'Array' for O(1) lookup in the De Boor algorithm.
data BSplineCurve a = BSplineCurve
  { bsplineDegree :: !Int,
    bsplineKnots :: !(Array Int Float),
    bsplineControlPoints :: ![a]
  }
  deriving (Show, Eq)

-- | A NURBS curve: a B-spline with per-control-point weights.
-- The weights list must have the same length as the B-spline
-- control points.
data NURBSCurve a = NURBSCurve
  { nurbsBSpline :: !(BSplineCurve a),
    nurbsWeights :: ![Float]
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Bezier curves
-- ----------------------------------------------------------------

-- | Evaluate a Bezier curve at parameter @t@ using De Casteljau's
-- algorithm. Returns 'Nothing' if the curve has no control points.
--
-- @
-- P_i^[0] = P_i
-- P_i^[r] = (1 - t) * P_i^[r-1] + t * P_{i+1}^[r-1]
-- C(t) = P_0^[n]
-- @
evalBezier :: (VecSpace a) => BezierCurve a -> Float -> Maybe a
evalBezier (BezierCurve []) _ = Nothing
evalBezier (BezierCurve pts) t = deCasteljauReduce pts
  where
    deCasteljauReduce [single] = Just single
    deCasteljauReduce [] = Nothing
    deCasteljauReduce level = deCasteljauReduce (pairwiseLerp t level)

-- | Split a Bezier curve at parameter @t@ into two sub-curves.
-- The left sub-curve covers @[0, t]@ and the right covers @[t, 1]@.
-- Returns 'Nothing' if the curve has no control points.
--
-- The left sub-curve control points are the left edges of the
-- De Casteljau tableau; the right are the right edges (reversed).
splitBezier :: (VecSpace a) => BezierCurve a -> Float -> Maybe (BezierCurve a, BezierCurve a)
splitBezier (BezierCurve []) _ = Nothing
splitBezier (BezierCurve pts) t =
  case (traverse safeFirst tableau, traverse safeLast tableau) of
    (Just leftPts, Just rightPts) ->
      Just (BezierCurve leftPts, BezierCurve (reverse rightPts))
    _ -> Nothing
  where
    tableau = buildTableau t pts

-- | Compute the hodograph (derivative curve) of a Bezier curve.
-- If the curve has degree @n@, the derivative has degree @n - 1@
-- with control points @Q_i = n * (P_{i+1} - P_i)@.
-- Returns 'Nothing' if the curve has fewer than 2 control points.
bezierDerivative :: (VecSpace a) => BezierCurve a -> Maybe (BezierCurve a)
bezierDerivative (BezierCurve pts)
  | degree < 1 = Nothing
  | otherwise = Just (BezierCurve derivPts)
  where
    degree = length pts - 1
    scaleFactor = fromIntegral degree
    derivPts = zipWith (\pNext pCur -> scaleFactor *^ (pNext ^-^ pCur)) (drop 1 pts) pts

-- | Evaluate the derivative of a Bezier curve at parameter @t@.
-- Returns 'Nothing' if the curve has fewer than 2 control points.
evalBezierDerivative :: (VecSpace a) => BezierCurve a -> Float -> Maybe a
evalBezierDerivative curve t = do
  deriv <- bezierDerivative curve
  evalBezier deriv t

-- ----------------------------------------------------------------
-- Bezier internal helpers
-- ----------------------------------------------------------------

-- | Build the full De Casteljau tableau: a list of lists where each
-- successive list is one shorter than the previous.
buildTableau :: (VecSpace a) => Float -> [a] -> [[a]]
buildTableau _ [] = []
buildTableau _ [single] = [[single]]
buildTableau t level = level : buildTableau t (pairwiseLerp t level)

-- | Safely get the first element of a list.
safeFirst :: [a] -> Maybe a
safeFirst (x : _) = Just x
safeFirst [] = Nothing

-- ----------------------------------------------------------------
-- B-spline curves
-- ----------------------------------------------------------------

-- | Evaluate a B-spline curve at parameter @u@ using De Boor's algorithm.
-- Returns 'Nothing' if:
--
-- * The parameter is outside the valid knot range
-- * The curve definition is invalid (wrong knot count, etc.)
-- * The degree is negative
evalBSpline :: (VecSpace a) => BSplineCurve a -> Float -> Maybe a
evalBSpline (BSplineCurve deg knots pts) u
  | deg < 0 = Nothing
  | numPts < 1 = Nothing
  | numKnots /= numPts + deg + 1 = Nothing
  | u < knotLow || u > knotHigh = Nothing
  | otherwise = deBoor deg spanIdx relevantPts
  where
    numPts = length pts
    (kLo, kHi) = bounds knots
    numKnots = kHi - kLo + 1

    -- Valid parameter range: [u_{deg}, u_{numPts}]
    knotLow = indexKnot knots deg
    knotHigh = indexKnot knots numPts

    -- Find knot span: the index k such that u_k <= u < u_{k+1}.
    -- For u == knotHigh, we back up to the last non-empty span.
    spanIdx = findKnotSpan knots deg (numPts - 1) u

    -- Extract the p+1 relevant control points for the span.
    relevantPts = take (deg + 1) (drop (spanIdx - deg) pts)

    -- De Boor iteration: reduce p+1 points down to 1.
    deBoor 0 _ [single] = Just single
    deBoor _ _ [] = Nothing
    deBoor level spanK dPts
      | length dPts < level + 1 = Nothing
      | otherwise = deBoor (level - 1) spanK blended
      where
        blendLevel = deg - level + 1
        blended = zipWith3 blendPair [0 ..] dPts (drop 1 dPts)
        blendPair idx prev next =
          let knotIdx = spanK - deg + blendLevel + idx
              knotLeft = indexKnot knots knotIdx
              knotRight = indexKnot knots (knotIdx + level)
              denom = knotRight - knotLeft
              alpha =
                if abs denom < zeroKnotSpanThreshold
                  then 0.0
                  else (u - knotLeft) / denom
           in lerp alpha prev next

-- | Compute the derivative of a B-spline curve.
-- The derivative of a degree-@p@ B-spline is a degree-@(p-1)@ B-spline
-- with control points:
--
-- @Q_i = p * (P_{i+1} - P_i) / (u_{i+p+1} - u_{i+1})@
--
-- Returns 'Nothing' if the degree is less than 1 or the curve is
-- invalid.
bsplineDerivative :: (VecSpace a) => BSplineCurve a -> Maybe (BSplineCurve a)
bsplineDerivative (BSplineCurve deg knots pts)
  | deg < 1 = Nothing
  | numPts < 2 = Nothing
  | numKnots /= numPts + deg + 1 = Nothing
  | otherwise = Just (BSplineCurve derivDeg derivKnots derivPts)
  where
    numPts = length pts
    (kLo, kHi) = bounds knots
    numKnots = kHi - kLo + 1
    derivDeg = deg - 1
    scaleFactor = fromIntegral deg

    -- Interior knots for the derivative: drop the first and last
    knotList = [knots ! i | i <- [kLo .. kHi]]
    derivKnots = knotArray (drop 1 (take (numKnots - 1) knotList))

    derivPts =
      [ let knotRight = indexKnot knots (idx + deg + 1)
            knotLeft = indexKnot knots (idx + 1)
            denom = knotRight - knotLeft
            scale =
              if abs denom < zeroKnotSpanThreshold
                then 0.0
                else scaleFactor / denom
         in scale *^ (pNext ^-^ pCur)
      | (idx, pCur, pNext) <- zip3 [0 ..] pts (drop 1 pts)
      ]

-- ----------------------------------------------------------------
-- B-spline internal helpers
-- ----------------------------------------------------------------

-- | Threshold below which a knot span is considered zero-length.
-- Used for the 0/0 = 0 convention in B-spline evaluation.
zeroKnotSpanThreshold :: Float
zeroKnotSpanThreshold = 1.0e-10

-- | Safely index into a knot vector array. Returns 0 for out-of-range
-- indices (should not occur with valid input).
indexKnot :: Array Int Float -> Int -> Float
indexKnot knotArr idx
  | idx < lo || idx > hi = 0.0
  | otherwise = knotArr ! idx
  where
    (lo, hi) = bounds knotArr

-- | Convert a knot list to an array for O(1) lookup.
knotArray :: [Float] -> Array Int Float
knotArray [] = listArray (0, -1) []
knotArray knots = listArray (0, length knots - 1) knots

-- | Find the knot span index for parameter @u@. Returns @k@ such
-- that @u_k <= u < u_{k+1}@. For @u@ equal to the upper boundary,
-- backs up to the last non-degenerate span.
--
-- Uses a binary search over the valid range @[deg, numPts]@.
findKnotSpan :: Array Int Float -> Int -> Int -> Float -> Int
findKnotSpan knots deg lastIdx u
  -- Clamp to the last valid span when u is at the upper boundary
  | u >= indexKnot knots (lastIdx + 1) = clampToLastSpan lastIdx
  | otherwise = binarySearchSpan knots deg lastIdx u
  where
    clampToLastSpan idx
      | idx <= deg = deg
      | indexKnot knots idx < indexKnot knots (idx + 1) = idx
      | otherwise = clampToLastSpan (idx - 1)

-- | Binary search for the knot span containing @u@.
binarySearchSpan :: Array Int Float -> Int -> Int -> Float -> Int
binarySearchSpan knots low high u = go low high
  where
    go lo hi
      | hi - lo <= 1 = lo
      | u < indexKnot knots mid = go lo mid
      | otherwise = go mid hi
      where
        mid = (lo + hi) `div` 2

-- ----------------------------------------------------------------
-- NURBS curves
-- ----------------------------------------------------------------

-- | Evaluate a NURBS curve at parameter @u@ using basis function
-- summation with rational weighting.
--
-- @C(u) = sum(N_i(u) * w_i * P_i) / sum(N_i(u) * w_i)@
--
-- Returns 'Nothing' if the underlying B-spline is invalid, the
-- weights list has the wrong length, or the denominator is near zero.
evalNURBS :: (VecSpace a) => NURBSCurve a -> Float -> Maybe a
evalNURBS (NURBSCurve bspline weights) u
  | length weights /= numPts = Nothing
  | numPts < 1 = Nothing
  | deg < 0 = Nothing
  | numKnots /= numPts + deg + 1 = Nothing
  | u < knotLow || u > knotHigh = Nothing
  | abs denominator < zeroKnotSpanThreshold = Nothing
  | otherwise = Just ((1.0 / denominator) *^ numerator)
  where
    deg = bsplineDegree bspline
    knots = bsplineKnots bspline
    pts = bsplineControlPoints bspline
    numPts = length pts
    (nkLo, nkHi) = bounds knots
    numKnots = nkHi - nkLo + 1

    knotLow = indexKnot knots deg
    knotHigh = indexKnot knots numPts

    spanIdx = findKnotSpan knots deg (numPts - 1) u

    -- Compute the deg+1 non-zero basis functions at u
    basisVals = computeBasisFunctions knots deg spanIdx u

    -- Extract the relevant control points and weights
    startIdx = spanIdx - deg
    relevantPts = take (deg + 1) (drop startIdx pts)
    relevantWeights = take (deg + 1) (drop startIdx weights)

    -- Weighted sum: numerator = sum(N_i * w_i * P_i)
    -- Denominator = sum(N_i * w_i)
    (numerator, denominator) =
      foldl'
        accumulateWeighted
        (vzero, 0.0)
        (zip3 basisVals relevantWeights relevantPts)

    accumulateWeighted (!accNum, !accDen) (basis, weight, pt) =
      let factor = basis * weight
       in (accNum ^+^ factor *^ pt, accDen + factor)

-- ----------------------------------------------------------------
-- Basis function computation
-- ----------------------------------------------------------------

-- | Compute the @deg + 1@ non-zero B-spline basis functions at
-- parameter @u@, given the knot span index. Uses the Cox-de Boor
-- recurrence with the triangular table approach.
--
-- Returns a list of length @deg + 1@.
computeBasisFunctions :: Array Int Float -> Int -> Int -> Float -> [Float]
computeBasisFunctions knots deg spanIdx u = go [1.0] 1
  where
    go current level
      | level > deg = current
      | otherwise = go next (level + 1)
      where
        -- For each level, we expand from 'level' values to 'level+1' values.
        -- The left boundary term:
        leftTerm =
          let knotRight = indexKnot knots (spanIdx + 1)
              knotLeft = indexKnot knots (spanIdx + 1 - level)
              denom = knotRight - knotLeft
           in if abs denom < zeroKnotSpanThreshold
                then 0.0
                else (knotRight - u) / denom * firstBasis current
        -- The right boundary term:
        rightTerm =
          let knotRight = indexKnot knots (spanIdx + level)
              knotLeft = indexKnot knots spanIdx
              denom = knotRight - knotLeft
           in if abs denom < zeroKnotSpanThreshold
                then 0.0
                else (u - knotLeft) / denom * lastBasis current
        -- Interior terms: combine pairs
        interiorTerms =
          zipWith3
            computeInterior
            [1 ..]
            current
            (drop 1 current)

        computeInterior offset bLeft bRight =
          let leftIdx = spanIdx + 1 - level + offset
              rightIdx = leftIdx + level
              knotL = indexKnot knots leftIdx
              knotR = indexKnot knots rightIdx
              denomL = knotR - knotL
              partLeft =
                if abs denomL < zeroKnotSpanThreshold
                  then 0.0
                  else (knotR - u) / denomL * bRight
              rightKnotL = indexKnot knots (leftIdx - 1)
              rightKnotR = indexKnot knots (leftIdx - 1 + level)
              denomR = rightKnotR - rightKnotL
              partRight =
                if abs denomR < zeroKnotSpanThreshold
                  then 0.0
                  else (u - rightKnotL) / denomR * bLeft
           in partLeft + partRight
        next = [leftTerm] ++ interiorTerms ++ [rightTerm]

    firstBasis (x : _) = x
    firstBasis [] = 0.0

    lastBasis [x] = x
    lastBasis (_ : rest) = lastBasis rest
    lastBasis [] = 0.0

-- ----------------------------------------------------------------
-- Arc-length parameterization
-- ----------------------------------------------------------------

-- | Precomputed arc-length lookup table for fast inverse mapping
-- from arc length to curve parameter.
data ArcLengthTable = ArcLengthTable
  { -- | Pairs of (parameter, cumulative arc length) in an Array for O(1) lookup
    arcLengthEntries :: !(Array Int (Float, Float)),
    -- | Total arc length of the curve
    arcLengthTotal :: !Float
  }
  deriving (Show, Eq)

-- | Build an arc-length table by sampling the curve at uniform
-- parameter intervals and integrating the derivative magnitude
-- via 5-point Gauss-Legendre quadrature.
--
-- Parameters:
--
-- * @evalFn@ — evaluates the derivative at a parameter value
-- * @numSamples@ — number of integration segments (clamped to >= 1)
-- * @paramMin@ — start of parameter range
-- * @paramMax@ — end of parameter range
buildArcLengthTable ::
  (VecSpace a) =>
  -- | Derivative evaluation function
  (Float -> a) ->
  -- | Derivative magnitude function
  (a -> Float) ->
  -- | Number of integration segments (clamped to >= 1)
  Int ->
  -- | Parameter range start
  Float ->
  -- | Parameter range end
  Float ->
  ArcLengthTable
buildArcLengthTable derivFn magnitudeFn samplesRaw paramMin paramMax =
  ArcLengthTable entriesArr total
  where
    samples = max minArcLengthSamples samplesRaw
    paramStep = (paramMax - paramMin) / fromIntegral samples

    -- Build entries: first entry is (paramMin, 0), then accumulate
    entriesList = scanl accumSegment (paramMin, 0.0) [1 .. samples]
    entriesArr = listArray (0, samples) entriesList

    accumSegment (_, !cumLen) segIdx =
      let segStart = paramMin + fromIntegral (segIdx - 1) * paramStep
          segEnd = paramMin + fromIntegral segIdx * paramStep
          segLen = gaussLegendre5 derivFn magnitudeFn segStart segEnd
       in (segEnd, cumLen + segLen)

    total = snd (entriesArr ! samples)

-- | Map an arc-length value @s@ to the corresponding curve parameter.
-- Uses binary search on the precomputed table with linear interpolation.
-- The input is clamped to @[0, totalArcLength]@.
arcLengthToParam :: ArcLengthTable -> Float -> Float
arcLengthToParam (ArcLengthTable entries total) sRaw
  | entryCount == 0 = 0.0
  | sRaw <= 0.0 = fst (entries ! eLo)
  | sRaw >= total = fst (entries ! eHi)
  | otherwise = binarySearchArcLength entries sClamped
  where
    (eLo, eHi) = bounds entries
    entryCount = eHi - eLo + 1
    sClamped = max 0.0 (min total sRaw)

-- | Get the total arc length from a precomputed table.
totalArcLength :: ArcLengthTable -> Float
totalArcLength = arcLengthTotal

-- ----------------------------------------------------------------
-- Arc-length internal helpers
-- ----------------------------------------------------------------

-- | Minimum number of arc-length integration segments.
minArcLengthSamples :: Int
minArcLengthSamples = 1

-- | Binary search the arc-length table to find the segment containing
-- the target arc length, then linearly interpolate the parameter.
binarySearchArcLength :: Array Int (Float, Float) -> Float -> Float
binarySearchArcLength entries targetS = interpolateSegment lo hi
  where
    (eLo, eHi) = bounds entries
    (lo, hi) = searchEntries eLo eHi

    searchEntries loIdx hiIdx
      | hiIdx - loIdx <= 1 = (entries ! loIdx, entries ! hiIdx)
      | sndOfEntry midEntry > targetS = searchEntries loIdx midIdx
      | otherwise = searchEntries midIdx hiIdx
      where
        midIdx = (loIdx + hiIdx) `div` 2
        midEntry = entries ! midIdx

    sndOfEntry (_, s) = s

    interpolateSegment (paramLo, sLo) (paramHi, sHi)
      | abs segLen < zeroKnotSpanThreshold = paramLo
      | otherwise = paramLo + fraction * (paramHi - paramLo)
      where
        segLen = sHi - sLo
        fraction = (targetS - sLo) / segLen

-- | 5-point Gauss-Legendre quadrature over @[a, b]@.
-- Integrates the magnitude of the derivative function.
gaussLegendre5 ::
  (VecSpace a) =>
  -- | Derivative evaluation function
  (Float -> a) ->
  -- | Magnitude function
  (a -> Float) ->
  -- | Lower bound
  Float ->
  -- | Upper bound
  Float ->
  Float
gaussLegendre5 derivFn magnitudeFn lower upper = halfWidth * weightedSum
  where
    halfWidth = (upper - lower) * 0.5
    midpoint = (upper + lower) * 0.5

    -- Evaluate at each Gauss point and sum weighted values
    weightedSum =
      gl5Weight0 * sampleAt gl5Node0
        + gl5Weight1 * (sampleAt (-gl5Node1) + sampleAt gl5Node1)
        + gl5Weight2 * (sampleAt (-gl5Node2) + sampleAt gl5Node2)

    sampleAt node =
      let param = midpoint + halfWidth * node
       in magnitudeFn (derivFn param)

-- ----------------------------------------------------------------
-- Gauss-Legendre 5-point quadrature constants
-- ----------------------------------------------------------------

-- | Node 0 (center) for 5-point Gauss-Legendre: t = 0.
gl5Node0 :: Float
gl5Node0 = 0.0

-- | Node 1 for 5-point Gauss-Legendre: +/- 1\/3 * sqrt(5 - 2 * sqrt(10\/7)).
gl5Node1 :: Float
gl5Node1 = 0.5384693101056831

-- | Node 2 for 5-point Gauss-Legendre: +/- 1\/3 * sqrt(5 + 2 * sqrt(10\/7)).
gl5Node2 :: Float
gl5Node2 = 0.9061798459386640

-- | Weight 0 for 5-point Gauss-Legendre: 128/225.
gl5Weight0 :: Float
gl5Weight0 = 0.5688888888888889

-- | Weight 1 for 5-point Gauss-Legendre: (322 + 13*sqrt(70)) / 900.
gl5Weight1 :: Float
gl5Weight1 = 0.4786286704993665

-- | Weight 2 for 5-point Gauss-Legendre: (322 - 13*sqrt(70)) / 900.
gl5Weight2 :: Float
gl5Weight2 = 0.2369268850561891
