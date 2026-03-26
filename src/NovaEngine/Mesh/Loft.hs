-- | Lofted and swept mesh surfaces.
--
-- Revolve (lathe), extrude, loft from pre-sampled rings, and sweep
-- a 2D profile along a 3D spine with rotation-minimizing Bishop
-- frames.
module NovaEngine.Mesh.Loft
  ( -- * Revolve
    revolve,

    -- * Loft
    loftRings,

    -- * Extrude
    extrude,

    -- * Sweep
    sweep,
  )
where

import Data.Array (Array, listArray, (!))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Minimum tessellation segments around a profile circumference.
minProfileSegments :: Int
minProfileSegments = 3

-- | Minimum tessellation segments along a spine or extrusion axis.
minSpineSegments :: Int
minSpineSegments = 1

-- | Minimum number of rings required for lofting.
minLoftRings :: Int
minLoftRings = 2

-- | Minimum points per ring for lofting.
minRingPoints :: Int
minRingPoints = 3

-- | Threshold below which a vector is considered zero-length.
nearZeroThreshold :: Float
nearZeroThreshold = 1.0e-6

-- | Default tangent when analytical computation degenerates.
defaultTangent :: V4
defaultTangent = V4 1 0 0 1

-- | Y-axis unit vector.
yAxis :: V3
yAxis = V3 0 1 0

-- | X-axis unit vector.
xAxis :: V3
xAxis = V3 1 0 0

-- | Reflection scalar factor used in Bishop frame propagation.
reflectionFactor :: Float
reflectionFactor = 2.0

-- | Threshold for detecting near-parallel vectors when building
-- perpendicular frames.
perpendicularThreshold :: Float
perpendicularThreshold = 0.9

-- | Half used for centering pole vertex UVs.
poleUVOffset :: Float
poleUVOffset = 0.5

-- ----------------------------------------------------------------
-- Revolve (lathe)
-- ----------------------------------------------------------------

-- | Rotate a 2D profile curve around the Y axis to create a surface
-- of revolution.
--
-- The profile function maps @t@ in @[0, 1]@ to @(radius, height)@.
-- The derivative function provides analytical @(dr\/dt, dz\/dt)@ for
-- smooth normals. Pole handling generates triangle fans when the
-- profile radius is zero. Seam vertices are duplicated at the sweep
-- boundary for correct UV mapping.
revolve ::
  -- | Profile: @t -> (radius, height)@
  (Float -> V2) ->
  -- | Profile derivative: @t -> (dr\/dt, dz\/dt)@
  (Float -> V2) ->
  -- | Profile segments along @t@ (clamped to >= 1)
  Int ->
  -- | Slices around Y axis (clamped to >= 3)
  Int ->
  -- | Sweep angle in radians (e.g. @2 * pi@ for full revolution)
  Float ->
  Mesh
revolve profile profileDeriv profileSegsRaw slicesRaw sweepAngle =
  mkMesh vertices indices
  where
    profileSegs = max minSpineSegments profileSegsRaw
    slices = max minProfileSegments slicesRaw

    -- Sample profile at each t value
    profileSamples =
      [ let t = fromIntegral i / fromIntegral profileSegs
         in (t, profile t, profileDeriv t)
      | i <- [0 .. profileSegs]
      ]

    -- Identify poles: profile points where radius is near zero
    topIsPole = case profileSamples of
      ((_, V2 radius _, _) : _) -> abs radius < nearZeroThreshold
      [] -> False

    bottomIsPole = case reverse profileSamples of
      ((_, V2 radius _, _) : _) -> abs radius < nearZeroThreshold
      [] -> False

    -- Top pole vertices (one per slice for triangle fan)
    -- Normal direction at pole is determined by the profile derivative:
    -- if dz/dt > 0 (going up from pole), normal points up (+Y);
    -- if dz/dt < 0 (going down from pole), normal points down (-Y).
    topPoleVerts = case profileSamples of
      ((_, V2 _ height, V2 _ dzdt) : _) ->
        [ let u = (fromIntegral j + poleUVOffset) / fromIntegral slices
              poleNormalY = if dzdt >= 0 then 1.0 else (-1.0)
           in vertex
                (V3 0 height 0)
                (V3 0 poleNormalY 0)
                (V2 u 0)
                defaultTangent
        | j <- [0 .. slices - 1]
        ]
      [] -> []

    -- Bottom pole vertices (one per slice for triangle fan)
    bottomPoleVerts = case reverse profileSamples of
      ((_, V2 _ height, V2 _ dzdt) : _) ->
        [ let u = (fromIntegral j + poleUVOffset) / fromIntegral slices
              poleNormalY = if dzdt <= 0 then (-1.0) else 1.0
           in vertex
                (V3 0 height 0)
                (V3 0 poleNormalY 0)
                (V2 u 1)
                defaultTangent
        | j <- [0 .. slices - 1]
        ]
      [] -> []

    -- Body rows: skip pole rows at top/bottom
    bodySamples =
      let dropTop = if topIsPole then drop 1 else id
          trimBottom = if bottomIsPole then dropLast else id
       in trimBottom (dropTop profileSamples)

    bodyVerts =
      [ let theta = fromIntegral j * sweepAngle / fromIntegral slices
            sinTheta = sin theta
            cosTheta = cos theta
            V2 radius height = samplePos
            V2 drdt dzdt = sampleDeriv
            -- S(t, theta) = (r * cos theta, z, r * sin theta)
            px = radius * cosTheta
            py = height
            pz = radius * sinTheta
            -- Partial derivatives for analytical normal
            -- dS/dt = (dr/dt * cos theta, dz/dt, dr/dt * sin theta)
            -- dS/dtheta = (-r * sin theta, 0, r * cos theta)
            dsdt = V3 (drdt * cosTheta) dzdt (drdt * sinTheta)
            dsdtheta = V3 (negate radius * sinTheta) 0 (radius * cosTheta)
            -- Outward normal: use cross product, then ensure it points
            -- radially away from the Y axis via dot check
            radialDir = V3 cosTheta 0 sinTheta
            candidate = cross dsdtheta dsdt
            outwardCandidate =
              if dot candidate radialDir >= 0
                then candidate
                else negateV candidate
            normal = safeNormalize radialDir outwardCandidate
            u = fromIntegral j / fromIntegral slices
            v = sampleT
            -- Tangent: dS/dtheta direction
            tangentDir = safeNormalize (V3 (negate sinTheta) 0 cosTheta) dsdtheta
            V3 tx ty tz = tangentDir
         in vertex (V3 px py pz) normal (V2 u v) (V4 tx ty tz 1)
      | (sampleT, samplePos, sampleDeriv) <- bodySamples,
        j <- [0 .. slices]
      ]

    bodyRowCount = length bodySamples
    bodyColCount = slices + 1

    -- Assemble vertices
    vertices =
      (if topIsPole then topPoleVerts else [])
        ++ bodyVerts
        ++ (if bottomIsPole then bottomPoleVerts else [])

    -- Indices: top pole fan
    topPoleIndices =
      if topIsPole
        then
          [ idx
          | j <- [0 .. slices - 1],
            let poleIdx = fromIntegral j
                bodyBase = fromIntegral slices
                bj = bodyBase + fromIntegral j,
            idx <-
              if profileGoesUp
                then [poleIdx, bj + 1, bj]
                else [poleIdx, bj, bj + 1]
          ]
        else []

    topPoleVertCount = if topIsPole then slices else 0

    -- Indices: body quad bands
    -- Profile direction determines winding. When profile goes upward
    -- (height increases with t), cross(t_step, theta_step) = outward,
    -- so use [a, c, b, b, c, d]. When downward, use [a, b, c, b, d, c].
    profileGoesUp = case bodySamples of
      ((_, V2 _ z0, _) : rest) -> case lastSample rest of
        Just (_, V2 _ z1, _) -> z1 > z0
        Nothing -> True
      [] -> True

    lastSample [x] = Just x
    lastSample (_ : xs) = lastSample xs
    lastSample [] = Nothing

    bodyIndices =
      [ idx
      | i <- [0 .. bodyRowCount - 2],
        j <- [0 .. slices - 1],
        let rowWidth = fromIntegral bodyColCount
            bodyBase = fromIntegral topPoleVertCount
            a = bodyBase + fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <-
          if profileGoesUp
            then [a, c, b, b, c, d]
            else [a, b, c, b, d, c]
      ]

    -- Indices: bottom pole fan
    bottomPoleIndices =
      if bottomIsPole
        then
          [ idx
          | j <- [0 .. slices - 1],
            let poleBase = fromIntegral topPoleVertCount + fromIntegral (bodyRowCount * bodyColCount)
                poleIdx = poleBase + fromIntegral j
                lastBodyBase = fromIntegral topPoleVertCount + fromIntegral ((bodyRowCount - 1) * bodyColCount)
                bj = lastBodyBase + fromIntegral j,
            idx <-
              if profileGoesUp
                then [bj, bj + 1, poleIdx]
                else [bj + 1, bj, poleIdx]
          ]
        else []

    indices = topPoleIndices ++ bodyIndices ++ bottomPoleIndices

-- ----------------------------------------------------------------
-- Loft from pre-sampled rings
-- ----------------------------------------------------------------

-- | Connect a list of pre-sampled vertex rings into a lofted mesh.
--
-- Each ring is a list of 3D positions. All rings should have the
-- same number of points. Normals are computed from cross products
-- of the ring tangent and spine direction. Returns 'Nothing' if
-- fewer than two rings are provided or any ring has fewer than
-- three points.
loftRings ::
  -- | List of rings (at least 2, each at least 3 points)
  [[V3]] ->
  -- | Closed profiles (connect last point to first in each ring)
  Bool ->
  Maybe Mesh
loftRings rings closedProfile
  | length rings < minLoftRings = Nothing
  | any (\ring -> length ring < minRingPoints) rings = Nothing
  | not (allSameLength rings) = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    ringCount = length rings
    pointsPerRing = case rings of
      (firstRing : _) -> length firstRing
      [] -> 0

    -- Column count includes seam duplication for closed profiles
    colCount = if closedProfile then pointsPerRing + 1 else pointsPerRing

    -- Convert rings to Arrays for O(1) point lookup
    ringArrays :: Array Int (Array Int V3)
    ringArrays = listArray (0, ringCount - 1) (map ringToArray rings)

    ringToArray :: [V3] -> Array Int V3
    ringToArray [] = listArray (0, -1) []
    ringToArray ps = listArray (0, length ps - 1) ps

    -- Compute spine direction for each ring (for normal computation)
    spineDirections = computeSpineDirections rings
    spineArr :: Array Int V3
    spineArr = listArray (0, max 0 (length spineDirections - 1)) spineDirections

    -- Generate vertices
    vertices =
      [ let pointIdx = j `mod` pointsPerRing
            ringArr = ringArrays ! i
            pos = ringArr ! pointIdx
            -- Ring tangent: direction along the ring
            nextPointIdx = (pointIdx + 1) `mod` pointsPerRing
            prevPointIdx = (pointIdx + pointsPerRing - 1) `mod` pointsPerRing
            nextPos = ringArr ! nextPointIdx
            prevPos = ringArr ! prevPointIdx
            ringTangent = safeNormalize xAxis (nextPos ^-^ prevPos)
            -- Spine direction at this ring
            spineDir = spineArr ! i
            -- Normal: cross product of spine direction and ring tangent
            rawNormal = cross spineDir ringTangent
            normal = safeNormalize yAxis rawNormal
            -- UV coordinates
            u = fromIntegral j / fromIntegral (max 1 (colCount - 1))
            v = fromIntegral i / fromIntegral (max 1 (ringCount - 1))
            -- Tangent: ring direction
            V3 tx ty tz = ringTangent
         in vertex pos normal (V2 u v) (V4 tx ty tz 1)
      | i <- [0 .. ringCount - 1],
        j <- [0 .. colCount - 1]
      ]

    -- Generate quad indices between consecutive rings
    indices =
      [ idx
      | i <- [0 .. ringCount - 2],
        j <- [0 .. colCount - 2],
        let rowWidth = fromIntegral colCount
            a = fromIntegral i * rowWidth + fromIntegral j
            b = a + 1
            c = a + rowWidth
            d = c + 1,
        idx <- [a, b, c, b, d, c]
      ]

-- ----------------------------------------------------------------
-- Extrude
-- ----------------------------------------------------------------

-- | Push a 2D profile along a direction to create an extruded mesh.
--
-- The profile function maps @t@ in @[0, 1]@ to 2D cross-section
-- coordinates @(x, y)@, placed in the plane perpendicular to the
-- extrusion direction. The derivative provides analytical tangents.
extrude ::
  -- | Profile: @t -> (x, y)@ in cross-section plane
  (Float -> V2) ->
  -- | Profile derivative: @t -> (dx\/dt, dy\/dt)@
  (Float -> V2) ->
  -- | Extrusion direction (will be normalized)
  V3 ->
  -- | Extrusion length
  Float ->
  -- | Profile segments (clamped to >= 3)
  Int ->
  -- | Extrusion segments (clamped to >= 1)
  Int ->
  Mesh
extrude profile profileDeriv direction extrusionLength profileSegsRaw extrudeSegsRaw =
  mkMesh vertices indices
  where
    profileSegs = max minProfileSegments profileSegsRaw
    extrudeSegs = max minSpineSegments extrudeSegsRaw

    -- Normalized extrusion direction
    extrudeDir = safeNormalize yAxis direction

    -- Build a coordinate frame perpendicular to extrusion direction
    (frameRight, frameUp) = buildPerpendicularFrame extrudeDir

    -- Generate vertices
    vertices =
      [ let -- Extrusion parameter
            extrudeT = fromIntegral i / fromIntegral extrudeSegs
            spinePos = (extrudeT * extrusionLength) *^ extrudeDir
            -- Profile parameter
            profileT = fromIntegral j / fromIntegral profileSegs
            V2 profileX profileY = profile profileT
            -- Position: spine position + profile offset in local frame
            pos = spinePos ^+^ profileX *^ frameRight ^+^ profileY *^ frameUp
            -- Normal: outward from profile
            V2 derivX derivY = profileDeriv profileT
            -- Profile tangent in 3D (along profile curve)
            profileTangent3D = derivX *^ frameRight ^+^ derivY *^ frameUp
            -- Normal = cross(extrude direction, profile tangent)
            rawNormal = cross extrudeDir profileTangent3D
            normal = safeNormalize frameUp rawNormal
            -- UV
            u = fromIntegral j / fromIntegral profileSegs
            v = extrudeT
            -- Surface tangent: along extrusion direction
            V3 tx ty tz = extrudeDir
         in vertex pos normal (V2 u v) (V4 tx ty tz 1)
      | i <- [0 .. extrudeSegs],
        j <- [0 .. profileSegs]
      ]

    -- Standard grid indices
    indices = buildGridIndices extrudeSegs profileSegs

-- ----------------------------------------------------------------
-- Sweep
-- ----------------------------------------------------------------

-- | Move a 2D profile along a 3D spine with rotation-minimizing
-- Bishop frames (double-reflection algorithm, Wang et al. 2008).
--
-- The spine curve and its derivative define the path. The profile
-- is evaluated in the local frame at each spine sample point.
sweep ::
  -- | Spine curve: @t -> position@
  (Float -> V3) ->
  -- | Spine derivative: @t -> tangent direction@
  (Float -> V3) ->
  -- | Profile: @t -> (x, y)@ in cross-section plane
  (Float -> V2) ->
  -- | Spine segments (clamped to >= 1)
  Int ->
  -- | Profile segments (clamped to >= 3)
  Int ->
  Mesh
sweep spineCurve spineDeriv profile spineSegsRaw profileSegsRaw =
  mkMesh vertices indices
  where
    spineSegs = max minSpineSegments spineSegsRaw
    profileSegs = max minProfileSegments profileSegsRaw

    -- Sample spine positions and tangents
    spinePoints =
      [ spineCurve (fromIntegral i / fromIntegral spineSegs)
      | i <- [0 .. spineSegs]
      ]

    spineTangents =
      [ safeNormalize yAxis (spineDeriv (fromIntegral i / fromIntegral spineSegs))
      | i <- [0 .. spineSegs]
      ]

    -- Compute rotation-minimizing frames along the spine
    frames = bishopFrames spinePoints spineTangents

    -- Generate vertices
    vertices =
      [ let spinePos = fromMaybe vzero (safeIndex spinePoints i)
            (frameTangent, frameUp, frameRight) = fromMaybe (yAxis, xAxis, V3 0 0 1) (safeIndex frames i)
            -- Profile parameter
            profileT = fromIntegral j / fromIntegral profileSegs
            V2 profileX profileY = profile profileT
            -- Position in local frame
            pos = spinePos ^+^ profileX *^ frameRight ^+^ profileY *^ frameUp
            -- Normal: outward from spine
            outward = pos ^-^ spinePos
            normal = safeNormalize frameUp outward
            -- UV
            u = fromIntegral j / fromIntegral profileSegs
            v = fromIntegral i / fromIntegral spineSegs
            -- Surface tangent: spine direction
            V3 tx ty tz = frameTangent
         in vertex pos normal (V2 u v) (V4 tx ty tz 1)
      | i <- [0 .. spineSegs],
        j <- [0 .. profileSegs]
      ]

    -- Standard grid indices
    indices = buildGridIndices spineSegs profileSegs

-- ----------------------------------------------------------------
-- Bishop frame (double-reflection algorithm)
-- ----------------------------------------------------------------

-- | Compute rotation-minimizing frames along a discrete spine using
-- the double-reflection method (Wang et al. 2008).
--
-- Returns @(tangent, up, right)@ frames at each spine point.
-- The initial frame is constructed from the first tangent using
-- an arbitrary perpendicular vector.
bishopFrames ::
  -- | Spine positions
  [V3] ->
  -- | Spine tangents (normalized)
  [V3] ->
  -- | Frames: @(tangent, up, right)@ at each point
  [(V3, V3, V3)]
bishopFrames positions tangents = case (positions, tangents) of
  ([], _) -> []
  (_, []) -> []
  (_ : _, firstTangent : _) ->
    let (initialRight, initialUp) = buildPerpendicularFrame firstTangent
        initialFrame = (firstTangent, initialUp, initialRight)
        -- Pair each step with (prevPos, nextPos, nextTangent)
        steps = zip3 positions (drop 1 positions) (drop 1 tangents)
     in strictScanl propagateFrame initialFrame steps
  where
    propagateFrame (prevTangent, prevUp, prevRight) (prevPos, nextPos, nextTangent) =
      -- Wang et al. 2008 double-reflection:
      -- 1. Reflect across plane perpendicular to chord vector
      -- 2. Reflect again across plane perpendicular to tangent difference
      let chord = nextPos ^-^ prevPos
          chordSq = dot chord chord
       in if chordSq < nearZeroThreshold
            then (nextTangent, prevUp, prevRight)
            else
              let factor1 = reflectionFactor / chordSq
                  upRefl = prevUp ^-^ (factor1 * dot chord prevUp) *^ chord
                  rightRefl = prevRight ^-^ (factor1 * dot chord prevRight) *^ chord
                  tangentRefl = prevTangent ^-^ (factor1 * dot chord prevTangent) *^ chord
                  tangentDiff = nextTangent ^-^ tangentRefl
                  tangentDiffSq = dot tangentDiff tangentDiff
               in if tangentDiffSq < nearZeroThreshold
                    then (nextTangent, upRefl, rightRefl)
                    else
                      let factor2 = reflectionFactor / tangentDiffSq
                          newUp = upRefl ^-^ (factor2 * dot tangentDiff upRefl) *^ tangentDiff
                          newRight = rightRefl ^-^ (factor2 * dot tangentDiff rightRefl) *^ tangentDiff
                       in (nextTangent, newUp, newRight)

-- ----------------------------------------------------------------
-- Grid index generation
-- ----------------------------------------------------------------

-- | Generate triangle indices for a grid of @(rows + 1) x (cols + 1)@
-- vertices. Produces two triangles per quad cell with CCW winding.
buildGridIndices ::
  -- | Number of row segments
  Int ->
  -- | Number of column segments
  Int ->
  [Word32]
buildGridIndices rows cols =
  [ idx
  | i <- [0 .. rows - 1],
    j <- [0 .. cols - 1],
    let rowWidth = fromIntegral (cols + 1)
        a = fromIntegral i * rowWidth + fromIntegral j
        b = a + 1
        c = a + rowWidth
        d = c + 1,
    idx <- [a, b, c, b, d, c]
  ]

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Build a perpendicular frame from a direction vector.
-- Returns @(right, up)@ where @right@ and @up@ are perpendicular
-- to the input direction and to each other.
buildPerpendicularFrame :: V3 -> (V3, V3)
buildPerpendicularFrame dir =
  let -- Choose a vector not parallel to dir for cross product
      candidate =
        if abs (dot dir yAxis) < perpendicularThreshold
          then yAxis
          else xAxis
      right = safeNormalize xAxis (cross dir candidate)
      up = safeNormalize yAxis (cross right dir)
   in (right, up)

-- | Compute spine directions for loft ring interpolation.
-- Uses forward differences for the first ring, backward for the
-- last, and central differences for interior rings.
computeSpineDirections :: [[V3]] -> [V3]
computeSpineDirections [] = []
computeSpineDirections [single] = [safeNormalize yAxis (ringCenter single)]
computeSpineDirections allRings =
  let centers = map ringCenter allRings
      centerCount = length centers
   in [ computeDirection centers i centerCount
      | i <- [0 .. centerCount - 1]
      ]
  where
    computeDirection centers idx count
      | idx == 0 =
          safeNormalize yAxis (fromMaybe vzero (safeIndex centers 1) ^-^ fromMaybe vzero (safeIndex centers 0))
      | idx == count - 1 =
          safeNormalize yAxis (fromMaybe vzero (safeIndex centers (count - 1)) ^-^ fromMaybe vzero (safeIndex centers (count - 2)))
      | otherwise =
          safeNormalize yAxis (fromMaybe vzero (safeIndex centers (idx + 1)) ^-^ fromMaybe vzero (safeIndex centers (idx - 1)))

-- | Compute the centroid of a list of points.
ringCenter :: [V3] -> V3
ringCenter [] = vzero
ringCenter points =
  let pointCount = length points
      summed = foldl' (^+^) vzero points
   in (1.0 / fromIntegral pointCount) *^ summed

-- | Strict left scan, like 'scanl' but with strict accumulator
-- evaluation.
strictScanl :: (b -> a -> b) -> b -> [a] -> [b]
strictScanl _ initial [] = [initial]
strictScanl fn initial (x : xs) =
  let !next = fn initial x
   in initial : strictScanl fn next xs

-- | Drop the last element from a list. Returns the empty list if
-- given an empty list.
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [_] = []
dropLast (x : xs) = x : dropLast xs

-- | Check that all sublists have the same length.
allSameLength :: [[a]] -> Bool
allSameLength [] = True
allSameLength (x : xs) = all (\y -> length y == len) xs
  where
    len = length x
