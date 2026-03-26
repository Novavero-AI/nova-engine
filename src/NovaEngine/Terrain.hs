-- | Heightmap terrain generation and erosion simulation.
--
-- Grid-based terrain from height functions or explicit heightmaps,
-- with thermal and hydraulic erosion, terracing, plateau clamping,
-- and heightmap blending.
module NovaEngine.Terrain
  ( -- * Height function
    HeightFn,

    -- * Terrain generation
    terrain,
    fromHeightmap,
    sampleGrid,

    -- * Heightmap transforms
    terrace,
    plateau,
    clampHeights,
    blendHeightmaps,

    -- * Erosion
    thermalErosion,
    hydraulicErosion,
  )
where

import Data.Array (Array, accum, array, listArray, range, (!))
import Data.List (foldl')
import NovaEngine.Mesh.Types
  ( Mesh,
    V2 (..),
    V3 (..),
    V4 (..),
    Word32,
    applyIterations,
    clampF,
    cross,
    dot,
    fastFloor,
    mkMesh,
    nearZeroLength,
    normalize,
    safeNormalize,
    vertex,
  )

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | A height function mapping (x, z) world coordinates to a y height.
type HeightFn = Float -> Float -> Float

-- ----------------------------------------------------------------
-- Terrain generation
-- ----------------------------------------------------------------

-- | Generate an XZ grid mesh from a height function.
--
-- The grid spans from @(-width\/2, -depth\/2)@ to @(width\/2, depth\/2)@
-- with @(segsX+1) * (segsZ+1)@ vertices. Returns 'Nothing' if width
-- or depth is not positive.
terrain ::
  -- | Height function mapping (x, z) to y
  HeightFn ->
  -- | Width (X extent)
  Float ->
  -- | Depth (Z extent)
  Float ->
  -- | Segments along X (clamped to >= 1)
  Int ->
  -- | Segments along Z (clamped to >= 1)
  Int ->
  Maybe Mesh
terrain heightFn width depth segsXRaw segsZRaw
  | width <= 0 || depth <= 0 = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    segsX = max 1 segsXRaw
    segsZ = max 1 segsZRaw
    sz = segsZ + 1
    dx = width / fromIntegral segsX
    dz = depth / fromIntegral segsZ
    halfW = width / 2.0
    halfD = depth / 2.0
    fSegsX = fromIntegral segsX :: Float
    fSegsZ = fromIntegral segsZ :: Float

    -- Pre-sample heights into an array for O(1) normal lookups
    heightArr :: Array (Int, Int) Float
    heightArr =
      array
        ((0, 0), (segsX, segsZ))
        [ ((ix, iz), heightFn x z)
        | ix <- [0 .. segsX],
          let x = -halfW + fromIntegral ix * dx,
          iz <- [0 .. segsZ],
          let z = -halfD + fromIntegral iz * dz
        ]

    -- Look up height with clamped indices for boundary normals
    heightAt :: Int -> Int -> Float
    heightAt ix iz = heightArr ! (clampI 0 segsX ix, clampI 0 segsZ iz)

    vertices =
      [ let x = -halfW + fromIntegral ix * dx
            z = -halfD + fromIntegral iz * dz
            y = heightArr ! (ix, iz)
            -- Central differences for normal: n = (-dh/dx, 1, -dh/dz)
            slopeX = (heightAt (ix - 1) iz - heightAt (ix + 1) iz) / (2.0 * dx)
            slopeZ = (heightAt ix (iz - 1) - heightAt ix (iz + 1)) / (2.0 * dz)
            nrm = normalize (V3 slopeX 1.0 slopeZ)
            u = fromIntegral ix / fSegsX
            v = fromIntegral iz / fSegsZ
            -- Tangent: dS/dx = (1, dh/dx, 0), where dh/dx = -slopeX
            dSdu = V3 1.0 (negate slopeX) 0.0
            tangentDir = safeNormalize (V3 1 0 0) dSdu
            -- Handedness from bitangent direction dS/dz = (0, dh/dz, 1)
            dSdv = V3 0.0 (negate slopeZ) 1.0
            handedness = if dot (cross nrm tangentDir) dSdv >= 0 then 1.0 else (-1.0)
            V3 tx ty tz = tangentDir
         in vertex (V3 x y z) nrm (V2 u v) (V4 tx ty tz handedness)
      | ix <- [0 .. segsX],
        iz <- [0 .. segsZ]
      ]

    indices =
      [ idx
      | ix <- [0 .. segsX - 1],
        iz <- [0 .. segsZ - 1],
        let tl = fromIntegral (ix * sz + iz) :: Word32
            tr = fromIntegral (ix * sz + iz + 1)
            bl = fromIntegral ((ix + 1) * sz + iz)
            br = fromIntegral ((ix + 1) * sz + iz + 1),
        idx <- [tl, bl, tr, tr, bl, br]
      ]

-- | Build a terrain mesh from an explicit heightmap grid.
--
-- The outer list is rows along X, inner list is columns along Z.
-- Returns 'Nothing' if width or depth is not positive, the grid is
-- empty, or rows have inconsistent lengths.
fromHeightmap ::
  -- | Width (X extent)
  Float ->
  -- | Depth (Z extent)
  Float ->
  -- | Heightmap grid (outer = X rows, inner = Z columns)
  [[Float]] ->
  Maybe Mesh
fromHeightmap _ _ [] = Nothing
fromHeightmap width depth grid@(firstRow : _)
  | width <= 0 || depth <= 0 = Nothing
  | cols == 0 = Nothing
  | any (\r -> length r /= cols) grid = Nothing
  | otherwise = Just (mkMesh vertices indices)
  where
    rows = length grid
    cols = length firstRow
    segsX = rows - 1
    segsZ = cols - 1
    sz = cols
    dx = if segsX > 0 then width / fromIntegral segsX else 1.0
    dz = if segsZ > 0 then depth / fromIntegral segsZ else 1.0
    halfW = width / 2.0
    halfD = depth / 2.0
    fSegsX = fromIntegral (max 1 segsX) :: Float
    fSegsZ = fromIntegral (max 1 segsZ) :: Float

    -- Build array from grid
    heightArr :: Array (Int, Int) Float
    heightArr =
      listArray
        ((0, 0), (segsX, segsZ))
        [h | row <- grid, h <- row]

    heightAt :: Int -> Int -> Float
    heightAt ix iz = heightArr ! (clampI 0 segsX ix, clampI 0 segsZ iz)

    vertices =
      [ let x = -halfW + fromIntegral ix * dx
            z = -halfD + fromIntegral iz * dz
            y = heightArr ! (ix, iz)
            -- Central differences for normal: n = (-dh/dx, 1, -dh/dz)
            slopeX = (heightAt (ix - 1) iz - heightAt (ix + 1) iz) / (2.0 * dx)
            slopeZ = (heightAt ix (iz - 1) - heightAt ix (iz + 1)) / (2.0 * dz)
            nrm = normalize (V3 slopeX 1.0 slopeZ)
            u = fromIntegral ix / fSegsX
            v = fromIntegral iz / fSegsZ
            -- Tangent: dS/dx = (1, dh/dx, 0), where dh/dx = -slopeX
            dSdu = V3 1.0 (negate slopeX) 0.0
            tangentDir = safeNormalize (V3 1 0 0) dSdu
            -- Handedness from bitangent direction dS/dz = (0, dh/dz, 1)
            dSdv = V3 0.0 (negate slopeZ) 1.0
            handedness = if dot (cross nrm tangentDir) dSdv >= 0 then 1.0 else (-1.0)
            V3 tx ty tz = tangentDir
         in vertex (V3 x y z) nrm (V2 u v) (V4 tx ty tz handedness)
      | ix <- [0 .. segsX],
        iz <- [0 .. segsZ]
      ]

    indices
      | segsX < 1 || segsZ < 1 = []
      | otherwise =
          [ idx
          | ix <- [0 .. segsX - 1],
            iz <- [0 .. segsZ - 1],
            let tl = fromIntegral (ix * sz + iz) :: Word32
                tr = fromIntegral (ix * sz + iz + 1)
                bl = fromIntegral ((ix + 1) * sz + iz)
                br = fromIntegral ((ix + 1) * sz + iz + 1),
            idx <- [tl, bl, tr, tr, bl, br]
          ]

-- | Sample a height function into a grid of @(segsX+1)@ rows of
-- @(segsZ+1)@ values.
sampleGrid ::
  -- | Height function
  HeightFn ->
  -- | Segments along X (clamped to >= 1)
  Int ->
  -- | Segments along Z (clamped to >= 1)
  Int ->
  -- | Width (X extent)
  Float ->
  -- | Depth (Z extent)
  Float ->
  [[Float]]
sampleGrid heightFn segsXRaw segsZRaw width depth =
  [ [ heightFn x z
    | iz <- [0 .. segsZ],
      let z = -halfD + fromIntegral iz * dz
    ]
  | ix <- [0 .. segsX],
    let x = -halfW + fromIntegral ix * dx
  ]
  where
    segsX = max 1 segsXRaw
    segsZ = max 1 segsZRaw
    halfW = width / 2.0
    halfD = depth / 2.0
    dx = width / fromIntegral segsX
    dz = depth / fromIntegral segsZ

-- ----------------------------------------------------------------
-- Heightmap transforms
-- ----------------------------------------------------------------

-- | Quantize heights to @n@ discrete terrace levels.
--
-- If @n <= 0@ or the height range is zero, the grid is returned
-- unchanged.
terrace ::
  -- | Number of terrace levels
  Int ->
  -- | Input heightmap
  [[Float]] ->
  [[Float]]
terrace n grid
  | n <= 0 = grid
  | null allHeights = grid
  | rangeH < nearZeroLength = grid
  | otherwise = map (map quantize) grid
  where
    allHeights = concat grid
    (minH, maxH) = foldl' (\(!lo, !hi) h -> (min lo h, max hi h)) (1.0 / 0.0, -(1.0 / 0.0)) allHeights
    rangeH = maxH - minH
    fn = fromIntegral n :: Float

    quantize h =
      let t = (h - minH) / rangeH
          level = fromIntegral (fastFloor (t * fn)) / fn
       in level * rangeH + minH

-- | Flatten all heights above a threshold to a replacement value.
plateau ::
  -- | Threshold height
  Float ->
  -- | Replacement height for cells above threshold
  Float ->
  -- | Input heightmap
  [[Float]] ->
  [[Float]]
plateau threshold replacement = map (map clampCell)
  where
    clampCell h = if h > threshold then replacement else h

-- | Clamp all heightmap values to the range @[lo, hi]@.
clampHeights ::
  -- | Minimum height
  Float ->
  -- | Maximum height
  Float ->
  -- | Input heightmap
  [[Float]] ->
  [[Float]]
clampHeights lo hi = map (map (clampF lo hi))

-- | Blend two heightmaps element-wise. Each cell is
-- @(1 - t) * a + t * b@. If grids differ in size, the smaller
-- dimensions are used.
blendHeightmaps ::
  -- | Blend weight (0 = all A, 1 = all B)
  Float ->
  -- | Heightmap A
  [[Float]] ->
  -- | Heightmap B
  [[Float]] ->
  [[Float]]
blendHeightmaps t =
  zipWith (zipWith blendCell)
  where
    blendCell a b = (1.0 - t) * a + t * b

-- ----------------------------------------------------------------
-- Erosion
-- ----------------------------------------------------------------

-- | Iterative thermal erosion simulation.
--
-- Each iteration transfers material from higher to lower cells when
-- the height difference exceeds the talus angle. Uses 'Data.Array'
-- internally for O(1) access.
thermalErosion ::
  -- | Number of iterations (clamped to >= 0)
  Int ->
  -- | Talus angle threshold
  Float ->
  -- | Input heightmap
  [[Float]] ->
  [[Float]]
thermalErosion _ _ [] = []
thermalErosion itersRaw talusAngle grid@(firstRow : _)
  | null firstRow = grid
  | iters <= 0 = grid
  | otherwise = arrayToGrid rows cols (applyIterations iters erodeStep initArr)
  where
    iters = max 0 itersRaw
    rows = length grid
    cols = length firstRow
    maxR = rows - 1
    maxC = cols - 1

    initArr :: Array (Int, Int) Float
    initArr = gridToArray grid rows cols

    erodeStep :: Array (Int, Int) Float -> Array (Int, Int) Float
    erodeStep arr =
      let -- Process each edge once: only transfer from the higher cell
          -- to the lower cell to avoid double-counting.
          edges = uniqueEdges maxR maxC
          updates = concatMap (edgeTransfer arr) edges
       in applyUpdates arr updates

    edgeTransfer ::
      Array (Int, Int) Float ->
      ((Int, Int), (Int, Int)) ->
      [((Int, Int), Float)]
    edgeTransfer arr (src, dst) =
      let hSrc = arr ! src
          hDst = arr ! dst
          diff = hSrc - hDst
       in if abs diff > talusAngle
            then
              let amount = (abs diff - talusAngle) * thermalTransferRate
                  (high, low) = if diff > 0 then (src, dst) else (dst, src)
               in [(high, negate amount), (low, amount)]
            else []

-- | Simplified hydraulic erosion simulation.
--
-- Each iteration adds rain, flows water to the lowest neighbor,
-- erodes proportional to water, deposits in local minima, and
-- evaporates. Uses 'Data.Array' internally for O(1) access.
hydraulicErosion ::
  -- | Number of iterations (clamped to >= 0)
  Int ->
  -- | Rain amount per iteration
  Float ->
  -- | Erosion strength
  Float ->
  -- | Input heightmap
  [[Float]] ->
  [[Float]]
hydraulicErosion _ _ _ [] = []
hydraulicErosion itersRaw rainAmount erosionStrength grid@(firstRow : _)
  | null firstRow = grid
  | iters <= 0 = grid
  | otherwise = arrayToGrid rows cols finalHeight
  where
    iters = max 0 itersRaw
    rows = length grid
    cols = length firstRow
    maxR = rows - 1
    maxC = cols - 1

    initHeight :: Array (Int, Int) Float
    initHeight = gridToArray grid rows cols

    initWater :: Array (Int, Int) Float
    initWater =
      listArray ((0, 0), (maxR, maxC)) (replicate (rows * cols) 0.0)

    (finalHeight, _) = applyIterations iters stepHydraulic (initHeight, initWater)

    stepHydraulic ::
      (Array (Int, Int) Float, Array (Int, Int) Float) ->
      (Array (Int, Int) Float, Array (Int, Int) Float)
    stepHydraulic (hArr, wArr) =
      let -- Step 1: Add rain
          wRain = fmap (+ rainAmount) wArr
          -- Steps 2-5: Flow, erode, deposit, evaporate
          cells = range ((0, 0), (maxR, maxC))
          (hUpdates, wUpdates) = foldl' (processCell hArr wRain) ([], []) cells
          hArrEroded = applyUpdates hArr hUpdates
          wArrFlowed = applyUpdates wRain wUpdates
          -- Step 6: Evaporate
          wFinal = fmap (* evaporationRate) wArrFlowed
       in (hArrEroded, wFinal)

    processCell ::
      Array (Int, Int) Float ->
      Array (Int, Int) Float ->
      ([((Int, Int), Float)], [((Int, Int), Float)]) ->
      (Int, Int) ->
      ([((Int, Int), Float)], [((Int, Int), Float)])
    processCell hArr wArr (hAcc, wAcc) (i, j) =
      let h = hArr ! (i, j)
          w = wArr ! (i, j)
          ns = neighbors4 maxR maxC i j
          -- Find lowest neighbor
          lowestN = case ns of
            [] -> (i, j)
            (first : rest) ->
              foldl' (\best nb -> if hArr ! nb < hArr ! best then nb else best) first rest
          hLow = hArr ! lowestN
          -- Step 3: Flow water to lowest neighbor
          canFlow = hLow < h && not (null ns)
          wFlow = if canFlow then w * waterFlowRate else 0
          wUpd =
            if canFlow
              then [((i, j), negate wFlow), (lowestN, wFlow)]
              else []
          -- Step 4: Erode proportional to water flow (not standing water)
          erodeAmt = erosionStrength * wFlow
          hErosion = [((i, j), negate erodeAmt) | canFlow]
          -- Step 5: Deposit in local minima
          isLocalMin = not (null ns) && all (\nb -> hArr ! nb >= h) ns
          hDeposit =
            [((i, j), erosionStrength * depositionFactor * w) | isLocalMin]
       in (hDeposit ++ hErosion ++ hAcc, wUpd ++ wAcc)

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Clamp an integer to a range.
clampI :: Int -> Int -> Int -> Int
clampI lo hi x = max lo (min hi x)

-- | Get 4-connected neighbor indices within grid bounds.
neighbors4 :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors4 maxR maxC i j =
  [ (ni, nj)
  | (di, dj) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
    let ni = i + di,
    let nj = j + dj,
    ni >= 0,
    ni <= maxR,
    nj >= 0,
    nj <= maxC
  ]

-- | Convert a nested list to an Array.
gridToArray :: [[Float]] -> Int -> Int -> Array (Int, Int) Float
gridToArray g r c =
  listArray ((0, 0), (r - 1, c - 1)) [h | row <- g, h <- row]

-- | Convert an Array back to a nested list.
arrayToGrid :: Int -> Int -> Array (Int, Int) Float -> [[Float]]
arrayToGrid r c arr =
  [[arr ! (i, j) | j <- [0 .. c - 1]] | i <- [0 .. r - 1]]

-- | Apply additive updates to an array in a single pass.
applyUpdates :: Array (Int, Int) Float -> [((Int, Int), Float)] -> Array (Int, Int) Float
applyUpdates = accum (+)

-- | Unique grid edges (each pair of adjacent cells appears once).
-- Only edges where @src < dst@ (lexicographic) are emitted.
uniqueEdges :: Int -> Int -> [((Int, Int), (Int, Int))]
uniqueEdges maxR maxC =
  [ ((i, j), (ni, nj))
  | i <- [0 .. maxR],
    j <- [0 .. maxC],
    (di, dj) <- [(1, 0), (0, 1)],
    let ni = i + di,
    let nj = j + dj,
    ni <= maxR,
    nj <= maxC
  ]

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Fraction of the height difference transferred per thermal erosion step.
thermalTransferRate :: Float
thermalTransferRate = 0.5

-- | Fraction of water remaining after evaporation each hydraulic step.
evaporationRate :: Float
evaporationRate = 0.9

-- | Fraction of standing water that flows to the lowest neighbor.
waterFlowRate :: Float
waterFlowRate = 0.5

-- | Fraction of erosion strength used for deposition in local minima.
depositionFactor :: Float
depositionFactor = 0.5
