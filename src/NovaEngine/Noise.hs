-- | Pure noise functions for displacement and procedural textures.
--
-- Improved Perlin (2D, 3D), simplex (2D, 3D), Worley\/cellular,
-- FBM, ridged multifractal, turbulence, and domain warping.
-- Seed-derived permutation tables via splitmix PRNG.
module NovaEngine.Noise
  ( -- * Configuration
    NoiseConfig,
    mkNoiseConfig,

    -- * Improved Perlin noise
    perlin2D,
    perlin3D,

    -- * Simplex noise
    simplex2D,
    simplex3D,

    -- * 4D simplex noise
    simplex4D,

    -- * Worley (cellular) noise
    WorleyResult (..),
    worley2D,
    worley3D,

    -- * Fractal combinators
    fbm,
    fbm3D,
    ridged,
    turbulence,
    defaultLacunarity,
    defaultPersistence,

    -- * Domain warping
    domainWarp2D,
  )
where

import Data.Array (Array, listArray, (!))
import Data.Bits (shiftR, xor, (.&.))
import Data.List (foldl', sortBy)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import NovaEngine.Mesh.Types (fastFloor, lerpFloat)

-- ----------------------------------------------------------------
-- Splitmix PRNG
-- ----------------------------------------------------------------

-- | Minimal splitmix PRNG state: a single counter.
newtype SplitMix = SplitMix Word64

-- | The golden-ratio-derived increment for splitmix.
splitmixGamma :: Word64
splitmixGamma = 0x9E3779B97F4A7C15

-- | Advance the PRNG state and produce a mixed 64-bit output.
nextSplitMix :: SplitMix -> (Word64, SplitMix)
nextSplitMix (SplitMix state) =
  let stateNext = state + splitmixGamma
      mixed = mixBits stateNext
   in (mixed, SplitMix stateNext)

-- | Three rounds of xor-shift + multiply to mix bits.
mixBits :: Word64 -> Word64
mixBits val =
  let step1 = (val `xor` (val `shiftR` 30)) * 0xBF58476D1CE4E5B9
      step2 = (step1 `xor` (step1 `shiftR` 27)) * 0x94D049BB133111EB
      step3 = step2 `xor` (step2 `shiftR` 31)
   in step3

-- ----------------------------------------------------------------
-- Permutation table
-- ----------------------------------------------------------------

-- | Immutable noise configuration derived from a seed.
-- Contains a permutation table stored as an 'Array' for O(1) lookup.
newtype NoiseConfig = NoiseConfig
  { noisePermTable :: Array Int Int
  }

-- | Number of entries in the base permutation table.
permTableSize :: Int
permTableSize = 256

-- | Build a 'NoiseConfig' from a 64-bit seed.
-- Generates a 256-entry permutation via Fisher-Yates shuffle driven
-- by splitmix, then doubles it to 512 entries for index wrapping.
mkNoiseConfig :: Word64 -> NoiseConfig
mkNoiseConfig seed =
  let rng = SplitMix seed
      baseList = [0 .. permTableSize - 1]
      shuffled = fisherYatesShuffle rng baseList
      doubled = shuffled ++ shuffled
      permSize = length doubled
      table = listArray (0, permSize - 1) doubled
   in NoiseConfig {noisePermTable = table}

-- | Pure Fisher-Yates shuffle using splitmix for randomness.
-- O(n²) for list-based selection, but n = 256 so negligible.
fisherYatesShuffle :: SplitMix -> [Int] -> [Int]
fisherYatesShuffle _ [] = []
fisherYatesShuffle rng items =
  let len = length items
      (val, rngNext) = nextSplitMix rng
      idx = fromIntegral (val `mod` fromIntegral len)
      (picked, remaining) = removeAt idx items
   in picked : fisherYatesShuffle rngNext remaining

-- | Remove element at index from a list, returning the element
-- and the remaining list. Uses safe indexing with modular wrapping.
removeAt :: Int -> [Int] -> (Int, [Int])
removeAt idx xs =
  let safeIdx = idx `mod` max 1 (length xs)
      (before, after) = splitAt safeIdx xs
   in case after of
        (element : rest) -> (element, before ++ rest)
        [] -> case xs of
          (fallback : rest) -> (fallback, rest)
          [] -> (0, [])

-- | Look up a value in the permutation table.
-- Uses O(1) array indexing with bitmask wrapping.
permLookup :: NoiseConfig -> Int -> Int
permLookup config idx =
  noisePermTable config ! (idx .&. permMask)

-- | Bitmask for wrapping permutation table indices to [0, 511].
permMask :: Int
permMask = 511

-- ----------------------------------------------------------------
-- Fade curve and interpolation
-- ----------------------------------------------------------------

-- | C2 fade curve: 6t^5 - 15t^4 + 10t^3.
-- Unique degree-5 polynomial with f(0)=0, f(1)=1 and
-- vanishing first and second derivatives at endpoints.
fade :: Float -> Float
fade t = t * t * t * (t * (t * fadeCoeffA - fadeCoeffB) + fadeCoeffC)

-- | Coefficient for t^5 term in fade curve.
fadeCoeffA :: Float
fadeCoeffA = 6.0

-- | Coefficient for t^4 term in fade curve.
fadeCoeffB :: Float
fadeCoeffB = 15.0

-- | Coefficient for t^3 term in fade curve.
fadeCoeffC :: Float
fadeCoeffC = 10.0

-- ----------------------------------------------------------------
-- Gradient vectors
-- ----------------------------------------------------------------

-- | Select one of 12 edge gradient vectors for 3D Perlin noise.
-- Midpoints of cube edges: {(±1,±1,0), (±1,0,±1), (0,±1,±1)}.
-- Returns the dot product with the given offset vector directly.
--
-- Uses @mod 12@ rather than a bitmask because 12 is not a power of
-- two — a bitmask would skip gradient directions and introduce
-- directional bias.
grad3D :: Int -> Float -> Float -> Float -> Float
grad3D hash xOff yOff zOff =
  case hash `mod` 12 of
    0 -> xOff + yOff
    1 -> negate xOff + yOff
    2 -> xOff - yOff
    3 -> negate xOff - yOff
    4 -> xOff + zOff
    5 -> negate xOff + zOff
    6 -> xOff - zOff
    7 -> negate xOff - zOff
    8 -> yOff + zOff
    9 -> negate yOff + zOff
    10 -> yOff - zOff
    _ -> negate yOff - zOff

-- | Select one of 4 gradient vectors for 2D Perlin noise.
-- The four diagonals: {(±1,±1)}.
grad2D :: Int -> Float -> Float -> Float
grad2D hash xOff yOff =
  case hash .&. gradMask2D of
    0 -> xOff + yOff
    1 -> negate xOff + yOff
    2 -> xOff - yOff
    _ -> negate xOff - yOff

-- | Mask for selecting among 4 gradient directions in 2D.
gradMask2D :: Int
gradMask2D = 3

-- ----------------------------------------------------------------
-- Improved Perlin noise
-- ----------------------------------------------------------------

-- | 2D improved Perlin noise. Output approximately in [-1, 1].
perlin2D :: NoiseConfig -> Float -> Float -> Float
perlin2D config px py =
  let xi = fastFloor px
      yi = fastFloor py
      xf = px - fromIntegral xi
      yf = py - fromIntegral yi
      xFaded = fade xf
      yFaded = fade yf
      -- Hash corners
      aa = permLookup config (permLookup config xi + yi)
      ab = permLookup config (permLookup config xi + yi + 1)
      ba = permLookup config (permLookup config (xi + 1) + yi)
      bb = permLookup config (permLookup config (xi + 1) + yi + 1)
      -- Gradient dot products at each corner
      g00 = grad2D aa xf yf
      g10 = grad2D ba (xf - 1.0) yf
      g01 = grad2D ab xf (yf - 1.0)
      g11 = grad2D bb (xf - 1.0) (yf - 1.0)
      -- Bilinear interpolation with faded coordinates
      xLerp0 = lerpFloat xFaded g00 g10
      xLerp1 = lerpFloat xFaded g01 g11
   in lerpFloat yFaded xLerp0 xLerp1

-- | 3D improved Perlin noise. Output approximately in [-1, 1].
perlin3D :: NoiseConfig -> Float -> Float -> Float -> Float
perlin3D config px py pz =
  let xi = fastFloor px
      yi = fastFloor py
      zi = fastFloor pz
      xf = px - fromIntegral xi
      yf = py - fromIntegral yi
      zf = pz - fromIntegral zi
      xFaded = fade xf
      yFaded = fade yf
      zFaded = fade zf
      -- Hash the 8 corners of the unit cube
      permX0 = permLookup config xi
      permX1 = permLookup config (xi + 1)
      permX0Y0 = permLookup config (permX0 + yi)
      permX0Y1 = permLookup config (permX0 + yi + 1)
      permX1Y0 = permLookup config (permX1 + yi)
      permX1Y1 = permLookup config (permX1 + yi + 1)
      -- Gradient dot products at each corner
      g000 = grad3D (permLookup config (permX0Y0 + zi)) xf yf zf
      g100 = grad3D (permLookup config (permX1Y0 + zi)) (xf - 1.0) yf zf
      g010 = grad3D (permLookup config (permX0Y1 + zi)) xf (yf - 1.0) zf
      g110 = grad3D (permLookup config (permX1Y1 + zi)) (xf - 1.0) (yf - 1.0) zf
      g001 = grad3D (permLookup config (permX0Y0 + zi + 1)) xf yf (zf - 1.0)
      g101 = grad3D (permLookup config (permX1Y0 + zi + 1)) (xf - 1.0) yf (zf - 1.0)
      g011 = grad3D (permLookup config (permX0Y1 + zi + 1)) xf (yf - 1.0) (zf - 1.0)
      g111 = grad3D (permLookup config (permX1Y1 + zi + 1)) (xf - 1.0) (yf - 1.0) (zf - 1.0)
      -- Trilinear interpolation using faded coordinates
      xLerp00 = lerpFloat xFaded g000 g100
      xLerp10 = lerpFloat xFaded g010 g110
      xLerp01 = lerpFloat xFaded g001 g101
      xLerp11 = lerpFloat xFaded g011 g111
      yLerp0 = lerpFloat yFaded xLerp00 xLerp10
      yLerp1 = lerpFloat yFaded xLerp01 xLerp11
   in lerpFloat zFaded yLerp0 yLerp1

-- ----------------------------------------------------------------
-- Simplex noise
-- ----------------------------------------------------------------

-- | 2D skew factor: (sqrt(3) - 1) / 2.
skewFactor2D :: Float
skewFactor2D = 0.3660254037844386

-- | 2D unskew factor: (3 - sqrt(3)) / 6.
unskewFactor2D :: Float
unskewFactor2D = 0.21132486540518713

-- | Radial falloff radius squared for 2D simplex noise.
simplexRadius2D :: Float
simplexRadius2D = 0.5

-- | Scaling factor for 2D simplex noise to bring output to [-1, 1].
simplexScale2D :: Float
simplexScale2D = 70.0

-- | 2D simplex noise. Output approximately in [-1, 1].
simplex2D :: NoiseConfig -> Float -> Float -> Float
simplex2D config px py =
  let -- Skew input space to simplex space
      skew = (px + py) * skewFactor2D
      xi = fastFloor (px + skew)
      yi = fastFloor (py + skew)
      -- Unskew back to input space
      unskew = fromIntegral (xi + yi) * unskewFactor2D
      x0 = px - (fromIntegral xi - unskew)
      y0 = py - (fromIntegral yi - unskew)
      -- Determine which simplex we are in
      (i1, j1) =
        if x0 > y0
          then (1, 0) -- lower triangle
          else (0, 1) -- upper triangle
          -- Offsets for second and third corners
      x1 = x0 - fromIntegral i1 + unskewFactor2D
      y1 = y0 - fromIntegral j1 + unskewFactor2D
      x2 = x0 - 1.0 + 2.0 * unskewFactor2D
      y2 = y0 - 1.0 + 2.0 * unskewFactor2D
      -- Hash the corners
      gi0 = permLookup config (permLookup config xi + yi)
      gi1 = permLookup config (permLookup config (xi + i1) + yi + j1)
      gi2 = permLookup config (permLookup config (xi + 1) + yi + 1)
      -- Radial kernel contributions
      contrib0 = simplexContrib2D gi0 x0 y0
      contrib1 = simplexContrib2D gi1 x1 y1
      contrib2 = simplexContrib2D gi2 x2 y2
   in simplexScale2D * (contrib0 + contrib1 + contrib2)

-- | Compute a single simplex corner contribution in 2D.
simplexContrib2D :: Int -> Float -> Float -> Float
simplexContrib2D gi xOff yOff =
  let t = simplexRadius2D - xOff * xOff - yOff * yOff
   in if t < 0
        then 0.0
        else
          let tSquared = t * t
           in tSquared * tSquared * grad2D gi xOff yOff

-- | 3D skew factor: 1/3.
skewFactor3D :: Float
skewFactor3D = 1.0 / 3.0

-- | 3D unskew factor: 1/6.
unskewFactor3D :: Float
unskewFactor3D = 1.0 / 6.0

-- | Radial falloff radius squared for 3D simplex noise.
simplexRadius3D :: Float
simplexRadius3D = 0.6

-- | Scaling factor for 3D simplex noise to bring output to [-1, 1].
simplexScale3D :: Float
simplexScale3D = 32.0

-- | 3D simplex noise. Output approximately in [-1, 1].
simplex3D :: NoiseConfig -> Float -> Float -> Float -> Float
simplex3D config px py pz =
  let -- Skew input space to simplex space
      skew = (px + py + pz) * skewFactor3D
      xi = fastFloor (px + skew)
      yi = fastFloor (py + skew)
      zi = fastFloor (pz + skew)
      -- Unskew back to input space
      unskew = fromIntegral (xi + yi + zi) * unskewFactor3D
      x0 = px - (fromIntegral xi - unskew)
      y0 = py - (fromIntegral yi - unskew)
      z0 = pz - (fromIntegral zi - unskew)
      -- Determine simplex traversal order by sorting offsets
      offsets = sortBy (comparing (Down . snd)) [(0 :: Int, x0), (1, y0), (2, z0)]
      (i1, j1, k1, i2, j2, k2) = simplexOffsets3D offsets
      -- Offsets for corners 1-3
      x1 = x0 - fromIntegral i1 + unskewFactor3D
      y1 = y0 - fromIntegral j1 + unskewFactor3D
      z1 = z0 - fromIntegral k1 + unskewFactor3D
      x2 = x0 - fromIntegral i2 + 2.0 * unskewFactor3D
      y2 = y0 - fromIntegral j2 + 2.0 * unskewFactor3D
      z2 = z0 - fromIntegral k2 + 2.0 * unskewFactor3D
      x3 = x0 - 1.0 + 3.0 * unskewFactor3D
      y3 = y0 - 1.0 + 3.0 * unskewFactor3D
      z3 = z0 - 1.0 + 3.0 * unskewFactor3D
      -- Hash corners
      gi0 = permLookup config (permLookup config (permLookup config xi + yi) + zi)
      gi1 = permLookup config (permLookup config (permLookup config (xi + i1) + yi + j1) + zi + k1)
      gi2 = permLookup config (permLookup config (permLookup config (xi + i2) + yi + j2) + zi + k2)
      gi3 = permLookup config (permLookup config (permLookup config (xi + 1) + yi + 1) + zi + 1)
      -- Radial kernel contributions
      contrib0 = simplexContrib3D gi0 x0 y0 z0
      contrib1 = simplexContrib3D gi1 x1 y1 z1
      contrib2 = simplexContrib3D gi2 x2 y2 z2
      contrib3 = simplexContrib3D gi3 x3 y3 z3
   in simplexScale3D * (contrib0 + contrib1 + contrib2 + contrib3)

-- | Determine simplex traversal offsets from sorted coordinate indices.
-- The first step goes along the axis with the largest offset,
-- the second step adds the next largest.
simplexOffsets3D :: [(Int, Float)] -> (Int, Int, Int, Int, Int, Int)
simplexOffsets3D sorted =
  let axis0 = fst (safeIndexList sorted 0)
      axis1 = fst (safeIndexList sorted 1)
      -- First intermediate vertex: step along largest axis
      i1 = if axis0 == 0 then 1 else 0
      j1 = if axis0 == 1 then 1 else 0
      k1 = if axis0 == 2 then 1 else 0
      -- Second intermediate vertex: step along two largest axes
      i2 = if axis0 == 0 || axis1 == 0 then 1 else 0
      j2 = if axis0 == 1 || axis1 == 1 then 1 else 0
      k2 = if axis0 == 2 || axis1 == 2 then 1 else 0
   in (i1, j1, k1, i2, j2, k2)

-- | Compute a single simplex corner contribution in 3D.
simplexContrib3D :: Int -> Float -> Float -> Float -> Float
simplexContrib3D gi xOff yOff zOff =
  let t = simplexRadius3D - xOff * xOff - yOff * yOff - zOff * zOff
   in if t < 0
        then 0.0
        else
          let tSquared = t * t
           in tSquared * tSquared * grad3D gi xOff yOff zOff

-- ----------------------------------------------------------------
-- Worley (cellular) noise
-- ----------------------------------------------------------------

-- | Result of Worley noise evaluation: distances to the nearest
-- and second nearest feature points.
data WorleyResult = WorleyResult
  { worleyF1 :: !Float,
    worleyF2 :: !Float
  }
  deriving (Show, Eq)

-- | Initial large distance for Worley distance comparisons.
worleyMaxDistance :: Float
worleyMaxDistance = 1.0e30

-- | 2D Worley noise using Euclidean distance.
-- Searches a 3x3 neighborhood of cells for the nearest
-- and second nearest feature points.
worley2D :: NoiseConfig -> Float -> Float -> WorleyResult
worley2D config px py =
  let cellX = fastFloor px
      cellY = fastFloor py
      initialResult = WorleyResult worleyMaxDistance worleyMaxDistance
      neighborOffsets = [-1, 0, 1]
      result =
        foldl'
          ( \acc dx ->
              foldl'
                ( \innerAcc dy ->
                    let neighborX = cellX + dx
                        neighborY = cellY + dy
                        -- Deterministic jittered feature point
                        hash = worleyHash2D config neighborX neighborY
                        featureX = fromIntegral neighborX + hashToFloat hash
                        featureY = fromIntegral neighborY + hashToFloat (hash * worleyHashMult + worleyHashOffset)
                        distSq = (px - featureX) * (px - featureX) + (py - featureY) * (py - featureY)
                     in insertDistance innerAcc distSq
                )
                acc
                neighborOffsets
          )
          initialResult
          neighborOffsets
   in WorleyResult (sqrt (worleyF1 result)) (sqrt (worleyF2 result))

-- | 3D Worley noise using Euclidean distance.
-- Searches a 3x3x3 = 27 cell neighborhood.
worley3D :: NoiseConfig -> Float -> Float -> Float -> WorleyResult
worley3D config px py pz =
  let cellX = fastFloor px
      cellY = fastFloor py
      cellZ = fastFloor pz
      initialResult = WorleyResult worleyMaxDistance worleyMaxDistance
      neighborOffsets = [-1, 0, 1]
      result =
        foldl'
          ( \acc dx ->
              foldl'
                ( \innerAcc dy ->
                    foldl'
                      ( \deepAcc dz ->
                          let neighborX = cellX + dx
                              neighborY = cellY + dy
                              neighborZ = cellZ + dz
                              hash = worleyHash3D config neighborX neighborY neighborZ
                              featureX = fromIntegral neighborX + hashToFloat hash
                              featureY = fromIntegral neighborY + hashToFloat (hash * worleyHashMult + worleyHashOffset)
                              featureZ = fromIntegral neighborZ + hashToFloat (hash * worleyHashMult2 + worleyHashOffset2)
                              distX = px - featureX
                              distY = py - featureY
                              distZ = pz - featureZ
                              distSq = distX * distX + distY * distY + distZ * distZ
                           in insertDistance deepAcc distSq
                      )
                      innerAcc
                      neighborOffsets
                )
                acc
                neighborOffsets
          )
          initialResult
          neighborOffsets
   in WorleyResult (sqrt (worleyF1 result)) (sqrt (worleyF2 result))

-- | Hash multiplier for decorrelating Worley feature point axes.
worleyHashMult :: Int
worleyHashMult = 127

-- | Hash offset for decorrelating Worley feature point axes.
worleyHashOffset :: Int
worleyHashOffset = 31

-- | Second hash multiplier for the Z axis in 3D Worley.
worleyHashMult2 :: Int
worleyHashMult2 = 269

-- | Second hash offset for the Z axis in 3D Worley.
worleyHashOffset2 :: Int
worleyHashOffset2 = 53

-- | Hash 2D cell coordinates through the permutation table.
worleyHash2D :: NoiseConfig -> Int -> Int -> Int
worleyHash2D config cx cy =
  permLookup config (permLookup config cx + cy)

-- | Hash 3D cell coordinates through the permutation table.
worleyHash3D :: NoiseConfig -> Int -> Int -> Int -> Int
worleyHash3D config cx cy cz =
  permLookup config (permLookup config (permLookup config cx + cy) + cz)

-- | Convert a hash value to a float in [0, 1).
hashToFloat :: Int -> Float
hashToFloat h = fromIntegral (h .&. hashToFloatMask) / hashToFloatDivisor

-- | Bitmask for hash-to-float conversion (8 bits of precision).
hashToFloatMask :: Int
hashToFloatMask = 255

-- | Divisor for hash-to-float conversion.
hashToFloatDivisor :: Float
hashToFloatDivisor = 256.0

-- | Insert a squared distance into a Worley result, maintaining
-- F1 <= F2 ordering.
insertDistance :: WorleyResult -> Float -> WorleyResult
insertDistance (WorleyResult f1 f2) distSq
  | distSq < f1 = WorleyResult distSq f1
  | distSq < f2 = WorleyResult f1 distSq
  | otherwise = WorleyResult f1 f2

-- ----------------------------------------------------------------
-- Fractal combinators
-- ----------------------------------------------------------------

-- | Default lacunarity: frequency multiplier per octave.
defaultLacunarity :: Float
defaultLacunarity = 2.0

-- | Default persistence: amplitude multiplier per octave.
defaultPersistence :: Float
defaultPersistence = 0.5

-- | Ridged multifractal offset constant.
ridgedOffset :: Float
ridgedOffset = 1.0

-- | Minimum octave count for fractal combinators.
minOctaves :: Int
minOctaves = 1

-- | Fractal Brownian Motion for 2D noise.
-- Layers octaves at increasing frequency and decreasing amplitude.
-- The result is normalized by the sum of amplitudes.
fbm ::
  -- | Base 2D noise function
  (Float -> Float -> Float) ->
  -- | Number of octaves
  Int ->
  -- | Lacunarity (frequency multiplier)
  Float ->
  -- | Persistence (amplitude multiplier)
  Float ->
  -- | X coordinate
  Float ->
  -- | Y coordinate
  Float ->
  Float
fbm noiseFn octaves lacunarity persistence px py =
  let clampedOctaves = max minOctaves octaves
      (totalValue, totalAmplitude, _, _) =
        foldl'
          ( \(!val, !amp, !freq, !ampMul) _ ->
              let noiseVal = noiseFn (px * freq) (py * freq)
               in ( val + noiseVal * ampMul,
                    amp + ampMul,
                    freq * lacunarity,
                    ampMul * persistence
                  )
          )
          (0.0, 0.0, 1.0, 1.0)
          [1 .. clampedOctaves]
   in if totalAmplitude > 0 then totalValue / totalAmplitude else 0.0

-- | Fractal Brownian Motion for 3D noise.
-- Layers octaves at increasing frequency and decreasing amplitude.
-- The result is normalized by the sum of amplitudes.
fbm3D ::
  -- | Base 3D noise function
  (Float -> Float -> Float -> Float) ->
  -- | Number of octaves
  Int ->
  -- | Lacunarity (frequency multiplier)
  Float ->
  -- | Persistence (amplitude multiplier)
  Float ->
  -- | X coordinate
  Float ->
  -- | Y coordinate
  Float ->
  -- | Z coordinate
  Float ->
  Float
fbm3D noiseFn octaves lacunarity persistence px py pz =
  let clampedOctaves = max minOctaves octaves
      (totalValue, totalAmplitude, _, _) =
        foldl'
          ( \(!val, !amp, !freq, !ampMul) _ ->
              let noiseVal = noiseFn (px * freq) (py * freq) (pz * freq)
               in ( val + noiseVal * ampMul,
                    amp + ampMul,
                    freq * lacunarity,
                    ampMul * persistence
                  )
          )
          (0.0, 0.0, 1.0, 1.0)
          [1 .. clampedOctaves]
   in if totalAmplitude > 0 then totalValue / totalAmplitude else 0.0

-- | Ridged multifractal noise for 2D.
-- Each octave computes @(offset - |noise|)^2@, weighted by the
-- previous octave's output to concentrate detail on ridges.
ridged ::
  -- | Base 2D noise function
  (Float -> Float -> Float) ->
  -- | Number of octaves
  Int ->
  -- | Lacunarity (frequency multiplier)
  Float ->
  -- | Persistence (amplitude multiplier)
  Float ->
  -- | X coordinate
  Float ->
  -- | Y coordinate
  Float ->
  Float
ridged noiseFn octaves lacunarity persistence px py =
  let clampedOctaves = max minOctaves octaves
      (totalValue, _, _, _) =
        foldl'
          ( \(!val, !weight, !freq, !ampMul) _ ->
              let noiseVal = noiseFn (px * freq) (py * freq)
                  signal = ridgedOffset - abs noiseVal
                  signalSquared = signal * signal
                  clampedWeight = min 1.0 (max 0.0 weight)
                  weighted = signalSquared * clampedWeight
               in ( val + weighted * ampMul,
                    signalSquared,
                    freq * lacunarity,
                    ampMul * persistence
                  )
          )
          (0.0, 1.0, 1.0, 1.0)
          [1 .. clampedOctaves]
   in totalValue

-- | Turbulence: FBM with absolute value per octave.
-- Always produces positive output.
turbulence ::
  -- | Base 2D noise function
  (Float -> Float -> Float) ->
  -- | Number of octaves
  Int ->
  -- | Lacunarity (frequency multiplier)
  Float ->
  -- | Persistence (amplitude multiplier)
  Float ->
  -- | X coordinate
  Float ->
  -- | Y coordinate
  Float ->
  Float
turbulence noiseFn octaves lacunarity persistence px py =
  let clampedOctaves = max minOctaves octaves
      (totalValue, totalAmplitude, _, _) =
        foldl'
          ( \(!val, !amp, !freq, !ampMul) _ ->
              let noiseVal = abs (noiseFn (px * freq) (py * freq))
               in ( val + noiseVal * ampMul,
                    amp + ampMul,
                    freq * lacunarity,
                    ampMul * persistence
                  )
          )
          (0.0, 0.0, 1.0, 1.0)
          [1 .. clampedOctaves]
   in if totalAmplitude > 0 then totalValue / totalAmplitude else 0.0

-- ----------------------------------------------------------------
-- Domain warping
-- ----------------------------------------------------------------

-- | Decorrelation offset for domain warp X channel.
warpOffsetX :: Float
warpOffsetX = 5.2

-- | Decorrelation offset for domain warp Y channel.
warpOffsetY :: Float
warpOffsetY = 1.3

-- | 2D domain warp: displaces input coordinates by noise-derived
-- offsets before evaluating the base noise function.
domainWarp2D ::
  -- | Base 2D noise function
  (Float -> Float -> Float) ->
  -- | Warp amplitude
  Float ->
  -- | X coordinate
  Float ->
  -- | Y coordinate
  Float ->
  Float
domainWarp2D noiseFn amplitude px py =
  let warpX = noiseFn (px + warpOffsetX) (py + warpOffsetY)
      warpY = noiseFn (px + warpOffsetY) (py + warpOffsetX)
   in noiseFn (px + amplitude * warpX) (py + amplitude * warpY)

-- ----------------------------------------------------------------
-- Utilities
-- ----------------------------------------------------------------

-- | Safe list indexing: returns the element at the given index,
-- or a default value if out of bounds.
safeIndexList :: [(Int, Float)] -> Int -> (Int, Float)
safeIndexList xs idx = case drop idx xs of
  (element : _) -> element
  [] -> (0, 0.0)

-- ----------------------------------------------------------------
-- 4D Simplex noise
-- ----------------------------------------------------------------

-- | 4D skew factor: (sqrt(5) - 1) / 4.
skewFactor4D :: Float
skewFactor4D = (sqrt 5.0 - 1.0) / 4.0

-- | 4D unskew factor: (5 - sqrt(5)) / 20.
unskewFactor4D :: Float
unskewFactor4D = (5.0 - sqrt 5.0) / 20.0

-- | Radial falloff radius for 4D simplex noise.
simplexRadius4D :: Float
simplexRadius4D = 0.6

-- | Scaling factor for 4D simplex noise to bring output to [-1, 1].
simplexScale4D :: Float
simplexScale4D = 27.0

-- | Number of gradient directions in 4D.
gradCount4D :: Int
gradCount4D = 32

-- | 4D simplex noise. Output approximately in [-1, 1].
--
-- Uses the simplex algorithm in 4D: skew to a hypercubic lattice,
-- determine which of the 24 simplices the point falls in by sorting
-- coordinate offsets, then sum radial-falloff-weighted gradient
-- contributions from the 5 simplex corners.
simplex4D :: NoiseConfig -> Float -> Float -> Float -> Float -> Float
simplex4D config px py pz pw =
  let skew = (px + py + pz + pw) * skewFactor4D
      xi = fastFloor (px + skew)
      yi = fastFloor (py + skew)
      zi = fastFloor (pz + skew)
      wi = fastFloor (pw + skew)
      unskew = fromIntegral (xi + yi + zi + wi) * unskewFactor4D
      x0 = px - (fromIntegral xi - unskew)
      y0 = py - (fromIntegral yi - unskew)
      z0 = pz - (fromIntegral zi - unskew)
      w0 = pw - (fromIntegral wi - unskew)
      sorted =
        sortBy
          (comparing (Down . snd))
          [(0 :: Int, x0), (1, y0), (2, z0), (3, w0)]
      (i1, j1, k1, l1, i2, j2, k2, l2, i3, j3, k3, l3) =
        simplexOffsets4D sorted
      x1 = x0 - fromIntegral i1 + unskewFactor4D
      y1 = y0 - fromIntegral j1 + unskewFactor4D
      z1 = z0 - fromIntegral k1 + unskewFactor4D
      w1 = w0 - fromIntegral l1 + unskewFactor4D
      x2 = x0 - fromIntegral i2 + 2.0 * unskewFactor4D
      y2 = y0 - fromIntegral j2 + 2.0 * unskewFactor4D
      z2 = z0 - fromIntegral k2 + 2.0 * unskewFactor4D
      w2 = w0 - fromIntegral l2 + 2.0 * unskewFactor4D
      x3 = x0 - fromIntegral i3 + 3.0 * unskewFactor4D
      y3 = y0 - fromIntegral j3 + 3.0 * unskewFactor4D
      z3 = z0 - fromIntegral k3 + 3.0 * unskewFactor4D
      w3 = w0 - fromIntegral l3 + 3.0 * unskewFactor4D
      x4 = x0 - 1.0 + 4.0 * unskewFactor4D
      y4 = y0 - 1.0 + 4.0 * unskewFactor4D
      z4 = z0 - 1.0 + 4.0 * unskewFactor4D
      w4 = w0 - 1.0 + 4.0 * unskewFactor4D
      gi0 =
        permLookup
          config
          (permLookup config (permLookup config (permLookup config xi + yi) + zi) + wi)
      gi1 =
        permLookup
          config
          (permLookup config (permLookup config (permLookup config (xi + i1) + yi + j1) + zi + k1) + wi + l1)
      gi2 =
        permLookup
          config
          (permLookup config (permLookup config (permLookup config (xi + i2) + yi + j2) + zi + k2) + wi + l2)
      gi3 =
        permLookup
          config
          (permLookup config (permLookup config (permLookup config (xi + i3) + yi + j3) + zi + k3) + wi + l3)
      gi4 =
        permLookup
          config
          (permLookup config (permLookup config (permLookup config (xi + 1) + yi + 1) + zi + 1) + wi + 1)
      contrib0 = simplexContrib4D gi0 x0 y0 z0 w0
      contrib1 = simplexContrib4D gi1 x1 y1 z1 w1
      contrib2 = simplexContrib4D gi2 x2 y2 z2 w2
      contrib3 = simplexContrib4D gi3 x3 y3 z3 w3
      contrib4 = simplexContrib4D gi4 x4 y4 z4 w4
   in simplexScale4D * (contrib0 + contrib1 + contrib2 + contrib3 + contrib4)

-- | Determine simplex traversal offsets from sorted 4D coordinate indices.
simplexOffsets4D ::
  [(Int, Float)] ->
  (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
simplexOffsets4D sorted =
  let axis0 = fst (safeIndexList sorted 0)
      axis1 = fst (safeIndexList sorted 1)
      axis2 = fst (safeIndexList sorted 2)
      i1 = if axis0 == 0 then 1 else 0
      j1 = if axis0 == 1 then 1 else 0
      k1 = if axis0 == 2 then 1 else 0
      l1 = if axis0 == 3 then 1 else 0
      i2 = if axis0 == 0 || axis1 == 0 then 1 else 0
      j2 = if axis0 == 1 || axis1 == 1 then 1 else 0
      k2 = if axis0 == 2 || axis1 == 2 then 1 else 0
      l2 = if axis0 == 3 || axis1 == 3 then 1 else 0
      i3 = if axis0 == 0 || axis1 == 0 || axis2 == 0 then 1 else 0
      j3 = if axis0 == 1 || axis1 == 1 || axis2 == 1 then 1 else 0
      k3 = if axis0 == 2 || axis1 == 2 || axis2 == 2 then 1 else 0
      l3 = if axis0 == 3 || axis1 == 3 || axis2 == 3 then 1 else 0
   in (i1, j1, k1, l1, i2, j2, k2, l2, i3, j3, k3, l3)

-- | Compute a single simplex corner contribution in 4D.
simplexContrib4D :: Int -> Float -> Float -> Float -> Float -> Float
simplexContrib4D gi xOff yOff zOff wOff =
  let t = simplexRadius4D - xOff * xOff - yOff * yOff - zOff * zOff - wOff * wOff
   in if t < 0
        then 0.0
        else
          let tSquared = t * t
           in tSquared * tSquared * grad4D gi xOff yOff zOff wOff

-- | Select one of 32 gradient directions in 4D and compute the dot
-- product with the given offset vector.
grad4D :: Int -> Float -> Float -> Float -> Float -> Float
grad4D hash xOff yOff zOff wOff =
  case hash `mod` gradCount4D of
    0 -> xOff + yOff + zOff
    1 -> xOff + yOff - zOff
    2 -> xOff - yOff + zOff
    3 -> xOff - yOff - zOff
    4 -> negate xOff + yOff + zOff
    5 -> negate xOff + yOff - zOff
    6 -> negate xOff - yOff + zOff
    7 -> negate xOff - yOff - zOff
    8 -> xOff + yOff + wOff
    9 -> xOff + yOff - wOff
    10 -> xOff - yOff + wOff
    11 -> xOff - yOff - wOff
    12 -> negate xOff + yOff + wOff
    13 -> negate xOff + yOff - wOff
    14 -> negate xOff - yOff + wOff
    15 -> negate xOff - yOff - wOff
    16 -> xOff + zOff + wOff
    17 -> xOff + zOff - wOff
    18 -> xOff - zOff + wOff
    19 -> xOff - zOff - wOff
    20 -> negate xOff + zOff + wOff
    21 -> negate xOff + zOff - wOff
    22 -> negate xOff - zOff + wOff
    23 -> negate xOff - zOff - wOff
    24 -> yOff + zOff + wOff
    25 -> yOff + zOff - wOff
    26 -> yOff - zOff + wOff
    27 -> yOff - zOff - wOff
    28 -> negate yOff + zOff + wOff
    29 -> negate yOff + zOff - wOff
    30 -> negate yOff - zOff + wOff
    _ -> negate yOff - zOff - wOff
