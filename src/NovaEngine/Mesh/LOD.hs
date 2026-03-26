-- | Level-of-detail chain generation.
--
-- Wraps "NovaEngine.Mesh.Simplify" to produce multiple mesh detail levels
-- for distance-based rendering. Supports explicit ratio lists,
-- automatic halving chains, and screen-size selection.
module NovaEngine.Mesh.LOD
  ( -- * LOD level
    LODLevel (..),

    -- * Chain generation
    generateLOD,
    generateLODChain,

    -- * Selection
    screenSizeLOD,
  )
where

import NovaEngine.Mesh.Simplify (simplify)
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- LOD level
-- ----------------------------------------------------------------

-- | A single level of detail: the simplified mesh, its triangle
-- count, and the ratio relative to the full-detail source.
data LODLevel = LODLevel
  { lodMesh :: Mesh,
    lodTriangleCount :: !Int,
    lodRatio :: !Float
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Chain generation
-- ----------------------------------------------------------------

-- | Generate LOD levels from explicit ratios.
--
-- The first element is always the full-detail mesh (ratio 1.0).
-- Subsequent levels are produced by calling 'simplify' for each
-- ratio in @(0, 1)@.  Ratios outside that range are filtered out.
generateLOD :: [Float] -> Mesh -> [LODLevel]
generateLOD ratios mesh =
  let totalTris = length (meshIndices mesh) `div` indicesPerTriangle
      fullLevel = LODLevel mesh totalTris 1.0
      clampedRatios = filter (\r -> r > 0 && r < 1.0) ratios
      levels =
        map
          ( \r ->
              let target = max 1 (round (fromIntegral totalTris * r))
                  simplified = simplify target mesh
                  actualTris = length (meshIndices simplified) `div` indicesPerTriangle
               in LODLevel simplified actualTris r
          )
          clampedRatios
   in fullLevel : levels

-- | Auto-generate a chain of @n@ LOD levels with halving ratios.
--
-- Level 0 is the full mesh. Level @i@ targets
-- @0.5 ^^ i@ of the original triangle count.  The level count is
-- clamped to a minimum of 1.
generateLODChain :: Int -> Mesh -> [LODLevel]
generateLODChain n = generateLOD ratios
  where
    levels = max 1 n
    ratios = [0.5 ^^ i | i <- [1 .. levels - 1 :: Int]]

-- ----------------------------------------------------------------
-- Selection
-- ----------------------------------------------------------------

-- | Select an LOD level based on screen-space size.
--
-- Picks the coarsest level whose @lodRatio * screenSize >=
-- threshold@. If no level meets the threshold the coarsest
-- available level is returned. Returns 'Nothing' only when the
-- chain is empty.
screenSizeLOD :: Float -> Float -> [LODLevel] -> Maybe LODLevel
screenSizeLOD _ _ [] = Nothing
screenSizeLOD screenSize threshold levels =
  case filter (\l -> lodRatio l * screenSize >= threshold) levels of
    [] -> safeLast levels
    suitable -> safeLast suitable

-- | Number of indices per triangle.
indicesPerTriangle :: Int
indicesPerTriangle = 3
