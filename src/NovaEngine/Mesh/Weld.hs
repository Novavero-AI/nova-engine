-- | Vertex welding and mesh cleanup.
--
-- Merge vertices within an epsilon distance using spatial hashing
-- for O(n) expected time, and remove degenerate triangles.
module NovaEngine.Mesh.Weld
  ( -- * Welding
    weldVertices,

    -- * Cleanup
    removeDegenerateTriangles,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Minimum epsilon for welding. Prevents degenerate behavior
-- when epsilon is zero or negative.
minWeldEpsilon :: Float
minWeldEpsilon = 1.0e-10

-- | Number of neighbor offsets to check per axis (-1, 0, +1).
neighborOffsets :: [Int]
neighborOffsets = [-1, 0, 1]

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Merge vertices within epsilon distance, remapping all indices.
--
-- Uses spatial hashing with grid cells of size epsilon for O(n)
-- expected time. For each vertex, the 27 neighboring cells are
-- checked for an existing representative within epsilon distance.
-- If found, the vertex is mapped to that representative; otherwise
-- it becomes a new representative.
--
-- Epsilon is clamped to a minimum of 1e-10 to prevent degenerate
-- behavior.
weldVertices ::
  -- | Epsilon (merge distance)
  Float ->
  Mesh ->
  Mesh
weldVertices epsilonRaw (Mesh vertices indices _vertexCount) =
  let epsilon = max minWeldEpsilon epsilonRaw
      epsilonSq = epsilon * epsilon
      invCellSize = 1.0 / epsilon
      -- Process each vertex: assign to an existing representative
      -- or register as a new one
      (indexRemap, _grid, newVertsMap, newVertCount) =
        foldl'
          ( \(!remap, !grid, !accVerts, !nextIdx) (origIdx, vert) ->
              let pos = vPosition vert
                  cellCoord = positionToCell invCellSize pos
                  -- Search 27 neighboring cells for a match
                  matchResult =
                    findMatchInNeighbors
                      grid
                      accVerts
                      pos
                      epsilonSq
                      cellCoord
               in case matchResult of
                    Just existingIdx ->
                      -- Map this vertex to the existing representative
                      let newRemap = IntMap.insert origIdx existingIdx remap
                       in (newRemap, grid, accVerts, nextIdx)
                    Nothing ->
                      -- Register as a new representative
                      let newRemap = IntMap.insert origIdx nextIdx remap
                          newGrid = insertIntoGrid cellCoord nextIdx grid
                          newVerts = IntMap.insert nextIdx vert accVerts
                       in (newRemap, newGrid, newVerts, nextIdx + 1)
          )
          (IntMap.empty, Map.empty, IntMap.empty, 0)
          (zip [0 ..] vertices)
      -- Remap all indices
      remappedIndices = map (remapIndex indexRemap) indices
      -- Extract final vertex list in ascending key order
      finalVertices = IntMap.elems newVertsMap
   in Mesh finalVertices remappedIndices newVertCount

-- | Remove triangles where two or more vertex indices are the same.
-- This commonly occurs after welding when previously distinct vertices
-- are merged.
removeDegenerateTriangles :: Mesh -> Mesh
removeDegenerateTriangles (Mesh vertices indices vertexCount) =
  let filteredIndices = filterDegenerateTriangles indices
   in Mesh vertices filteredIndices vertexCount

-- ----------------------------------------------------------------
-- Spatial hashing
-- ----------------------------------------------------------------

-- | A spatial hash grid mapping cell coordinates to lists of vertex
-- indices that reside in that cell.
type SpatialGrid = Map (Int, Int, Int) [Int]

-- | Convert a position to a grid cell coordinate.
positionToCell :: Float -> V3 -> (Int, Int, Int)
positionToCell invCellSize (V3 px py pz) =
  ( fastFloor (px * invCellSize),
    fastFloor (py * invCellSize),
    fastFloor (pz * invCellSize)
  )

-- | Insert a vertex index into the spatial grid at the given cell.
insertIntoGrid :: (Int, Int, Int) -> Int -> SpatialGrid -> SpatialGrid
insertIntoGrid cell idx =
  Map.insertWith (++) cell [idx]

-- | Search the 27 neighboring cells for a vertex within epsilon
-- distance of the given position.
findMatchInNeighbors ::
  SpatialGrid ->
  IntMap Vertex ->
  V3 ->
  Float ->
  (Int, Int, Int) ->
  Maybe Int
findMatchInNeighbors grid vertMap queryPos epsilonSq (cx, cy, cz) =
  searchCells neighborCells
  where
    neighborCells =
      [ (cx + dx, cy + dy, cz + dz)
      | dx <- neighborOffsets,
        dy <- neighborOffsets,
        dz <- neighborOffsets
      ]

    searchCells [] = Nothing
    searchCells (cell : rest) =
      case Map.lookup cell grid of
        Nothing -> searchCells rest
        Just cellIndices ->
          case findMatchInCell cellIndices of
            Just matched -> Just matched
            Nothing -> searchCells rest

    findMatchInCell [] = Nothing
    findMatchInCell (vidx : rest) =
      let vertPos = lookupVertexPosition vertMap vidx
       in if distanceSq queryPos vertPos <= epsilonSq
            then Just vidx
            else findMatchInCell rest

-- | Look up a vertex position by index in the accumulated vertex map.
lookupVertexPosition :: IntMap Vertex -> Int -> V3
lookupVertexPosition vertMap targetIdx =
  case IntMap.lookup targetIdx vertMap of
    Just vert -> vPosition vert
    -- Sentinel far from any real geometry; ensures the distance
    -- check in the caller naturally rejects this non-existent vertex.
    Nothing -> V3 1e30 1e30 1e30

-- ----------------------------------------------------------------
-- Triangle filtering
-- ----------------------------------------------------------------

-- | Filter out degenerate triangles from an index list.
-- A triangle is degenerate if any two of its three indices are equal.
filterDegenerateTriangles :: [Word32] -> [Word32]
filterDegenerateTriangles = go
  where
    go (a : b : c : rest)
      | a == b || b == c || a == c = go rest
      | otherwise = a : b : c : go rest
    go _ = []

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Remap a single index through the weld mapping.
-- Falls back to the original index if not found (should not happen
-- with correct usage; the identity mapping is a safe default).
remapIndex :: IntMap Int -> Word32 -> Word32
remapIndex remap idx =
  maybe idx fromIntegral (IntMap.lookup (fromIntegral idx) remap)
