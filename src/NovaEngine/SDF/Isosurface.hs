-- | Implicit surface to mesh conversion.
--
-- Marching cubes for extracting triangle meshes from signed distance
-- fields. Samples an SDF on a regular 3D grid and produces an indexed
-- triangle mesh with normals, UVs, and tangents.
module NovaEngine.SDF.Isosurface
  ( -- * Marching cubes
    marchingCubes,
  )
where

import Data.Array (Array, listArray, (!))
import Data.Bits (shiftL, testBit)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Word (Word16)
import NovaEngine.Mesh.Types
import NovaEngine.SDF (interpolateEdge, sdfGradient, triplanarTangent, triplanarUV)

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Number of corners in a marching cubes voxel.
cornerCount :: Int
cornerCount = 8

-- ----------------------------------------------------------------
-- Marching cubes
-- ----------------------------------------------------------------

-- | Extract a triangle mesh from a signed distance field using the
-- marching cubes algorithm.
--
-- The SDF should return negative values inside the surface and
-- positive values outside. The grid spans from @minCorner@ to
-- @maxCorner@ with the given resolution in each axis.
marchingCubes ::
  -- | Signed distance function
  (V3 -> Float) ->
  -- | Grid minimum corner
  V3 ->
  -- | Grid maximum corner
  V3 ->
  -- | Resolution in X
  Int ->
  -- | Resolution in Y
  Int ->
  -- | Resolution in Z
  Int ->
  Mesh
marchingCubes sdf minCorner maxCorner resX resY resZ =
  let rx = max 1 resX
      ry = max 1 resY
      rz = max 1 resZ
      V3 x0 y0 z0 = minCorner
      V3 x1 y1 z1 = maxCorner
      stepX = (x1 - x0) / fromIntegral rx
      stepY = (y1 - y0) / fromIntegral ry
      stepZ = (z1 - z0) / fromIntegral rz

      -- Pre-sample SDF at all grid vertices for O(1) lookup per corner.
      sdfCache =
        listArray
          (0, (rx + 1) * (ry + 1) * (rz + 1) - 1)
          [ sdf
              ( V3
                  (x0 + fromIntegral vx * stepX)
                  (y0 + fromIntegral vy * stepY)
                  (z0 + fromIntegral vz * stepZ)
              )
          | vz <- [0 .. rz],
            vy <- [0 .. ry],
            vx <- [0 .. rx]
          ]

      -- Process all cubes, accumulating vertices, indices, and the
      -- edge-vertex deduplication map.
      cubeCoords =
        [ (ix, iy, iz)
        | ix <- [0 .. rx - 1],
          iy <- [0 .. ry - 1],
          iz <- [0 .. rz - 1]
        ]

      initialState = MarchState [] [] Map.empty 0

      finalState = foldl' (processCube sdf sdfCache x0 y0 z0 stepX stepY stepZ rx ry) initialState cubeCoords
   in mkMesh (reverse (msVertices finalState)) (reverse (msIndices finalState))

-- | Mutable state threaded through the cube processing fold.
data MarchState = MarchState
  { msVertices :: [Vertex],
    msIndices :: [Word32],
    msEdgeMap :: Map.Map (Int, Int) Int,
    msVertexCount :: Int
  }

-- | Process a single cube in the marching cubes grid.
processCube ::
  (V3 -> Float) ->
  Array Int Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Int ->
  Int ->
  MarchState ->
  (Int, Int, Int) ->
  MarchState
processCube sdf sdfCache x0 y0 z0 stepX stepY stepZ rx ry state (ix, iy, iz) =
  let -- Compute corner positions and look up cached SDF values
      cornerPos idx =
        let (dx, dy, dz) = cornerOffset idx
         in V3
              (x0 + (fromIntegral ix + dx) * stepX)
              (y0 + (fromIntegral iy + dy) * stepY)
              (z0 + (fromIntegral iz + dz) * stepZ)

      cornerVal idx =
        let (dxi, dyi, dzi) = cornerOffsetInt idx
         in sdfCache ! gridVertexIndex (ix + dxi) (iy + dyi) (iz + dzi) rx ry

      cornerList = map cornerPos [0 .. 7]
      valueList = map cornerVal [0 .. 7]
      corners = listArray (0, 7) cornerList :: Array Int V3
      values = listArray (0, 7) valueList :: Array Int Float

      -- Build the cube index from corner signs
      cubeIndex = buildCubeIndex valueList

      -- Look up which edges are crossed
      edgeBits = edgeTableLookup cubeIndex

      -- Get the triangle list for this configuration
      triList = triTableLookup cubeIndex
   in if edgeBits == 0
        then state
        else
          let -- For each active edge, compute or look up the interpolated vertex
              activeEdges = filter (edgeBitSet edgeBits) [0 .. 11]

              -- Process each active edge
              (stateWithEdges, edgeVertMap) =
                foldl'
                  ( \(st, evm) edgeIdx ->
                      let (ca, cb) = edgeEndpoints edgeIdx
                          -- Global vertex IDs for deduplication
                          globalA = globalCornerIndex ix iy iz ca rx ry
                          globalB = globalCornerIndex ix iy iz cb rx ry
                          edgeKey =
                            if globalA <= globalB
                              then (globalA, globalB)
                              else (globalB, globalA)
                       in case Map.lookup edgeKey (msEdgeMap st) of
                            Just vertIdx ->
                              (st, Map.insert edgeIdx vertIdx evm)
                            Nothing ->
                              let posA = corners ! ca
                                  posB = corners ! cb
                                  valA = values ! ca
                                  valB = values ! cb
                                  interpPos = interpolateEdge posA posB valA valB
                                  normal = sdfGradient sdf interpPos
                                  uv = triplanarUV interpPos normal
                                  tangent = triplanarTangent normal
                                  vert = vertex interpPos normal uv tangent
                                  vertIdx = msVertexCount st
                                  newState =
                                    st
                                      { msVertices = vert : msVertices st,
                                        msEdgeMap = Map.insert edgeKey vertIdx (msEdgeMap st),
                                        msVertexCount = vertIdx + 1
                                      }
                               in (newState, Map.insert edgeIdx vertIdx evm)
                  )
                  (state, Map.empty :: Map.Map Int Int)
                  activeEdges

              -- Emit triangles from the triangle table
              finalState = emitTriangles stateWithEdges edgeVertMap triList
           in finalState

-- | Emit triangles from the triangle table entry, looking up vertex
-- indices from the edge-to-vertex map.
emitTriangles :: MarchState -> Map.Map Int Int -> [Int] -> MarchState
emitTriangles state _ [] = state
emitTriangles state _ [_] = state
emitTriangles state _ [_, _] = state
emitTriangles state edgeVertMap (ea : eb : ec : rest) =
  case (Map.lookup ea edgeVertMap, Map.lookup eb edgeVertMap, Map.lookup ec edgeVertMap) of
    (Just va, Just vb, Just vc) ->
      let newState =
            state
              { msIndices =
                  fromIntegral va
                    : fromIntegral vb
                    : fromIntegral vc
                    : msIndices state
              }
       in emitTriangles newState edgeVertMap rest
    _ -> emitTriangles state edgeVertMap rest

-- ----------------------------------------------------------------
-- Cube geometry helpers
-- ----------------------------------------------------------------

-- | Offset from the base corner for each of the 8 cube corners.
cornerOffset :: Int -> (Float, Float, Float)
cornerOffset 0 = (0, 0, 0)
cornerOffset 1 = (1, 0, 0)
cornerOffset 2 = (1, 1, 0)
cornerOffset 3 = (0, 1, 0)
cornerOffset 4 = (0, 0, 1)
cornerOffset 5 = (1, 0, 1)
cornerOffset 6 = (1, 1, 1)
cornerOffset 7 = (0, 1, 1)
cornerOffset _ = (0, 0, 0)

-- | The two corner indices for each of the 12 edges.
edgeEndpoints :: Int -> (Int, Int)
edgeEndpoints 0 = (0, 1)
edgeEndpoints 1 = (1, 2)
edgeEndpoints 2 = (2, 3)
edgeEndpoints 3 = (3, 0)
edgeEndpoints 4 = (4, 5)
edgeEndpoints 5 = (5, 6)
edgeEndpoints 6 = (6, 7)
edgeEndpoints 7 = (7, 4)
edgeEndpoints 8 = (0, 4)
edgeEndpoints 9 = (1, 5)
edgeEndpoints 10 = (2, 6)
edgeEndpoints 11 = (3, 7)
edgeEndpoints _ = (0, 0)

-- | Build the 8-bit cube index from corner SDF values.
-- A corner is inside the surface if its value is negative.
buildCubeIndex :: [Float] -> Int
buildCubeIndex vals =
  foldl'
    (\acc (idx, val) -> if val < 0 then acc + shiftL 1 idx else acc)
    0
    (zip [0 :: Int ..] (take cornerCount vals))

-- | Check if an edge bit is set in the edge bitmask.
edgeBitSet :: Word16 -> Int -> Bool
edgeBitSet = testBit

-- | Compute a global corner index for edge deduplication.
-- Maps (cubeX, cubeY, cubeZ, cornerIdx) to a unique integer.
globalCornerIndex :: Int -> Int -> Int -> Int -> Int -> Int -> Int
globalCornerIndex ix iy iz corner rx ry =
  let (dx, dy, dz) = cornerOffsetInt corner
      gx = ix + dx
      gy = iy + dy
      gz = iz + dz
   in gx + gy * (rx + 1) + gz * (rx + 1) * (ry + 1)

-- | Flatten a 3D grid vertex coordinate to a unique integer key
-- for the SDF cache array.
gridVertexIndex :: Int -> Int -> Int -> Int -> Int -> Int
gridVertexIndex vx vy vz rx ry =
  vx + vy * (rx + 1) + vz * (rx + 1) * (ry + 1)

-- | Integer offsets for corners (matching cornerOffset).
cornerOffsetInt :: Int -> (Int, Int, Int)
cornerOffsetInt 0 = (0, 0, 0)
cornerOffsetInt 1 = (1, 0, 0)
cornerOffsetInt 2 = (1, 1, 0)
cornerOffsetInt 3 = (0, 1, 0)
cornerOffsetInt 4 = (0, 0, 1)
cornerOffsetInt 5 = (1, 0, 1)
cornerOffsetInt 6 = (1, 1, 1)
cornerOffsetInt 7 = (0, 1, 1)
cornerOffsetInt _ = (0, 0, 0)

-- ----------------------------------------------------------------
-- Lookup table access
-- ----------------------------------------------------------------

-- | Look up the edge bitmask for a cube configuration.
edgeTableLookup :: Int -> Word16
edgeTableLookup idx = edgeTableArr ! idx

-- | Look up the triangle list for a cube configuration.
triTableLookup :: Int -> [Int]
triTableLookup idx = triTableArr ! idx

-- | Edge table stored as an Array for O(1) lookup.
edgeTableArr :: Array Int Word16
edgeTableArr = listArray (0, 255) edgeTable

-- | Triangle table stored as an Array for O(1) lookup.
triTableArr :: Array Int [Int]
triTableArr = listArray (0, 255) triTable

-- ----------------------------------------------------------------
-- Marching cubes edge table
-- ----------------------------------------------------------------

-- | Standard marching cubes edge table. For each of the 256 possible
-- cube configurations, stores a 12-bit mask indicating which edges
-- are intersected by the isosurface.
edgeTable :: [Word16]
edgeTable =
  [ 0x000,
    0x109,
    0x203,
    0x30A,
    0x406,
    0x50F,
    0x605,
    0x70C,
    0x80C,
    0x905,
    0xA0F,
    0xB06,
    0xC0A,
    0xD03,
    0xE09,
    0xF00,
    0x190,
    0x099,
    0x393,
    0x29A,
    0x596,
    0x49F,
    0x795,
    0x69C,
    0x99C,
    0x895,
    0xB9F,
    0xA96,
    0xD9A,
    0xC93,
    0xF99,
    0xE90,
    0x230,
    0x339,
    0x033,
    0x13A,
    0x636,
    0x73F,
    0x435,
    0x53C,
    0xA3C,
    0xB35,
    0x83F,
    0x936,
    0xE3A,
    0xF33,
    0xC39,
    0xD30,
    0x3A0,
    0x2A9,
    0x1A3,
    0x0AA,
    0x7A6,
    0x6AF,
    0x5A5,
    0x4AC,
    0xBAC,
    0xAA5,
    0x9AF,
    0x8A6,
    0xFAA,
    0xEA3,
    0xDA9,
    0xCA0,
    0x460,
    0x569,
    0x663,
    0x76A,
    0x066,
    0x16F,
    0x265,
    0x36C,
    0xC6C,
    0xD65,
    0xE6F,
    0xF66,
    0x86A,
    0x963,
    0xA69,
    0xB60,
    0x5F0,
    0x4F9,
    0x7F3,
    0x6FA,
    0x1F6,
    0x0FF,
    0x3F5,
    0x2FC,
    0xDFC,
    0xCF5,
    0xFFF,
    0xEF6,
    0x9FA,
    0x8F3,
    0xBF9,
    0xAF0,
    0x650,
    0x759,
    0x453,
    0x55A,
    0x256,
    0x35F,
    0x055,
    0x15C,
    0xE5C,
    0xF55,
    0xC5F,
    0xD56,
    0xA5A,
    0xB53,
    0x859,
    0x950,
    0x7C0,
    0x6C9,
    0x5C3,
    0x4CA,
    0x3C6,
    0x2CF,
    0x1C5,
    0x0CC,
    0xFCC,
    0xEC5,
    0xDCF,
    0xCC6,
    0xBCA,
    0xAC3,
    0x9C9,
    0x8C0,
    0x8C0,
    0x9C9,
    0xAC3,
    0xBCA,
    0xCC6,
    0xDCF,
    0xEC5,
    0xFCC,
    0x0CC,
    0x1C5,
    0x2CF,
    0x3C6,
    0x4CA,
    0x5C3,
    0x6C9,
    0x7C0,
    0x950,
    0x859,
    0xB53,
    0xA5A,
    0xD56,
    0xC5F,
    0xF55,
    0xE5C,
    0x15C,
    0x055,
    0x35F,
    0x256,
    0x55A,
    0x453,
    0x759,
    0x650,
    0xAF0,
    0xBF9,
    0x8F3,
    0x9FA,
    0xEF6,
    0xFFF,
    0xCF5,
    0xDFC,
    0x2FC,
    0x3F5,
    0x0FF,
    0x1F6,
    0x6FA,
    0x7F3,
    0x4F9,
    0x5F0,
    0xB60,
    0xA69,
    0x963,
    0x86A,
    0xF66,
    0xE6F,
    0xD65,
    0xC6C,
    0x36C,
    0x265,
    0x16F,
    0x066,
    0x76A,
    0x663,
    0x569,
    0x460,
    0xCA0,
    0xDA9,
    0xEA3,
    0xFAA,
    0x8A6,
    0x9AF,
    0xAA5,
    0xBAC,
    0x4AC,
    0x5A5,
    0x6AF,
    0x7A6,
    0x0AA,
    0x1A3,
    0x2A9,
    0x3A0,
    0xD30,
    0xC39,
    0xF33,
    0xE3A,
    0x936,
    0x83F,
    0xB35,
    0xA3C,
    0x53C,
    0x435,
    0x73F,
    0x636,
    0x13A,
    0x033,
    0x339,
    0x230,
    0xE90,
    0xF99,
    0xC93,
    0xD9A,
    0xA96,
    0xB9F,
    0x895,
    0x99C,
    0x69C,
    0x795,
    0x49F,
    0x596,
    0x29A,
    0x393,
    0x099,
    0x190,
    0xF00,
    0xE09,
    0xD03,
    0xC0A,
    0xB06,
    0xA0F,
    0x905,
    0x80C,
    0x70C,
    0x605,
    0x50F,
    0x406,
    0x30A,
    0x203,
    0x109,
    0x000
  ]

-- ----------------------------------------------------------------
-- Marching cubes triangle table
-- ----------------------------------------------------------------

-- | Standard marching cubes triangle table. For each of the 256
-- possible cube configurations, lists the edges that form triangles.
-- Each group of 3 entries defines one triangle. Edge indices refer
-- to the 12 edges of the cube (0-11).
triTable :: [[Int]]
triTable =
  [ [],
    [0, 8, 3],
    [0, 1, 9],
    [1, 8, 3, 9, 8, 1],
    [1, 2, 10],
    [0, 8, 3, 1, 2, 10],
    [9, 2, 10, 0, 2, 9],
    [2, 8, 3, 2, 10, 8, 10, 9, 8],
    [3, 11, 2],
    [0, 11, 2, 8, 11, 0],
    [1, 9, 0, 2, 3, 11],
    [1, 11, 2, 1, 9, 11, 9, 8, 11],
    [3, 10, 1, 11, 10, 3],
    [0, 10, 1, 0, 8, 10, 8, 11, 10],
    [3, 9, 0, 3, 11, 9, 11, 10, 9],
    [9, 8, 10, 10, 8, 11],
    [4, 7, 8],
    [4, 3, 0, 7, 3, 4],
    [0, 1, 9, 8, 4, 7],
    [4, 1, 9, 4, 7, 1, 7, 3, 1],
    [1, 2, 10, 8, 4, 7],
    [3, 4, 7, 3, 0, 4, 1, 2, 10],
    [9, 2, 10, 9, 0, 2, 8, 4, 7],
    [2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4],
    [8, 4, 7, 3, 11, 2],
    [11, 4, 7, 11, 2, 4, 2, 0, 4],
    [9, 0, 1, 8, 4, 7, 2, 3, 11],
    [4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1],
    [3, 10, 1, 3, 11, 10, 7, 8, 4],
    [1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4],
    [4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3],
    [4, 7, 11, 4, 11, 9, 9, 11, 10],
    [9, 5, 4],
    [9, 5, 4, 0, 8, 3],
    [0, 5, 4, 1, 5, 0],
    [8, 5, 4, 8, 3, 5, 3, 1, 5],
    [1, 2, 10, 9, 5, 4],
    [3, 0, 8, 1, 2, 10, 4, 9, 5],
    [5, 2, 10, 5, 4, 2, 4, 0, 2],
    [2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8],
    [9, 5, 4, 2, 3, 11],
    [0, 11, 2, 0, 8, 11, 4, 9, 5],
    [0, 5, 4, 0, 1, 5, 2, 3, 11],
    [2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5],
    [10, 3, 11, 10, 1, 3, 9, 5, 4],
    [4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10],
    [5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3],
    [5, 4, 8, 5, 8, 10, 10, 8, 11],
    [9, 7, 8, 5, 7, 9],
    [9, 3, 0, 9, 5, 3, 5, 7, 3],
    [0, 7, 8, 0, 1, 7, 1, 5, 7],
    [1, 5, 3, 3, 5, 7],
    [9, 7, 8, 9, 5, 7, 10, 1, 2],
    [10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3],
    [8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2],
    [2, 10, 5, 2, 5, 3, 3, 5, 7],
    [7, 9, 5, 7, 8, 9, 3, 11, 2],
    [9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11],
    [2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7],
    [11, 2, 1, 11, 1, 7, 7, 1, 5],
    [9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11],
    [5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0],
    [11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0],
    [11, 10, 5, 7, 11, 5],
    [10, 6, 5],
    [0, 8, 3, 5, 10, 6],
    [9, 0, 1, 5, 10, 6],
    [1, 8, 3, 1, 9, 8, 5, 10, 6],
    [1, 6, 5, 2, 6, 1],
    [1, 6, 5, 1, 2, 6, 3, 0, 8],
    [9, 6, 5, 9, 0, 6, 0, 2, 6],
    [5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8],
    [2, 3, 11, 10, 6, 5],
    [11, 0, 8, 11, 2, 0, 10, 6, 5],
    [0, 1, 9, 2, 3, 11, 5, 10, 6],
    [5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11],
    [6, 3, 11, 6, 5, 3, 5, 1, 3],
    [0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6],
    [3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9],
    [6, 5, 9, 6, 9, 11, 11, 9, 8],
    [5, 10, 6, 4, 7, 8],
    [4, 3, 0, 4, 7, 3, 6, 5, 10],
    [1, 9, 0, 5, 10, 6, 8, 4, 7],
    [10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4],
    [6, 1, 2, 6, 5, 1, 4, 7, 8],
    [1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7],
    [8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6],
    [7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9],
    [3, 11, 2, 7, 8, 4, 10, 6, 5],
    [5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11],
    [0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6],
    [9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6],
    [8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6],
    [5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11],
    [0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7],
    [6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9],
    [10, 4, 9, 6, 4, 10],
    [4, 10, 6, 4, 9, 10, 0, 8, 3],
    [10, 0, 1, 10, 6, 0, 6, 4, 0],
    [8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10],
    [1, 4, 9, 1, 2, 4, 2, 6, 4],
    [3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4],
    [0, 2, 4, 4, 2, 6],
    [8, 3, 2, 8, 2, 4, 4, 2, 6],
    [10, 4, 9, 10, 6, 4, 11, 2, 3],
    [0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6],
    [3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10],
    [6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1],
    [9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3],
    [8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1],
    [3, 11, 6, 3, 6, 0, 0, 6, 4],
    [6, 4, 8, 11, 6, 8],
    [7, 10, 6, 7, 8, 10, 8, 9, 10],
    [0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10],
    [10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0],
    [10, 6, 7, 10, 7, 1, 1, 7, 3],
    [1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7],
    [2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9],
    [7, 8, 0, 7, 0, 6, 6, 0, 2],
    [7, 3, 2, 6, 7, 2],
    [2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7],
    [2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7],
    [1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11],
    [11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1],
    [8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6],
    [0, 9, 1, 11, 6, 7],
    [7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0],
    [7, 11, 6],
    [7, 6, 11],
    [3, 0, 8, 11, 7, 6],
    [0, 1, 9, 11, 7, 6],
    [8, 1, 9, 8, 3, 1, 11, 7, 6],
    [10, 1, 2, 6, 11, 7],
    [1, 2, 10, 3, 0, 8, 6, 11, 7],
    [2, 9, 0, 2, 10, 9, 6, 11, 7],
    [6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8],
    [7, 2, 3, 6, 2, 7],
    [7, 0, 8, 7, 6, 0, 6, 2, 0],
    [2, 7, 6, 2, 3, 7, 0, 1, 9],
    [1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6],
    [10, 7, 6, 10, 1, 7, 1, 3, 7],
    [10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8],
    [0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7],
    [7, 6, 10, 7, 10, 8, 8, 10, 9],
    [6, 8, 4, 11, 8, 6],
    [3, 6, 11, 3, 0, 6, 0, 4, 6],
    [8, 6, 11, 8, 4, 6, 9, 0, 1],
    [9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6],
    [6, 8, 4, 6, 11, 8, 2, 10, 1],
    [1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6],
    [4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9],
    [10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3],
    [8, 2, 3, 8, 4, 2, 4, 6, 2],
    [0, 4, 2, 4, 6, 2],
    [1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8],
    [1, 9, 4, 1, 4, 2, 2, 4, 6],
    [8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1],
    [10, 1, 0, 10, 0, 6, 6, 0, 4],
    [4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3],
    [10, 9, 4, 6, 10, 4],
    [4, 9, 5, 7, 6, 11],
    [0, 8, 3, 4, 9, 5, 11, 7, 6],
    [5, 0, 1, 5, 4, 0, 7, 6, 11],
    [11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5],
    [9, 5, 4, 10, 1, 2, 7, 6, 11],
    [6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5],
    [7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2],
    [3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6],
    [7, 2, 3, 7, 6, 2, 5, 4, 9],
    [9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7],
    [3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0],
    [6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8],
    [9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7],
    [1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4],
    [4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10],
    [7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10],
    [6, 9, 5, 6, 11, 9, 11, 8, 9],
    [3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5],
    [0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11],
    [6, 11, 3, 6, 3, 5, 5, 3, 1],
    [1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6],
    [0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10],
    [11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5],
    [6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3],
    [5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2],
    [9, 5, 6, 9, 6, 0, 0, 6, 2],
    [1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8],
    [1, 5, 6, 2, 1, 6],
    [1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6],
    [10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0],
    [0, 3, 8, 5, 6, 10],
    [10, 5, 6],
    [11, 5, 10, 7, 5, 11],
    [11, 5, 10, 11, 7, 5, 8, 3, 0],
    [5, 11, 7, 5, 10, 11, 1, 9, 0],
    [10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1],
    [11, 1, 2, 11, 7, 1, 7, 5, 1],
    [0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11],
    [9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7],
    [7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2],
    [2, 5, 10, 2, 3, 5, 3, 7, 5],
    [8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5],
    [9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2],
    [9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2],
    [1, 3, 5, 3, 7, 5],
    [0, 8, 7, 0, 7, 1, 1, 7, 5],
    [9, 0, 3, 9, 3, 5, 5, 3, 7],
    [9, 8, 7, 5, 9, 7],
    [5, 8, 4, 5, 10, 8, 10, 11, 8],
    [5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0],
    [0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5],
    [10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4],
    [2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8],
    [0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11],
    [0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5],
    [9, 4, 5, 2, 11, 3],
    [2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4],
    [5, 10, 2, 5, 2, 4, 4, 2, 0],
    [3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9],
    [5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2],
    [8, 4, 5, 8, 5, 3, 3, 5, 1],
    [0, 4, 5, 1, 0, 5],
    [8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5],
    [9, 4, 5],
    [4, 11, 7, 4, 9, 11, 9, 10, 11],
    [0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11],
    [1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11],
    [3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4],
    [4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2],
    [9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3],
    [11, 7, 4, 11, 4, 2, 2, 4, 0],
    [11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4],
    [2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9],
    [9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7],
    [3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10],
    [1, 10, 2, 8, 7, 4],
    [4, 9, 1, 4, 1, 7, 7, 1, 3],
    [4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1],
    [4, 0, 3, 7, 4, 3],
    [4, 8, 7],
    [9, 10, 8, 10, 11, 8],
    [3, 0, 9, 3, 9, 11, 11, 9, 10],
    [0, 1, 10, 0, 10, 8, 8, 10, 11],
    [3, 1, 10, 11, 3, 10],
    [1, 2, 11, 1, 11, 9, 9, 11, 8],
    [3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9],
    [0, 2, 11, 8, 0, 11],
    [3, 2, 11],
    [2, 3, 8, 2, 8, 10, 10, 8, 9],
    [9, 10, 2, 0, 9, 2],
    [2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8],
    [1, 10, 2],
    [1, 3, 8, 9, 1, 8],
    [0, 9, 1],
    [0, 3, 8],
    []
  ]
