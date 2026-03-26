-- | Dual contouring with sharp feature preservation.
--
-- Extracts triangle meshes from signed distance fields using dual
-- contouring. Unlike marching cubes, dual contouring places vertices
-- inside cells at positions that minimize error relative to the
-- Hermite data (intersection points and normals), preserving sharp
-- edges and corners in the output mesh.
module NovaEngine.SDF.DualContour
  ( -- * Dual contouring
    dualContour,
  )
where

import Data.Array (Array, listArray, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.List (foldl', sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import NovaEngine.Mesh.Types
import NovaEngine.SDF (interpolateEdge, sdfGradient, triplanarTangent, triplanarUV)

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Regularization weight for the QEF solver. Biases the solution
-- toward the mass point to prevent vertices from drifting too far
-- outside their cell.
qefRegularization :: Float
qefRegularization = 0.01

-- | Minimum denominator used in linear interpolation to avoid
-- division by near-zero values.
interpolationMinDenom :: Float
interpolationMinDenom = 1.0e-10

-- | Minimum squared length for a normal to be considered valid
-- during QEF accumulation.
minNormalLengthSq :: Float
minNormalLengthSq = 1.0e-12

-- ----------------------------------------------------------------
-- Edge axis identifiers
-- ----------------------------------------------------------------

-- | Edge along the X axis.
edgeAxisX :: Int
edgeAxisX = 0

-- | Edge along the Y axis.
edgeAxisY :: Int
edgeAxisY = 1

-- | Edge along the Z axis.
edgeAxisZ :: Int
edgeAxisZ = 2

-- ----------------------------------------------------------------
-- Dual contouring
-- ----------------------------------------------------------------

-- | Extract a triangle mesh from a signed distance field using dual
-- contouring.
--
-- The SDF should return negative values inside the surface and
-- positive values outside. The grid spans from @minCorner@ to
-- @maxCorner@ with the given resolution in each axis.
--
-- Dual contouring produces one vertex per active cell (a cell with
-- at least one sign-changing edge). Quads are emitted for each
-- internal edge shared by four cells that crosses the isosurface,
-- then split into two triangles.
dualContour ::
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
dualContour sdf minCorner maxCorner resX resY resZ =
  let rx = max 1 resX
      ry = max 1 resY
      rz = max 1 resZ
      V3 x0 y0 z0 = minCorner
      V3 x1 y1 z1 = maxCorner
      stepX = (x1 - x0) / fromIntegral rx
      stepY = (y1 - y0) / fromIntegral ry
      stepZ = (z1 - z0) / fromIntegral rz
      gridDims = GridDims rx ry rz

      -- Step 1: Sample SDF at all grid vertices.
      sdfValues = sampleGrid sdf x0 y0 z0 stepX stepY stepZ rx ry rz

      -- Step 2: Find Hermite data for all sign-changing edges.
      hermiteData =
        findHermiteEdges
          sdf
          sdfValues
          x0
          y0
          z0
          stepX
          stepY
          stepZ
          gridDims

      -- Step 3: Solve QEF for each active cell to place the vertex.
      cellVertexMap =
        solveCellVertices
          hermiteData
          x0
          y0
          z0
          stepX
          stepY
          stepZ
          gridDims

      -- Step 4: Emit quads for sign-changing edges shared by 4 cells.
      (vertices, indices) =
        emitQuads
          sdfValues
          cellVertexMap
          gridDims
   in mkMesh vertices indices

-- ----------------------------------------------------------------
-- Grid dimensions record
-- ----------------------------------------------------------------

-- | Grid resolution along each axis.
data GridDims = GridDims
  { gdResX :: Int,
    gdResY :: Int,
    gdResZ :: Int
  }

-- ----------------------------------------------------------------
-- SDF sampling
-- ----------------------------------------------------------------

-- | Sample the SDF at every vertex of the grid. Returns an Array
-- indexed by flattened vertex index for O(1) lookup.
sampleGrid ::
  (V3 -> Float) ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Int ->
  Int ->
  Int ->
  Array Int Float
sampleGrid sdf x0 y0 z0 stepX stepY stepZ rx ry rz =
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

-- | Flatten a 3D vertex coordinate to a unique integer key.
vertexIndex :: Int -> Int -> Int -> Int -> Int -> Int
vertexIndex vx vy vz rx ry =
  vx + vy * (rx + 1) + vz * (rx + 1) * (ry + 1)

-- | Look up the SDF value at a grid vertex using O(1) array indexing.
lookupSDF :: Array Int Float -> Int -> Int -> Int -> Int -> Int -> Float
lookupSDF sdfValues vx vy vz rx ry =
  sdfValues ! vertexIndex vx vy vz rx ry

-- ----------------------------------------------------------------
-- Hermite data (edge intersections)
-- ----------------------------------------------------------------

-- | Intersection point and surface normal for a sign-changing edge.
-- Fields are positional to avoid unused-selector warnings.
data HermitePoint = HermitePoint V3 V3

-- | Find all sign-changing edges and compute their Hermite data.
-- Returns a map from (edgeKey) to HermitePoint.
-- Edge keys encode the minimal vertex of the edge plus the axis.
findHermiteEdges ::
  (V3 -> Float) ->
  Array Int Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  GridDims ->
  Map.Map (Int, Int) HermitePoint
findHermiteEdges sdf sdfValues x0 y0 z0 stepX stepY stepZ dims =
  foldl' processVertex Map.empty allVertices
  where
    rx = gdResX dims
    ry = gdResY dims
    rz = gdResZ dims

    allVertices =
      [ (vx, vy, vz)
      | vx <- [0 .. rx],
        vy <- [0 .. ry],
        vz <- [0 .. rz]
      ]

    processVertex !acc (vx, vy, vz) =
      let valHere = lookupSDF sdfValues vx vy vz rx ry
          baseKey = vertexIndex vx vy vz rx ry
          posHere =
            V3
              (x0 + fromIntegral vx * stepX)
              (y0 + fromIntegral vy * stepY)
              (z0 + fromIntegral vz * stepZ)

          -- Check X-edge: (vx,vy,vz) -> (vx+1,vy,vz)
          accAfterX
            | vx < rx =
                let valNeighbor = lookupSDF sdfValues (vx + 1) vy vz rx ry
                 in if signChange valHere valNeighbor
                      then
                        let posNeighbor =
                              V3
                                (x0 + fromIntegral (vx + 1) * stepX)
                                (y0 + fromIntegral vy * stepY)
                                (z0 + fromIntegral vz * stepZ)
                            crossPt = interpolateEdge posHere posNeighbor valHere valNeighbor
                            normal = sdfGradient sdf crossPt
                         in Map.insert
                              (baseKey, edgeAxisX)
                              (HermitePoint crossPt normal)
                              acc
                      else acc
            | otherwise = acc

          -- Check Y-edge: (vx,vy,vz) -> (vx,vy+1,vz)
          accAfterY
            | vy < ry =
                let valNeighbor = lookupSDF sdfValues vx (vy + 1) vz rx ry
                 in if signChange valHere valNeighbor
                      then
                        let posNeighbor =
                              V3
                                (x0 + fromIntegral vx * stepX)
                                (y0 + fromIntegral (vy + 1) * stepY)
                                (z0 + fromIntegral vz * stepZ)
                            crossPt = interpolateEdge posHere posNeighbor valHere valNeighbor
                            normal = sdfGradient sdf crossPt
                         in Map.insert
                              (baseKey, edgeAxisY)
                              (HermitePoint crossPt normal)
                              accAfterX
                      else accAfterX
            | otherwise = accAfterX

          -- Check Z-edge: (vx,vy,vz) -> (vx,vy,vz+1)
          accAfterZ
            | vz < rz =
                let valNeighbor = lookupSDF sdfValues vx vy (vz + 1) rx ry
                 in if signChange valHere valNeighbor
                      then
                        let posNeighbor =
                              V3
                                (x0 + fromIntegral vx * stepX)
                                (y0 + fromIntegral vy * stepY)
                                (z0 + fromIntegral (vz + 1) * stepZ)
                            crossPt = interpolateEdge posHere posNeighbor valHere valNeighbor
                            normal = sdfGradient sdf crossPt
                         in Map.insert
                              (baseKey, edgeAxisZ)
                              (HermitePoint crossPt normal)
                              accAfterY
                      else accAfterY
            | otherwise = accAfterY
       in accAfterZ

-- | Test whether two SDF values have opposite signs (a sign change
-- across the edge).
signChange :: Float -> Float -> Bool
signChange valA valB = (valA < 0) /= (valB < 0)

-- ----------------------------------------------------------------
-- QEF solver and cell vertex placement
-- ----------------------------------------------------------------

-- | Accumulated data for the QEF: normals and intersection points
-- for all sign-changing edges in a cell.
data QEFData = QEFData [V3] [V3]

-- | For each active cell (one with at least one sign-changing edge),
-- solve the QEF and store the resulting vertex index and vertex.
-- Returns a map from cell key to (vertex index, Vertex).
solveCellVertices ::
  Map.Map (Int, Int) HermitePoint ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  GridDims ->
  IntMap.IntMap (Int, Vertex)
solveCellVertices hermiteData x0 y0 z0 stepX stepY stepZ dims =
  let rx = gdResX dims
      ry = gdResY dims
      rz = gdResZ dims

      -- All cells: a cell at (cx,cy,cz) spans vertices
      -- (cx,cy,cz) to (cx+1,cy+1,cz+1).
      cellCoords =
        [ (cx, cy, cz)
        | cx <- [0 .. rx - 1],
          cy <- [0 .. ry - 1],
          cz <- [0 .. rz - 1]
        ]

      (_, resultMap) =
        foldl' processCell (0 :: Int, IntMap.empty) cellCoords

      processCell (!nextIdx, !acc) (cx, cy, cz) =
        let cellKey = cellIndex cx cy cz rx ry
            qefData = gatherCellHermiteData hermiteData cx cy cz rx ry
         in case qefData of
              QEFData [] _ -> (nextIdx, acc)
              QEFData _ [] -> (nextIdx, acc)
              QEFData normals points ->
                let massPoint = computeMassPoint points
                    cellMin =
                      V3
                        (x0 + fromIntegral cx * stepX)
                        (y0 + fromIntegral cy * stepY)
                        (z0 + fromIntegral cz * stepZ)
                    cellMax =
                      V3
                        (x0 + fromIntegral (cx + 1) * stepX)
                        (y0 + fromIntegral (cy + 1) * stepY)
                        (z0 + fromIntegral (cz + 1) * stepZ)
                    solvedPos =
                      clampToCell cellMin cellMax $
                        solveQEF normals points massPoint
                    normal = computeAverageNormal normals
                    uv = triplanarUV solvedPos normal
                    tangent = triplanarTangent normal
                    vert = vertex solvedPos normal uv tangent
                 in (nextIdx + 1, IntMap.insert cellKey (nextIdx, vert) acc)
   in resultMap

-- | Flatten a 3D cell coordinate to a unique integer key.
cellIndex :: Int -> Int -> Int -> Int -> Int -> Int
cellIndex cx cy cz rx ry =
  cx + cy * rx + cz * rx * ry

-- | Gather all Hermite data associated with a cell. A cell at
-- (cx,cy,cz) owns 12 edges; we collect those that have sign changes.
--
-- Cell edges are indexed by the minimal vertex of each edge plus
-- the axis direction. The 12 edges of a unit cube:
--
-- X-edges (4): at (cx,cy,cz), (cx,cy+1,cz), (cx,cy,cz+1), (cx,cy+1,cz+1)
-- Y-edges (4): at (cx,cy,cz), (cx+1,cy,cz), (cx,cy,cz+1), (cx+1,cy,cz+1)
-- Z-edges (4): at (cx,cy,cz), (cx+1,cy,cz), (cx,cy+1,cz), (cx+1,cy+1,cz)
gatherCellHermiteData ::
  Map.Map (Int, Int) HermitePoint ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  QEFData
gatherCellHermiteData hermiteData cx cy cz rx ry =
  let edgeKeys =
        -- X-edges
        [ (vertexIndex cx cy cz rx ry, edgeAxisX),
          (vertexIndex cx (cy + 1) cz rx ry, edgeAxisX),
          (vertexIndex cx cy (cz + 1) rx ry, edgeAxisX),
          (vertexIndex cx (cy + 1) (cz + 1) rx ry, edgeAxisX),
          -- Y-edges
          (vertexIndex cx cy cz rx ry, edgeAxisY),
          (vertexIndex (cx + 1) cy cz rx ry, edgeAxisY),
          (vertexIndex cx cy (cz + 1) rx ry, edgeAxisY),
          (vertexIndex (cx + 1) cy (cz + 1) rx ry, edgeAxisY),
          -- Z-edges
          (vertexIndex cx cy cz rx ry, edgeAxisZ),
          (vertexIndex (cx + 1) cy cz rx ry, edgeAxisZ),
          (vertexIndex cx (cy + 1) cz rx ry, edgeAxisZ),
          (vertexIndex (cx + 1) (cy + 1) cz rx ry, edgeAxisZ)
        ]
   in foldl'
        ( \(QEFData ns ps) key ->
            case Map.lookup key hermiteData of
              Just (HermitePoint pos normal) ->
                QEFData (normal : ns) (pos : ps)
              Nothing ->
                QEFData ns ps
        )
        (QEFData [] [])
        edgeKeys

-- | Compute the mass point (centroid) of a list of positions.
computeMassPoint :: [V3] -> V3
computeMassPoint [] = vzero
computeMassPoint points =
  let count = length points
      sumPt = foldl' (^+^) vzero points
   in (1.0 / fromIntegral count) *^ sumPt

-- | Compute the average normal from a list of normals.
computeAverageNormal :: [V3] -> V3
computeAverageNormal [] = V3 0 1 0
computeAverageNormal normals =
  let sumN = foldl' (^+^) vzero normals
   in normalize sumN

-- | Solve the QEF to find the optimal vertex position.
--
-- We solve the system A^T A x = A^T b, where each row of A is a
-- surface normal and each element of b is
-- dot(normal, intersection_point).
--
-- Cramer's rule is an exact (non-iterative) solver, so a single
-- call to 'solveMat3' suffices. A regularization term biases
-- toward the mass point to keep the solution near the cell center.
solveQEF :: [V3] -> [V3] -> V3 -> V3
solveQEF normals points massPoint =
  let -- Accumulate A^T A and A^T b from Hermite data.
      -- Each tangent plane constraint: n_i . (x - p_i) = 0
      -- Rewritten: n_i . x = n_i . p_i
      -- So row of A is n_i, and element of b is n_i . p_i.
      (ata, atb) =
        foldl'
          accumHermiteRow
          (zeroMat3, vzero)
          (zip normals points)

      -- Add regularization: lambda * (x - massPoint) = 0
      -- This adds lambda * I to ATA and lambda * massPoint to ATb.
      ataReg = addRegularization qefRegularization ata
      atbReg = atb ^+^ qefRegularization *^ massPoint
   in solveMat3 ataReg atbReg massPoint

-- | Accumulate one Hermite row into the A^T A matrix and A^T b
-- vector. Row of A is the normal, element of b is dot(normal, point).
accumHermiteRow :: (Mat3, V3) -> (V3, V3) -> (Mat3, V3)
accumHermiteRow (ata, atb) (normal, point)
  | vlengthSq normal < minNormalLengthSq = (ata, atb)
  | otherwise =
      let V3 nx ny nz = normal
          bi = dot normal point

          -- Outer product n * n^T added to ATA
          newATA =
            Mat3
              (m3_00 ata + nx * nx)
              (m3_01 ata + nx * ny)
              (m3_02 ata + nx * nz)
              (m3_10 ata + ny * nx)
              (m3_11 ata + ny * ny)
              (m3_12 ata + ny * nz)
              (m3_20 ata + nz * nx)
              (m3_21 ata + nz * ny)
              (m3_22 ata + nz * nz)

          -- n * b added to ATb
          newATb = atb ^+^ bi *^ normal
       in (newATA, newATb)

-- ----------------------------------------------------------------
-- 3x3 matrix type for the QEF solver
-- ----------------------------------------------------------------

-- | Row-major 3x3 matrix.
data Mat3 = Mat3
  { m3_00 :: Float,
    m3_01 :: Float,
    m3_02 :: Float,
    m3_10 :: Float,
    m3_11 :: Float,
    m3_12 :: Float,
    m3_20 :: Float,
    m3_21 :: Float,
    m3_22 :: Float
  }

-- | The zero matrix.
zeroMat3 :: Mat3
zeroMat3 = Mat3 0 0 0 0 0 0 0 0 0

-- | Add a scalar multiple of the identity to a matrix (for
-- regularization).
addRegularization :: Float -> Mat3 -> Mat3
addRegularization lambda mat =
  mat
    { m3_00 = m3_00 mat + lambda,
      m3_11 = m3_11 mat + lambda,
      m3_22 = m3_22 mat + lambda
    }

-- | Solve a 3x3 linear system Ax = b using Cramer's rule.
-- Falls back to the provided default if the determinant is near
-- zero.
solveMat3 :: Mat3 -> V3 -> V3 -> V3
solveMat3 (Mat3 a00 a01 a02 a10 a11 a12 a20 a21 a22) (V3 bx by bz) fallback =
  let detA =
        a00 * (a11 * a22 - a12 * a21)
          - a01 * (a10 * a22 - a12 * a20)
          + a02 * (a10 * a21 - a11 * a20)
   in if abs detA < interpolationMinDenom
        then fallback
        else
          let invDet = 1.0 / detA
              solX =
                invDet
                  * ( bx * (a11 * a22 - a12 * a21)
                        - a01 * (by * a22 - bz * a12)
                        + a02 * (by * a21 - bz * a11)
                    )
              solY =
                invDet
                  * ( a00 * (by * a22 - bz * a12)
                        - bx * (a10 * a22 - a12 * a20)
                        + a02 * (a10 * bz - by * a20)
                    )
              solZ =
                invDet
                  * ( a00 * (a11 * bz - by * a21)
                        - a01 * (a10 * bz - by * a20)
                        + bx * (a10 * a21 - a11 * a20)
                    )
           in V3 solX solY solZ

-- | Clamp a position to lie within the given axis-aligned bounding
-- box.
clampToCell :: V3 -> V3 -> V3 -> V3
clampToCell (V3 minX minY minZ) (V3 maxX maxY maxZ) (V3 px py pz) =
  V3
    (clampF minX maxX px)
    (clampF minY maxY py)
    (clampF minZ maxZ pz)

-- ----------------------------------------------------------------
-- Quad emission
-- ----------------------------------------------------------------

-- | Emit quads (as pairs of triangles) for all internal sign-changing
-- edges. Each internal edge is shared by exactly four cells; we
-- connect their four dual vertices into a quad.
--
-- Returns the final vertex list and index list.
emitQuads ::
  Array Int Float ->
  IntMap.IntMap (Int, Vertex) ->
  GridDims ->
  ([Vertex], [Word32])
emitQuads sdfValues cellVertexMap dims =
  let rx = gdResX dims
      ry = gdResY dims
      rz = gdResZ dims

      -- Collect all internal edges that have sign changes.
      -- An X-edge at vertex (vx,vy,vz) is internal if all four
      -- surrounding cells exist, i.e., vy >= 1, vz >= 1,
      -- vy <= ry-1, vz <= rz-1, and vx < rx.
      -- Similarly for Y and Z edges.

      xEdges =
        [ (vx, vy, vz, edgeAxisX)
        | vx <- [0 .. rx - 1],
          vy <- [1 .. ry - 1],
          vz <- [1 .. rz - 1]
        ]

      yEdges =
        [ (vx, vy, vz, edgeAxisY)
        | vx <- [1 .. rx - 1],
          vy <- [0 .. ry - 1],
          vz <- [1 .. rz - 1]
        ]

      zEdges =
        [ (vx, vy, vz, edgeAxisZ)
        | vx <- [1 .. rx - 1],
          vy <- [1 .. ry - 1],
          vz <- [0 .. rz - 1]
        ]

      allEdges = xEdges ++ yEdges ++ zEdges

      -- Build the vertex array from the cell vertex map, sorted by
      -- vertex index to ensure correct index-to-vertex mapping.
      vertexArray =
        map snd $
          sortBy (comparing fst) $
            IntMap.foldr'
              (\(idx, vert) acc -> (idx, vert) : acc)
              []
              cellVertexMap

      -- Process each internal edge, collecting index triples.
      indices =
        foldl'
          ( \acc (vx, vy, vz, axis) ->
              let valA = lookupSDF sdfValues vx vy vz rx ry
                  valB = case axis of
                    0 -> lookupSDF sdfValues (vx + 1) vy vz rx ry
                    1 -> lookupSDF sdfValues vx (vy + 1) vz rx ry
                    _ -> lookupSDF sdfValues vx vy (vz + 1) rx ry
               in if signChange valA valB
                    then
                      emitEdgeQuad
                        cellVertexMap
                        rx
                        ry
                        vx
                        vy
                        vz
                        axis
                        valA
                        acc
                    else acc
          )
          []
          allEdges
   in (vertexArray, reverse indices)

-- | Emit a quad (two triangles) for a single sign-changing internal
-- edge. The four cells sharing the edge are connected.
--
-- For an X-edge at (vx,vy,vz), the four surrounding cells are:
--   (vx, vy-1, vz-1), (vx, vy, vz-1), (vx, vy-1, vz), (vx, vy, vz)
--
-- For a Y-edge at (vx,vy,vz), the four surrounding cells are:
--   (vx-1, vy, vz-1), (vx, vy, vz-1), (vx-1, vy, vz), (vx, vy, vz)
--
-- For a Z-edge at (vx,vy,vz), the four surrounding cells are:
--   (vx-1, vy-1, vz), (vx, vy-1, vz), (vx-1, vy, vz), (vx, vy, vz)
--
-- Winding order depends on the sign change direction to ensure
-- CCW front faces.
emitEdgeQuad ::
  IntMap.IntMap (Int, Vertex) ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Float ->
  [Word32] ->
  [Word32]
emitEdgeQuad cellVertexMap rx ry vx vy vz axis valA acc =
  let -- Determine the four cell indices based on edge axis.
      (c0, c1, c2, c3) = case axis of
        0 ->
          -- X-edge: four cells around (vy, vz)
          ( cellIndex vx (vy - 1) (vz - 1) rx ry,
            cellIndex vx vy (vz - 1) rx ry,
            cellIndex vx (vy - 1) vz rx ry,
            cellIndex vx vy vz rx ry
          )
        1 ->
          -- Y-edge: four cells around (vx, vz)
          ( cellIndex (vx - 1) vy (vz - 1) rx ry,
            cellIndex vx vy (vz - 1) rx ry,
            cellIndex (vx - 1) vy vz rx ry,
            cellIndex vx vy vz rx ry
          )
        _ ->
          -- Z-edge: four cells around (vx, vy)
          ( cellIndex (vx - 1) (vy - 1) vz rx ry,
            cellIndex vx (vy - 1) vz rx ry,
            cellIndex (vx - 1) vy vz rx ry,
            cellIndex vx vy vz rx ry
          )

      -- Look up the vertex indices for the four cells.
      lookupIdx key = fmap fst (IntMap.lookup key cellVertexMap)
   in case (lookupIdx c0, lookupIdx c1, lookupIdx c2, lookupIdx c3) of
        (Just i0, Just i1, Just i2, Just i3) ->
          let w0 = fromIntegral i0 :: Word32
              w1 = fromIntegral i1
              w2 = fromIntegral i2
              w3 = fromIntegral i3
           in -- Orient based on sign change direction.
              -- If valA < 0 (inside to outside), use one winding;
              -- if valA >= 0 (outside to inside), flip.
              --
              -- Quad vertices: c0--c1
              --                |    |
              --                c2--c3
              --
              -- CCW triangles: (c0,c2,c3) and (c0,c3,c1)
              -- or flipped:    (c0,c3,c2) and (c0,c1,c3)
              if valA < 0
                then
                  -- Inside to outside: normal faces toward positive SDF
                  w3 : w2 : w0 : w1 : w3 : w0 : acc
                else
                  -- Outside to inside: flip winding
                  w2 : w3 : w0 : w3 : w1 : w0 : acc
        _ -> acc
