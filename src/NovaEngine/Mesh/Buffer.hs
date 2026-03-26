-- | GPU-ready vertex and index buffer packing.
--
-- Pure functions that flatten 'Mesh' data into contiguous float\/index
-- arrays matching common GPU vertex attribute layouts.  No IO — the
-- consumer is responsible for uploading the packed data.
--
-- Two layout strategies are provided:
--
-- * __Interleaved__ ('packInterleaved') — all attributes for one vertex
--   are contiguous: @[px, py, pz, nx, ny, nz, u, v, tx, ty, tz, tw, ...]@.
--   Best for static meshes and typical @glVertexAttribPointer@ setups.
--
-- * __Separate__ ('packPositions', 'packNormals', 'packUVs',
--   'packTangents') — each attribute in its own buffer.  Useful when
--   only a subset of attributes is needed (e.g. shadow passes that
--   need only positions and indices).
module NovaEngine.Mesh.Buffer
  ( -- * Interleaved packing
    packInterleaved,

    -- * Separate attribute packing
    packPositions,
    packNormals,
    packUVs,
    packTangents,
    packIndices,

    -- * Layout constants
    interleavedStride,
    interleavedFloats,
    positionOffset,
    normalOffset,
    uvOffset,
    tangentOffset,
    colorOffset,
    positionComponents,
    normalComponents,
    uvComponents,
    tangentComponents,
    colorComponents,

    -- * Attribute descriptor
    AttribLayout (..),
    interleavedLayout,
  )
where

import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Interleaved packing
-- ----------------------------------------------------------------

-- | Pack all vertex attributes into an interleaved float list.
--
-- Layout per vertex (16 floats, 64 bytes):
--
-- @
-- [px, py, pz, nx, ny, nz, u, v, tx, ty, tz, tw, cr, cg, cb, ca]
-- @
--
-- Total length: @16 * vertexCount mesh@.
packInterleaved :: Mesh -> [Float]
packInterleaved mesh = concatMap packVertex (meshVertices mesh)
  where
    packVertex (Vertex (V3 px py pz) (V3 nx ny nz) (V2 u v) (V4 tx ty tz tw) (V4 cr cg cb ca)) =
      [px, py, pz, nx, ny, nz, u, v, tx, ty, tz, tw, cr, cg, cb, ca]

-- ----------------------------------------------------------------
-- Separate attribute packing
-- ----------------------------------------------------------------

-- | Pack only vertex positions into a float list.
--
-- Layout: @[px, py, pz, ...]@.  Length: @3 * vertexCount mesh@.
packPositions :: Mesh -> [Float]
packPositions mesh = concatMap packPos (meshVertices mesh)
  where
    packPos (Vertex (V3 px py pz) _ _ _ _) = [px, py, pz]

-- | Pack only vertex normals into a float list.
--
-- Layout: @[nx, ny, nz, ...]@.  Length: @3 * vertexCount mesh@.
packNormals :: Mesh -> [Float]
packNormals mesh = concatMap packNrm (meshVertices mesh)
  where
    packNrm (Vertex _ (V3 nx ny nz) _ _ _) = [nx, ny, nz]

-- | Pack only UV coordinates into a float list.
--
-- Layout: @[u, v, ...]@.  Length: @2 * vertexCount mesh@.
packUVs :: Mesh -> [Float]
packUVs mesh = concatMap packUV (meshVertices mesh)
  where
    packUV (Vertex _ _ (V2 u v) _ _) = [u, v]

-- | Pack only tangent vectors into a float list.
--
-- Layout: @[tx, ty, tz, tw, ...]@.  Length: @4 * vertexCount mesh@.
packTangents :: Mesh -> [Float]
packTangents mesh = concatMap packTan (meshVertices mesh)
  where
    packTan (Vertex _ _ _ (V4 tx ty tz tw) _) = [tx, ty, tz, tw]

-- | Extract the index list from a mesh.
--
-- Already in GPU-ready format ('Word32').  This is a convenience
-- function equivalent to 'meshIndices'.
packIndices :: Mesh -> [Word32]
packIndices = meshIndices

-- ----------------------------------------------------------------
-- Layout constants
-- ----------------------------------------------------------------

-- | Number of floats per vertex in the interleaved layout.
--
-- Position (3) + Normal (3) + UV (2) + Tangent (4) + Color (4) = 16.
interleavedFloats :: Int
interleavedFloats = 16

-- | Byte stride between consecutive vertices in the interleaved layout.
--
-- 16 floats * 4 bytes = 64.
interleavedStride :: Int
interleavedStride = interleavedFloats * floatBytes

-- | Byte offset of the position attribute (0).
positionOffset :: Int
positionOffset = 0

-- | Byte offset of the normal attribute (12).
normalOffset :: Int
normalOffset = positionComponents * floatBytes

-- | Byte offset of the UV attribute (24).
uvOffset :: Int
uvOffset = normalOffset + normalComponents * floatBytes

-- | Byte offset of the tangent attribute (32).
tangentOffset :: Int
tangentOffset = uvOffset + uvComponents * floatBytes

-- | Number of float components in a position (3).
positionComponents :: Int
positionComponents = 3

-- | Number of float components in a normal (3).
normalComponents :: Int
normalComponents = 3

-- | Number of float components in a UV (2).
uvComponents :: Int
uvComponents = 2

-- | Number of float components in a tangent (4).
tangentComponents :: Int
tangentComponents = 4

-- | Byte offset of the color attribute (48).
colorOffset :: Int
colorOffset = tangentOffset + tangentComponents * floatBytes

-- | Number of float components in a color (4).
colorComponents :: Int
colorComponents = 4

-- ----------------------------------------------------------------
-- Attribute descriptor
-- ----------------------------------------------------------------

-- | Describes a single vertex attribute for GPU setup.
--
-- Maps directly to @glVertexAttribPointer(index, components, GL_FLOAT,
-- GL_FALSE, stride, offset)@ or the Vulkan @VkVertexInputAttributeDescription@.
data AttribLayout = AttribLayout
  { -- | Attribute index (location in the shader).
    alIndex :: !Int,
    -- | Number of float components (e.g. 3 for position).
    alComponents :: !Int,
    -- | Byte offset from the start of a vertex.
    alOffset :: !Int,
    -- | Byte stride between consecutive vertices.
    alStride :: !Int
  }
  deriving (Show, Eq)

-- | Attribute layouts for the interleaved vertex format.
--
-- Returns five 'AttribLayout' values for position (0), normal (1),
-- UV (2), tangent (3), and color (4), using the standard
-- interleaved stride.
interleavedLayout :: [AttribLayout]
interleavedLayout =
  [ AttribLayout 0 positionComponents positionOffset interleavedStride,
    AttribLayout 1 normalComponents normalOffset interleavedStride,
    AttribLayout 2 uvComponents uvOffset interleavedStride,
    AttribLayout 3 tangentComponents tangentOffset interleavedStride,
    AttribLayout 4 colorComponents colorOffset interleavedStride
  ]

-- ----------------------------------------------------------------
-- Internal constants
-- ----------------------------------------------------------------

-- | Size of a 32-bit float in bytes.
floatBytes :: Int
floatBytes = 4
