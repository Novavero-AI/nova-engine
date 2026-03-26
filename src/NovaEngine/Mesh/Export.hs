-- | Mesh export to OBJ and glTF 2.0 formats.
--
-- Wavefront OBJ (text) and glTF 2.0 (JSON with inline base64 buffer)
-- exporters. No external dependencies beyond @base@.
--
-- Internally uses 'ShowS' (difference lists) for /O(n)/ total string
-- building instead of left-nested @++@ which would be /O(n²)/.
module NovaEngine.Mesh.Export
  ( -- * Wavefront OBJ
    meshToOBJ,
    meshesToOBJ,

    -- * glTF 2.0
    meshToGLTF,
    meshesToGLTF,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Word (Word8)
import GHC.Float (castFloatToWord32)
import NovaEngine.Mesh.Types (Mesh (..), V2 (..), V3 (..), V4 (..), Vertex (..), Word32)

-- ================================================================
-- OBJ export
-- ================================================================

-- | Export a single mesh to Wavefront OBJ text format.
--
-- Emits @v@, @vn@, @vt@, and @f@ lines. Face indices are 1-based
-- and reference position, texcoord, and normal in @v\/vt\/vn@ form.
meshToOBJ :: Mesh -> String
meshToOBJ mesh =
  ( formatPositionsS vertices
      . formatNormalsS vertices
      . formatUVsS vertices
      . formatFacesS indices
  )
    ""
  where
    vertices = meshVertices mesh
    indices = meshIndices mesh

-- | Export multiple named meshes to a single Wavefront OBJ string.
--
-- Each mesh is preceded by an @o@ (object name) line. Vertex indices
-- are offset so that each mesh references the correct global vertices.
meshesToOBJ :: [(String, Mesh)] -> String
meshesToOBJ namedMeshes =
  foldr (\pair acc -> emitObjectS pair . acc) id offsetPairs ""
  where
    offsets = scanl (\acc (_, mesh) -> acc + meshVertexCount mesh) 0 namedMeshes
    offsetPairs = zip offsets namedMeshes

    emitObjectS (offset, (name, mesh)) =
      let vertices = meshVertices mesh
          indices = meshIndices mesh
       in showString "o "
            . showString name
            . showChar '\n'
            . formatPositionsS vertices
            . formatNormalsS vertices
            . formatUVsS vertices
            . formatFacesOffsetS offset indices

-- ----------------------------------------------------------------
-- OBJ formatting helpers (ShowS)
-- ----------------------------------------------------------------

-- | Format all vertex positions as OBJ @v@ lines.
formatPositionsS :: [Vertex] -> ShowS
formatPositionsS = foldr (\vert acc -> formatPositionS vert . acc) id

-- | Format a single vertex position as an OBJ @v@ line.
formatPositionS :: Vertex -> ShowS
formatPositionS vert =
  let V3 px py pz = vPosition vert
   in showString "v "
        . showFloat px
        . showChar ' '
        . showFloat py
        . showChar ' '
        . showFloat pz
        . showChar '\n'

-- | Format all vertex normals as OBJ @vn@ lines.
formatNormalsS :: [Vertex] -> ShowS
formatNormalsS = foldr (\vert acc -> formatNormalS vert . acc) id

-- | Format a single vertex normal as an OBJ @vn@ line.
formatNormalS :: Vertex -> ShowS
formatNormalS vert =
  let V3 nx ny nz = vNormal vert
   in showString "vn "
        . showFloat nx
        . showChar ' '
        . showFloat ny
        . showChar ' '
        . showFloat nz
        . showChar '\n'

-- | Format all vertex UVs as OBJ @vt@ lines.
formatUVsS :: [Vertex] -> ShowS
formatUVsS = foldr (\vert acc -> formatUVS vert . acc) id

-- | Format a single vertex UV as an OBJ @vt@ line.
formatUVS :: Vertex -> ShowS
formatUVS vert =
  let V2 tu tv = vUV vert
   in showString "vt "
        . showFloat tu
        . showChar ' '
        . showFloat tv
        . showChar '\n'

-- | Format triangle faces from indices with a base offset of 0.
formatFacesS :: [Word32] -> ShowS
formatFacesS = formatFacesOffsetS 0

-- | Format triangle faces from indices, adding a 1-based offset.
-- OBJ indices are 1-based, so we add @globalOffset + 1@.
formatFacesOffsetS :: Int -> [Word32] -> ShowS
formatFacesOffsetS globalOffset = go
  where
    go (idx0 : idx1 : idx2 : rest) =
      formatTriangleS idx0 idx1 idx2 . go rest
    go _ = id

    formatTriangleS idx0 idx1 idx2 =
      showString "f "
        . faceVertexS idx0
        . showChar ' '
        . faceVertexS idx1
        . showChar ' '
        . faceVertexS idx2
        . showChar '\n'

    faceVertexS idx =
      let oneBasedIndex = fromIntegral idx + globalOffset + objIndexBase
       in shows oneBasedIndex
            . showChar '/'
            . shows oneBasedIndex
            . showChar '/'
            . shows oneBasedIndex

-- | OBJ files use 1-based indexing.
objIndexBase :: Int
objIndexBase = 1

-- ================================================================
-- glTF 2.0 export
-- ================================================================

-- | Export a single mesh to a self-contained glTF 2.0 JSON string.
--
-- The binary buffer is embedded as a base64-encoded data URI.
-- Includes accessors for positions (VEC3), normals (VEC3),
-- texcoords (VEC2), tangents (VEC4), and indices (SCALAR UNSIGNED_INT).
-- Position accessor includes min\/max bounds.
meshToGLTF :: Mesh -> String
meshToGLTF mesh = meshesToGLTF [("mesh", mesh)]

-- | Export multiple named meshes to a single glTF 2.0 JSON string.
--
-- All mesh data shares one binary buffer, partitioned by buffer
-- views and accessors.
meshesToGLTF :: [(String, Mesh)] -> String
meshesToGLTF namedMeshes =
  jsonObjectS
    [ ("asset", gltfAssetS),
      ("scene", showChar '0'),
      ("scenes", jsonArrayS [jsonObjectS [("nodes", jsonArrayS (map showsInt nodeIndices))]]),
      ("nodes", jsonArrayS (zipWith formatNodeS nodeIndices namedMeshes)),
      ("meshes", jsonArrayS (zipWith formatMeshEntryS [0 :: Int ..] namedMeshes)),
      ("accessors", jsonArrayS allAccessors),
      ("bufferViews", jsonArrayS allBufferViews),
      ("buffers", jsonArrayS [formatBufferS totalBufferBytes allBytes])
    ]
    ""
  where
    meshCount = length namedMeshes
    nodeIndices = take meshCount [0 :: Int ..]

    allMeshData = map buildMeshData namedMeshes

    -- Each mesh produces 5 accessors and 5 buffer views
    buildMeshData :: (String, Mesh) -> MeshData
    buildMeshData (_, mesh) =
      let vertices = meshVertices mesh
          indices = meshIndices mesh
          vertCount = meshVertexCount mesh
          idxCount = length indices

          posBytes = concatMap (encodeV3 . vPosition) vertices
          nrmBytes = concatMap (encodeV3 . vNormal) vertices
          texBytes = concatMap (encodeV2 . vUV) vertices
          tanBytes = concatMap (encodeV4 . vTangent) vertices
          idxBytes = concatMap encodeWord32 indices

          posByteLen = vertCount * bytesPerVec3
          nrmByteLen = vertCount * bytesPerVec3
          texByteLen = vertCount * bytesPerVec2
          tanByteLen = vertCount * bytesPerVec4
          idxByteLen = idxCount * bytesPerWord32

          (posMin, posMax) = computePositionBounds vertices
       in MeshData
            { mdVertexCount = vertCount,
              mdIndexCount = idxCount,
              mdPositionBytes = posBytes,
              mdNormalBytes = nrmBytes,
              mdTexcoordBytes = texBytes,
              mdTangentBytes = tanBytes,
              mdIndexBytes = idxBytes,
              mdPositionByteLength = posByteLen,
              mdNormalByteLength = nrmByteLen,
              mdTexcoordByteLength = texByteLen,
              mdTangentByteLength = tanByteLen,
              mdIndexByteLength = idxByteLen,
              mdPositionMin = posMin,
              mdPositionMax = posMax
            }

    -- Compute byte offsets for all mesh data laid out sequentially
    allByteOffsets = scanl (\acc md -> acc + meshDataTotalBytes md) 0 allMeshData

    totalBufferBytes = sum (map meshDataTotalBytes allMeshData)

    allBytes = concatMap meshDataAllBytes allMeshData

    -- Build buffer views: 5 per mesh (position, normal, texcoord, tangent, index)
    allBufferViews = concatMap (uncurry buildBufferViewsS) (zip allByteOffsets allMeshData)

    -- Build accessors: 5 per mesh
    allAccessors = concatMap buildAccessorGroup (zipWith3Tuples [0 ..] allByteOffsets allMeshData)

    -- Format a node referencing its mesh
    formatNodeS nodeIdx (name, _) =
      jsonObjectS
        [ ("mesh", showsInt nodeIdx),
          ("name", jsonStringS name)
        ]

    -- Format a mesh entry referencing its accessors
    formatMeshEntryS meshIdx (name, _) =
      let baseAccessor = meshIdx * accessorsPerMesh
       in jsonObjectS
            [ ( "primitives",
                jsonArrayS
                  [ jsonObjectS
                      [ ( "attributes",
                          jsonObjectS
                            [ ("POSITION", showsInt baseAccessor),
                              ("NORMAL", showsInt (baseAccessor + normalAccessorOffset)),
                              ("TEXCOORD_0", showsInt (baseAccessor + texcoordAccessorOffset)),
                              ("TANGENT", showsInt (baseAccessor + tangentAccessorOffset))
                            ]
                        ),
                        ("indices", showsInt (baseAccessor + indexAccessorOffset))
                      ]
                  ]
              ),
              ("name", jsonStringS name)
            ]

-- ----------------------------------------------------------------
-- Mesh data record
-- ----------------------------------------------------------------

-- | Intermediate data for a single mesh being exported to glTF.
data MeshData = MeshData
  { mdVertexCount :: Int,
    mdIndexCount :: Int,
    mdPositionBytes :: [Word8],
    mdNormalBytes :: [Word8],
    mdTexcoordBytes :: [Word8],
    mdTangentBytes :: [Word8],
    mdIndexBytes :: [Word8],
    mdPositionByteLength :: Int,
    mdNormalByteLength :: Int,
    mdTexcoordByteLength :: Int,
    mdTangentByteLength :: Int,
    mdIndexByteLength :: Int,
    mdPositionMin :: V3,
    mdPositionMax :: V3
  }

-- | Total byte size of all buffer data for one mesh.
meshDataTotalBytes :: MeshData -> Int
meshDataTotalBytes md =
  mdPositionByteLength md
    + mdNormalByteLength md
    + mdTexcoordByteLength md
    + mdTangentByteLength md
    + mdIndexByteLength md

-- | Concatenate all buffer data for one mesh in attribute order.
meshDataAllBytes :: MeshData -> [Word8]
meshDataAllBytes md =
  mdPositionBytes md
    ++ mdNormalBytes md
    ++ mdTexcoordBytes md
    ++ mdTangentBytes md
    ++ mdIndexBytes md

-- ----------------------------------------------------------------
-- glTF buffer views and accessors
-- ----------------------------------------------------------------

-- | Number of accessors generated per mesh.
accessorsPerMesh :: Int
accessorsPerMesh = 5

-- | Number of buffer views generated per mesh.
bufferViewsPerMesh :: Int
bufferViewsPerMesh = 5

-- | Accessor offset for normals within a mesh's accessor group.
normalAccessorOffset :: Int
normalAccessorOffset = 1

-- | Accessor offset for texcoords within a mesh's accessor group.
texcoordAccessorOffset :: Int
texcoordAccessorOffset = 2

-- | Accessor offset for tangents within a mesh's accessor group.
tangentAccessorOffset :: Int
tangentAccessorOffset = 3

-- | Accessor offset for indices within a mesh's accessor group.
indexAccessorOffset :: Int
indexAccessorOffset = 4

-- | glTF component type for 32-bit float.
componentTypeFloat :: Int
componentTypeFloat = 5126

-- | glTF component type for 32-bit unsigned integer.
componentTypeUnsignedInt :: Int
componentTypeUnsignedInt = 5125

-- | glTF buffer view target for vertex attributes.
bufferTargetArrayBuffer :: Int
bufferTargetArrayBuffer = 34962

-- | glTF buffer view target for element (index) arrays.
bufferTargetElementArrayBuffer :: Int
bufferTargetElementArrayBuffer = 34963

-- | Build 5 buffer views for one mesh at the given byte offset.
buildBufferViewsS :: Int -> MeshData -> [ShowS]
buildBufferViewsS baseOffset md =
  [ formatBufferViewS posOffset (mdPositionByteLength md) bufferTargetArrayBuffer,
    formatBufferViewS nrmOffset (mdNormalByteLength md) bufferTargetArrayBuffer,
    formatBufferViewS texOffset (mdTexcoordByteLength md) bufferTargetArrayBuffer,
    formatBufferViewS tanOffset (mdTangentByteLength md) bufferTargetArrayBuffer,
    formatBufferViewS idxOffset (mdIndexByteLength md) bufferTargetElementArrayBuffer
  ]
  where
    posOffset = baseOffset
    nrmOffset = posOffset + mdPositionByteLength md
    texOffset = nrmOffset + mdNormalByteLength md
    tanOffset = texOffset + mdTexcoordByteLength md
    idxOffset = tanOffset + mdTangentByteLength md

-- | Build 5 accessors for one mesh.
buildAccessorsS :: Int -> MeshData -> [ShowS]
buildAccessorsS meshIdx md =
  [ formatAccessorS posView (mdVertexCount md) componentTypeFloat "VEC3" (Just (mdPositionMin md, mdPositionMax md)),
    formatAccessorS nrmView (mdVertexCount md) componentTypeFloat "VEC3" Nothing,
    formatAccessorS texView (mdVertexCount md) componentTypeFloat "VEC2" Nothing,
    formatAccessorS tanView (mdVertexCount md) componentTypeFloat "VEC4" Nothing,
    formatAccessorS idxView (mdIndexCount md) componentTypeUnsignedInt "SCALAR" Nothing
  ]
  where
    baseView = meshIdx * bufferViewsPerMesh
    posView = baseView
    nrmView = baseView + 1
    texView = baseView + 2
    tanView = baseView + 3
    idxView = baseView + 4

-- ----------------------------------------------------------------
-- glTF JSON formatting (ShowS)
-- ----------------------------------------------------------------

-- | glTF asset metadata.
gltfAssetS :: ShowS
gltfAssetS =
  jsonObjectS
    [ ("version", jsonStringS "2.0"),
      ("generator", jsonStringS "gb-mesh")
    ]

-- | Format a buffer view as a JSON object.
formatBufferViewS :: Int -> Int -> Int -> ShowS
formatBufferViewS byteOffset byteLength target =
  jsonObjectS
    [ ("buffer", showChar '0'),
      ("byteOffset", showsInt byteOffset),
      ("byteLength", showsInt byteLength),
      ("target", showsInt target)
    ]

-- | Format an accessor as a JSON object, optionally with min/max.
formatAccessorS :: Int -> Int -> Int -> String -> Maybe (V3, V3) -> ShowS
formatAccessorS bufferView count componentType accessorType maybeBounds =
  jsonObjectS (baseFields ++ boundsFields)
  where
    baseFields =
      [ ("bufferView", showsInt bufferView),
        ("componentType", showsInt componentType),
        ("count", showsInt count),
        ("type", jsonStringS accessorType)
      ]
    boundsFields = case maybeBounds of
      Nothing -> []
      Just (V3 minX minY minZ, V3 maxX maxY maxZ) ->
        [ ("min", jsonFloatArrayS [minX, minY, minZ]),
          ("max", jsonFloatArrayS [maxX, maxY, maxZ])
        ]

-- | Format a buffer as a JSON object with a data URI.
formatBufferS :: Int -> [Word8] -> ShowS
formatBufferS byteLength bytes =
  jsonObjectS
    [ ("uri", jsonStringS (bufferDataURIPrefix ++ encodeBase64 bytes)),
      ("byteLength", showsInt byteLength)
    ]

-- | Data URI prefix for an octet-stream buffer.
bufferDataURIPrefix :: String
bufferDataURIPrefix = "data:application/octet-stream;base64,"

-- ----------------------------------------------------------------
-- JSON builder helpers (ShowS)
-- ----------------------------------------------------------------

-- | Build a JSON object from key-value pairs.
-- Keys are plain strings, values are 'ShowS' builders.
jsonObjectS :: [(String, ShowS)] -> ShowS
jsonObjectS pairs =
  showChar '{' . intercalateSep (showChar ',') (map formatPairS pairs) . showChar '}'
  where
    formatPairS (key, val) = jsonStringS key . showChar ':' . val

-- | Build a JSON array from pre-formatted 'ShowS' elements.
jsonArrayS :: [ShowS] -> ShowS
jsonArrayS elems = showChar '[' . intercalateSep (showChar ',') elems . showChar ']'

-- | Wrap a Haskell string as a JSON string literal with escaping.
jsonStringS :: String -> ShowS
jsonStringS str = showChar '"' . foldr (\ch acc -> escapeCharS ch . acc) id str . showChar '"'
  where
    escapeCharS '"' = showString "\\\""
    escapeCharS '\\' = showString "\\\\"
    escapeCharS '\n' = showString "\\n"
    escapeCharS '\r' = showString "\\r"
    escapeCharS '\t' = showString "\\t"
    escapeCharS ch
      | ch < '\x20' = showString "\\u" . showString (hexPad4 (ord ch))
      | otherwise = showChar ch
    hexPad4 n =
      [ hexDigit (n `shiftR` 12 .&. 0xF),
        hexDigit (n `shiftR` 8 .&. 0xF),
        hexDigit (n `shiftR` 4 .&. 0xF),
        hexDigit (n .&. 0xF)
      ]
    hexDigit d
      | d < 10 = chr (ord '0' + d)
      | otherwise = chr (ord 'a' + d - 10)

-- | Format a list of floats as a JSON array.
jsonFloatArrayS :: [Float] -> ShowS
jsonFloatArrayS = jsonArrayS . map showFloat

-- | Intersperse a separator between 'ShowS' builders.
intercalateSep :: ShowS -> [ShowS] -> ShowS
intercalateSep _ [] = id
intercalateSep _ [single] = single
intercalateSep sep (first : rest) =
  first . foldr (\item acc -> sep . item . acc) id rest

-- | 'shows' specialised to 'Int' to avoid ambiguity.
showsInt :: Int -> ShowS
showsInt = shows

-- ----------------------------------------------------------------
-- Position bounds computation
-- ----------------------------------------------------------------

-- | Compute axis-aligned bounding box for vertex positions.
-- Returns (min, max). For empty vertex lists, returns zero vectors.
computePositionBounds :: [Vertex] -> (V3, V3)
computePositionBounds [] = (V3 0 0 0, V3 0 0 0)
computePositionBounds (firstVert : rest) =
  foldl' accumBounds (initialPos, initialPos) rest
  where
    initialPos = vPosition firstVert
    accumBounds (V3 minX minY minZ, V3 maxX maxY maxZ) vert =
      let V3 px py pz = vPosition vert
       in ( V3 (min minX px) (min minY py) (min minZ pz),
            V3 (max maxX px) (max maxY py) (max maxZ pz)
          )

-- ----------------------------------------------------------------
-- Binary encoding
-- ----------------------------------------------------------------

-- | Bytes per VEC3 (3 floats x 4 bytes).
bytesPerVec3 :: Int
bytesPerVec3 = 12

-- | Bytes per VEC2 (2 floats x 4 bytes).
bytesPerVec2 :: Int
bytesPerVec2 = 8

-- | Bytes per VEC4 (4 floats x 4 bytes).
bytesPerVec4 :: Int
bytesPerVec4 = 16

-- | Bytes per Word32 index.
bytesPerWord32 :: Int
bytesPerWord32 = 4

-- | Encode a V3 as 12 bytes (3 little-endian IEEE 754 floats).
encodeV3 :: V3 -> [Word8]
encodeV3 (V3 x y z) = encodeFloat32 x ++ encodeFloat32 y ++ encodeFloat32 z

-- | Encode a V2 as 8 bytes (2 little-endian IEEE 754 floats).
encodeV2 :: V2 -> [Word8]
encodeV2 (V2 x y) = encodeFloat32 x ++ encodeFloat32 y

-- | Encode a V4 as 16 bytes (4 little-endian IEEE 754 floats).
encodeV4 :: V4 -> [Word8]
encodeV4 (V4 x y z w) =
  encodeFloat32 x ++ encodeFloat32 y ++ encodeFloat32 z ++ encodeFloat32 w

-- | Encode a Float as 4 little-endian bytes (IEEE 754).
encodeFloat32 :: Float -> [Word8]
encodeFloat32 = encodeWord32 . castFloatToWord32

-- | Encode a Word32 as 4 little-endian bytes.
encodeWord32 :: Word32 -> [Word8]
encodeWord32 word =
  [ fromIntegral (word .&. byteMask),
    fromIntegral (shiftR word 8 .&. byteMask),
    fromIntegral (shiftR word 16 .&. byteMask),
    fromIntegral (shiftR word 24 .&. byteMask)
  ]

-- | Bitmask for extracting one byte from a Word32.
byteMask :: Word32
byteMask = 0xFF

-- ----------------------------------------------------------------
-- Base64 encoding
-- ----------------------------------------------------------------

-- | Encode a list of bytes to a base64 string (RFC 4648).
encodeBase64 :: [Word8] -> String
encodeBase64 = go
  where
    go (byte0 : byte1 : byte2 : rest) =
      let combined =
            shiftL (fromIntegral byte0 :: Word32) 16
              .|. shiftL (fromIntegral byte1 :: Word32) 8
              .|. fromIntegral byte2
          char0 = base64CharAt (shiftR combined 18 .&. base64IndexMask)
          char1 = base64CharAt (shiftR combined 12 .&. base64IndexMask)
          char2 = base64CharAt (shiftR combined 6 .&. base64IndexMask)
          char3 = base64CharAt (combined .&. base64IndexMask)
       in char0 : char1 : char2 : char3 : go rest
    go [byte0, byte1] =
      let combined =
            shiftL (fromIntegral byte0 :: Word32) 16
              .|. shiftL (fromIntegral byte1 :: Word32) 8
          char0 = base64CharAt (shiftR combined 18 .&. base64IndexMask)
          char1 = base64CharAt (shiftR combined 12 .&. base64IndexMask)
          char2 = base64CharAt (shiftR combined 6 .&. base64IndexMask)
       in [char0, char1, char2, base64PadChar]
    go [byte0] =
      let combined = shiftL (fromIntegral byte0 :: Word32) 16
          char0 = base64CharAt (shiftR combined 18 .&. base64IndexMask)
          char1 = base64CharAt (shiftR combined 12 .&. base64IndexMask)
       in [char0, char1, base64PadChar, base64PadChar]
    go [] = []

-- | 6-bit mask for extracting base64 indices.
base64IndexMask :: Word32
base64IndexMask = 0x3F

-- | Base64 padding character.
base64PadChar :: Char
base64PadChar = '='

-- | Look up a base64 character by its 6-bit index (0--63).
-- Uses arithmetic over the RFC 4648 alphabet ranges rather
-- than partial list indexing.
base64CharAt :: Word32 -> Char
base64CharAt idx
  | idx < 26 = chr (ord 'A' + fromIntegral idx)
  | idx < 52 = chr (ord 'a' + fromIntegral (idx - 26))
  | idx < 62 = chr (ord '0' + fromIntegral (idx - 52))
  | idx == 62 = '+'
  | otherwise = '/'

-- ----------------------------------------------------------------
-- Float formatting
-- ----------------------------------------------------------------

-- | Show a float value for export formats as a 'ShowS' builder.
showFloat :: Float -> ShowS
showFloat = shows

-- ----------------------------------------------------------------
-- Utility
-- ----------------------------------------------------------------

-- | Zip three lists into a list of triples.
zipWith3Tuples :: [a] -> [b] -> [c] -> [(a, b, c)]
zipWith3Tuples (x : xs) (y : ys) (z : zs) = (x, y, z) : zipWith3Tuples xs ys zs
zipWith3Tuples _ _ _ = []

-- | Apply a function of three arguments to a triple.
buildAccessorGroup :: (Int, Int, MeshData) -> [ShowS]
buildAccessorGroup (meshIdx, _byteOffset, md) = buildAccessorsS meshIdx md
