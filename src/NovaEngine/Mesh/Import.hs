-- | Mesh import from OBJ and glTF 2.0 formats.
--
-- Parses Wavefront OBJ text and glTF 2.0 JSON (with embedded
-- base64 buffer) into 'Mesh' values. The glTF parser handles the
-- exact format produced by "NovaEngine.Mesh.Export". All operations are
-- pure string parsers with no IO.
module NovaEngine.Mesh.Import
  ( -- * Wavefront OBJ
    parseOBJ,
    parseManyOBJ,

    -- * glTF 2.0
    parseGLTF,
    parseManyGLTF,
  )
where

import Data.Array (Array, listArray, (!))
import Data.Array qualified as Array
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, isSpace, ord)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat)
import NovaEngine.Mesh.Types

-- ================================================================
-- Wavefront OBJ
-- ================================================================

-- ----------------------------------------------------------------
-- OBJ parser state
-- ----------------------------------------------------------------

-- | Accumulator for OBJ parsing. Lists are built in reverse and
-- reversed once at the end.
data OBJState = OBJState
  { objPositions :: [V3],
    objNormals :: [V3],
    objUVs :: [V2],
    objFaces :: [[(Int, Int, Int)]],
    objCurrentObject :: String,
    objPosCount :: Int,
    objNrmCount :: Int,
    objUVCount :: Int
  }

-- | Initial empty OBJ parser state.
emptyOBJState :: OBJState
emptyOBJState =
  OBJState
    { objPositions = [],
      objNormals = [],
      objUVs = [],
      objFaces = [],
      objCurrentObject = defaultObjectName,
      objPosCount = 0,
      objNrmCount = 0,
      objUVCount = 0
    }

-- ----------------------------------------------------------------
-- Public OBJ API
-- ----------------------------------------------------------------

-- | Parse a Wavefront OBJ string into a single 'Mesh'.
--
-- Supports @v@, @vn@, @vt@, and @f@ lines. Face indices may use
-- any of the @v@, @v\/vt@, @v\/\/vn@, or @v\/vt\/vn@ formats.
-- Faces with more than three vertices are fan-triangulated.
-- Missing normals default to @(0, 1, 0)@; missing UVs default to
-- @(0, 0)@; tangents default to @(1, 0, 0, 1)@.
--
-- Returns 'Nothing' if no faces are found.
parseOBJ :: String -> Maybe Mesh
parseOBJ input =
  let state = foldl' parseLine emptyOBJState (lines input)
   in buildMeshFromState state

-- | Parse a Wavefront OBJ string containing multiple objects into
-- a list of @(name, Mesh)@ pairs.
--
-- Objects are delimited by @o@ lines. Faces appearing before any
-- @o@ line are assigned the name @\"default\"@. Note that vertex
-- data (@v@, @vn@, @vt@) is global across all objects — only
-- face assignments are split.
parseManyOBJ :: String -> [(String, Mesh)]
parseManyOBJ input =
  let allLines = lines input
      state0 = foldl' parseLine emptyOBJState allLines
      -- Collect object boundaries by re-scanning for 'o' lines
      objects = collectObjects allLines
      -- Build the shared vertex arrays once
      posArr = listArraySafe (reverse (objPositions state0))
      nrmArr = listArraySafe (reverse (objNormals state0))
      uvArr = listArraySafe (reverse (objUVs state0))
   in [ (name, buildMeshFromFaces posArr nrmArr uvArr faces)
      | (name, faces) <- objects,
        not (null faces)
      ]

-- ----------------------------------------------------------------
-- OBJ line parsing
-- ----------------------------------------------------------------

-- | Parse a single OBJ line and update the accumulator.
parseLine :: OBJState -> String -> OBJState
parseLine state line = case words line of
  ("v" : rest) -> case parseFloats rest of
    [x, y, z] ->
      state
        { objPositions = V3 x y z : objPositions state,
          objPosCount = objPosCount state + 1
        }
    _ -> state
  ("vn" : rest) -> case parseFloats rest of
    [x, y, z] ->
      state
        { objNormals = V3 x y z : objNormals state,
          objNrmCount = objNrmCount state + 1
        }
    _ -> state
  ("vt" : rest) -> case parseFloats rest of
    (u : v : _) ->
      state
        { objUVs = V2 u v : objUVs state,
          objUVCount = objUVCount state + 1
        }
    [u] ->
      state
        { objUVs = V2 u 0 : objUVs state,
          objUVCount = objUVCount state + 1
        }
    _ -> state
  ("f" : rest) ->
    let nPos = objPosCount state
        nUV = objUVCount state
        nNrm = objNrmCount state
        verts = map (parseFaceVertex nPos nUV nNrm) rest
     in state {objFaces = verts : objFaces state}
  ("o" : nameWords) ->
    state {objCurrentObject = unwords nameWords}
  _ -> state

-- | Parse a list of strings as floats, skipping unparseable tokens.
parseFloats :: [String] -> [Float]
parseFloats = map readFloatSafe

-- | Parse a face vertex specification. Handles all four OBJ
-- formats:
--
--   * @v@
--   * @v\/vt@
--   * @v\/\/vn@
--   * @v\/vt\/vn@
--
-- Returns @(positionIndex, uvIndex, normalIndex)@ using 0-based
-- indexing. Missing components are represented as @-1@.
-- Negative OBJ indices are resolved relative to the current count
-- of the corresponding vertex attribute.
parseFaceVertex :: Int -> Int -> Int -> String -> (Int, Int, Int)
parseFaceVertex nPos nUV nNrm s =
  case splitOnSlash s of
    [vStr] -> (readIdx nPos vStr, missingIndex, missingIndex)
    [vStr, vtStr] -> (readIdx nPos vStr, readIdxMaybe nUV vtStr, missingIndex)
    [vStr, vtStr, vnStr] -> (readIdx nPos vStr, readIdxMaybe nUV vtStr, readIdxMaybe nNrm vnStr)
    _ -> (0, missingIndex, missingIndex)
  where
    -- OBJ indices are 1-based; negative indices are relative to
    -- the current count of the corresponding attribute.
    resolveIdx count raw
      | raw < 0 = count + raw
      | otherwise = raw - 1
    readIdx count str = resolveIdx count (readIntSafe str)
    readIdxMaybe _ "" = missingIndex
    readIdxMaybe count str = resolveIdx count (readIntSafe str)

-- | Split a string on '/' characters.
splitOnSlash :: String -> [String]
splitOnSlash = go []
  where
    go acc [] = [reverse acc]
    go acc ('/' : rest) = reverse acc : go [] rest
    go acc (c : rest) = go (c : acc) rest

-- ----------------------------------------------------------------
-- OBJ mesh building
-- ----------------------------------------------------------------

-- | Build a 'Mesh' from the final parser state.
buildMeshFromState :: OBJState -> Maybe Mesh
buildMeshFromState state
  | null (objFaces state) = Nothing
  | otherwise =
      let posArr = listArraySafe (reverse (objPositions state))
          nrmArr = listArraySafe (reverse (objNormals state))
          uvArr = listArraySafe (reverse (objUVs state))
          faces = reverse (objFaces state)
       in Just (buildMeshFromFaces posArr nrmArr uvArr faces)

-- | Build a 'Mesh' from indexed face data and shared vertex
-- arrays.
buildMeshFromFaces ::
  Array Int V3 ->
  Array Int V3 ->
  Array Int V2 ->
  [[(Int, Int, Int)]] ->
  Mesh
buildMeshFromFaces posArr nrmArr uvArr faces =
  let -- Fan-triangulate and flatten
      triangulated = concatMap fanTriangulate faces
      -- Build vertices
      verts =
        [ buildVertex posArr nrmArr uvArr posIdx uvIdx nrmIdx
        | (posIdx, uvIdx, nrmIdx) <- triangulated
        ]
      idxs = [fromIntegral i :: Word32 | i <- [0 .. length verts - 1]]
   in mkMesh verts idxs

-- | Construct a 'Vertex' from array lookups with bounds checking.
buildVertex ::
  Array Int V3 ->
  Array Int V3 ->
  Array Int V2 ->
  Int ->
  Int ->
  Int ->
  Vertex
buildVertex posArr nrmArr uvArr posIdx uvIdx nrmIdx =
  let pos = safeArrayLookup posArr posIdx (V3 0 0 0)
      nrm = safeArrayLookup nrmArr nrmIdx defaultNormal
      uv = safeArrayLookup uvArr uvIdx defaultUV
   in vertex pos nrm uv defaultTangent

-- | Fan-triangulate a polygon. The first vertex is the pivot; each
-- consecutive pair of subsequent vertices forms a triangle with
-- it.
fanTriangulate :: [a] -> [a]
fanTriangulate [] = []
fanTriangulate [_] = []
fanTriangulate [_, _] = []
fanTriangulate (pivot : rest) = go rest
  where
    go (a : b : remaining) = pivot : a : b : go (b : remaining)
    go _ = []

-- ----------------------------------------------------------------
-- Multi-object OBJ
-- ----------------------------------------------------------------

-- | Scan OBJ lines to collect per-object face lists. Vertex data
-- is shared; only face assignments are partitioned by @o@ lines.
-- Vertex attribute counts are threaded through the fold so that
-- negative OBJ indices can be resolved correctly.
collectObjects :: [String] -> [(String, [[(Int, Int, Int)]])]
collectObjects allLines =
  let (finalName, finalFaces, acc, _, _, _) =
        foldl' scanLine (defaultObjectName, [], [], 0, 0, 0) allLines
      -- Don't forget the last object
      complete = reverse ((finalName, reverse finalFaces) : acc)
   in complete
  where
    scanLine (curName, curFaces, accObjs, nPos, nUV, nNrm) line =
      case words line of
        ("v" : rest) -> case parseFloats rest of
          [_, _, _] -> (curName, curFaces, accObjs, nPos + 1, nUV, nNrm)
          _ -> (curName, curFaces, accObjs, nPos, nUV, nNrm)
        ("vt" : rest) -> case parseFloats rest of
          (_ : _ : _) -> (curName, curFaces, accObjs, nPos, nUV + 1, nNrm)
          [_] -> (curName, curFaces, accObjs, nPos, nUV + 1, nNrm)
          _ -> (curName, curFaces, accObjs, nPos, nUV, nNrm)
        ("vn" : rest) -> case parseFloats rest of
          [_, _, _] -> (curName, curFaces, accObjs, nPos, nUV, nNrm + 1)
          _ -> (curName, curFaces, accObjs, nPos, nUV, nNrm)
        ("o" : nameWords) ->
          let newName = unwords nameWords
           in (newName, [], (curName, reverse curFaces) : accObjs, nPos, nUV, nNrm)
        ("f" : rest) ->
          let verts = map (parseFaceVertex nPos nUV nNrm) rest
           in (curName, verts : curFaces, accObjs, nPos, nUV, nNrm)
        _ -> (curName, curFaces, accObjs, nPos, nUV, nNrm)

-- ================================================================
-- glTF 2.0
-- ================================================================

-- ----------------------------------------------------------------
-- Public glTF API
-- ----------------------------------------------------------------

-- | Parse a glTF 2.0 JSON string with an embedded base64 buffer
-- into a 'Mesh'.
--
-- Handles the format produced by 'NovaEngine.Mesh.Export.meshToGLTF': a
-- single buffer with data URI, accessors for POSITION, NORMAL,
-- TEXCOORD_0 and indices. Returns 'Nothing' if the JSON is
-- malformed or required fields are missing.
parseGLTF :: String -> Maybe Mesh
parseGLTF input = do
  json <- parseJSON (dropWhile isSpace input)
  -- Decode the base64 buffer
  bufferBytes <- extractBufferBytes json
  -- Get the first mesh primitive
  (posAccIdx, nrmAccIdx, texAccIdx, tanAccIdx, indAccIdx) <- extractPrimitiveAccessors json
  -- Get all accessors and buffer views
  accessors <- getJArray =<< jLookup "accessors" json
  bufferViews <- getJArray =<< jLookup "bufferViews" json
  parsePrimitive bufferBytes accessors bufferViews posAccIdx nrmAccIdx texAccIdx tanAccIdx indAccIdx

-- | Parse all mesh primitives from a glTF 2.0 JSON string into a
-- list of 'Mesh' values. Returns an empty list if the JSON is
-- malformed or no primitives are found.
parseManyGLTF :: String -> [Mesh]
parseManyGLTF input = case parseJSON (dropWhile isSpace input) of
  Nothing -> []
  Just json -> case extractBufferBytes json of
    Nothing -> []
    Just bufferBytes ->
      let mAccessors = getJArray =<< jLookup "accessors" json
          mBufViews = getJArray =<< jLookup "bufferViews" json
       in case (mAccessors, mBufViews) of
            (Just accessors, Just bufferViews) ->
              case getJArray =<< jLookup "meshes" json of
                Nothing -> []
                Just meshesArr -> concatMap (parseMeshPrims bufferBytes accessors bufferViews) meshesArr
            _ -> []
  where
    parseMeshPrims bufferBytes accessors bufferViews mesh =
      case getJArray =<< jLookup "primitives" mesh of
        Nothing -> []
        Just prims ->
          [ m
          | prim <- prims,
            Just (posI, nrmI, texI, tanI, indI) <- [extractPrimAccessors prim],
            Just m <- [parsePrimitive bufferBytes accessors bufferViews posI nrmI texI tanI indI]
          ]

-- | Parse a single glTF primitive given its accessor indices.
parsePrimitive ::
  [Word8] -> [JValue] -> [JValue] -> Int -> Int -> Int -> Int -> Int -> Maybe Mesh
parsePrimitive bufferBytes accessors bufferViews posAccIdx nrmAccIdx texAccIdx tanAccIdx indAccIdx = do
  -- Read vertex positions
  positions <- readVec3Accessor bufferBytes accessors bufferViews posAccIdx
  -- Read normals (optional, default up)
  let normals = case readVec3Accessor bufferBytes accessors bufferViews nrmAccIdx of
        Just ns -> ns
        Nothing -> replicate (length positions) defaultNormal
  -- Read UVs (optional, default zero)
  let uvs = case readVec2Accessor bufferBytes accessors bufferViews texAccIdx of
        Just us -> us
        Nothing -> replicate (length positions) defaultUV
  -- Read tangents (optional, default (1,0,0,1))
  let tangents = case readVec4Accessor bufferBytes accessors bufferViews tanAccIdx of
        Just ts -> ts
        Nothing -> replicate (length positions) defaultTangent
  -- Read indices
  indices <- readScalarAccessor bufferBytes accessors bufferViews indAccIdx
  let verts =
        [ vertex p n uv t
        | (p, n, uv, t) <- zip4Safe positions normals uvs tangents
        ]
  pure (mkMesh verts (map fromIntegral indices))

-- ----------------------------------------------------------------
-- glTF buffer extraction
-- ----------------------------------------------------------------

-- | Extract and decode the base64 buffer from a glTF JSON root.
extractBufferBytes :: JValue -> Maybe [Word8]
extractBufferBytes root = do
  buffersArr <- getJArray =<< jLookup "buffers" root
  firstBuffer <- safeHead buffersArr
  uriVal <- jLookup "uri" firstBuffer
  uriStr <- getJString uriVal
  pure (decodeBase64URI uriStr)

-- | Extract accessor indices for a single primitive.
extractPrimAccessors :: JValue -> Maybe (Int, Int, Int, Int, Int)
extractPrimAccessors prim = do
  attrs <- jLookup "attributes" prim
  posIdx <- getJInt =<< jLookup "POSITION" attrs
  let nrmIdx = fromMaybe missingIndex (getJInt =<< jLookup "NORMAL" attrs)
  let texIdx = fromMaybe missingIndex (getJInt =<< jLookup "TEXCOORD_0" attrs)
  let tanIdx = fromMaybe missingIndex (getJInt =<< jLookup "TANGENT" attrs)
  indIdx <- getJInt =<< jLookup "indices" prim
  pure (posIdx, nrmIdx, texIdx, tanIdx, indIdx)

-- | Extract accessor indices for the first primitive of the first
-- mesh.
extractPrimitiveAccessors :: JValue -> Maybe (Int, Int, Int, Int, Int)
extractPrimitiveAccessors root = do
  meshesArr <- getJArray =<< jLookup "meshes" root
  firstMesh <- safeHead meshesArr
  primitivesArr <- getJArray =<< jLookup "primitives" firstMesh
  firstPrim <- safeHead primitivesArr
  extractPrimAccessors firstPrim

-- ----------------------------------------------------------------
-- glTF accessor reading
-- ----------------------------------------------------------------

-- | Read a VEC3 float accessor from the buffer.
readVec3Accessor ::
  [Word8] -> [JValue] -> [JValue] -> Int -> Maybe [V3]
readVec3Accessor buffer accessors bufferViews accIdx = do
  acc <- safeIndex accessors accIdx
  count <- getJInt =<< jLookup "count" acc
  bvIdx <- getJInt =<< jLookup "bufferView" acc
  bv <- safeIndex bufferViews bvIdx
  byteOffset <- getJInt =<< jLookup "byteOffset" bv
  let bytes = drop byteOffset buffer
  pure (readVec3s count bytes)

-- | Read a VEC2 float accessor from the buffer.
readVec2Accessor ::
  [Word8] -> [JValue] -> [JValue] -> Int -> Maybe [V2]
readVec2Accessor buffer accessors bufferViews accIdx = do
  acc <- safeIndex accessors accIdx
  count <- getJInt =<< jLookup "count" acc
  bvIdx <- getJInt =<< jLookup "bufferView" acc
  bv <- safeIndex bufferViews bvIdx
  byteOffset <- getJInt =<< jLookup "byteOffset" bv
  let bytes = drop byteOffset buffer
  pure (readVec2s count bytes)

-- | Read a VEC4 float accessor from the buffer.
readVec4Accessor ::
  [Word8] -> [JValue] -> [JValue] -> Int -> Maybe [V4]
readVec4Accessor buffer accessors bufferViews accIdx = do
  acc <- safeIndex accessors accIdx
  count <- getJInt =<< jLookup "count" acc
  bvIdx <- getJInt =<< jLookup "bufferView" acc
  bv <- safeIndex bufferViews bvIdx
  byteOffset <- getJInt =<< jLookup "byteOffset" bv
  let bytes = drop byteOffset buffer
  pure (readVec4s count bytes)

-- | Read a SCALAR unsigned int accessor from the buffer.
readScalarAccessor ::
  [Word8] -> [JValue] -> [JValue] -> Int -> Maybe [Word32]
readScalarAccessor buffer accessors bufferViews accIdx = do
  acc <- safeIndex accessors accIdx
  count <- getJInt =<< jLookup "count" acc
  bvIdx <- getJInt =<< jLookup "bufferView" acc
  bv <- safeIndex bufferViews bvIdx
  byteOffset <- getJInt =<< jLookup "byteOffset" bv
  let bytes = drop byteOffset buffer
  pure (readWord32s count bytes)

-- ----------------------------------------------------------------
-- Binary decoding
-- ----------------------------------------------------------------

-- | Read @n@ VEC3 values (3 little-endian floats each) from a byte
-- list.
readVec3s :: Int -> [Word8] -> [V3]
readVec3s 0 _ = []
readVec3s n (b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : b9 : b10 : b11 : rest) =
  let x = bytesToFloat b0 b1 b2 b3
      y = bytesToFloat b4 b5 b6 b7
      z = bytesToFloat b8 b9 b10 b11
   in V3 x y z : readVec3s (n - 1) rest
readVec3s _ _ = []

-- | Read @n@ VEC2 values (2 little-endian floats each) from a byte
-- list.
readVec2s :: Int -> [Word8] -> [V2]
readVec2s 0 _ = []
readVec2s n (b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : rest) =
  let u = bytesToFloat b0 b1 b2 b3
      v = bytesToFloat b4 b5 b6 b7
   in V2 u v : readVec2s (n - 1) rest
readVec2s _ _ = []

-- | Read @n@ VEC4 values (4 little-endian floats each) from a byte
-- list.
readVec4s :: Int -> [Word8] -> [V4]
readVec4s 0 _ = []
readVec4s n (b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : b9 : b10 : b11 : b12 : b13 : b14 : b15 : rest) =
  let x = bytesToFloat b0 b1 b2 b3
      y = bytesToFloat b4 b5 b6 b7
      z = bytesToFloat b8 b9 b10 b11
      w = bytesToFloat b12 b13 b14 b15
   in V4 x y z w : readVec4s (n - 1) rest
readVec4s _ _ = []

-- | Read @n@ SCALAR Word32 values (4 little-endian bytes each)
-- from a byte list.
readWord32s :: Int -> [Word8] -> [Word32]
readWord32s 0 _ = []
readWord32s n (b0 : b1 : b2 : b3 : rest) =
  bytesToWord32 b0 b1 b2 b3 : readWord32s (n - 1) rest
readWord32s _ _ = []

-- | Interpret 4 little-endian bytes as an IEEE 754 float.
bytesToFloat :: Word8 -> Word8 -> Word8 -> Word8 -> Float
bytesToFloat b0 b1 b2 b3 = castWord32ToFloat (bytesToWord32 b0 b1 b2 b3)

-- | Assemble 4 little-endian bytes into a 'Word32'.
bytesToWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
bytesToWord32 b0 b1 b2 b3 =
  fromIntegral b0
    .|. shiftL (fromIntegral b1) 8
    .|. shiftL (fromIntegral b2) 16
    .|. shiftL (fromIntegral b3) 24

-- ----------------------------------------------------------------
-- Base64 decoding
-- ----------------------------------------------------------------

-- | Decode a data URI with base64 content. Strips the
-- @data:...;base64,@ prefix before decoding.
decodeBase64URI :: String -> [Word8]
decodeBase64URI uri =
  let afterComma = dropWhile (/= ',') uri
   in case afterComma of
        (',' : b64) -> decodeBase64 b64
        _ -> decodeBase64 uri

-- | Decode a base64 string (RFC 4648) to bytes.
decodeBase64 :: String -> [Word8]
decodeBase64 = go
  where
    go (c0 : c1 : c2 : c3 : rest) =
      let b0 = b64Lookup c0
          b1 = b64Lookup c1
          b2 = b64Lookup c2
          b3 = b64Lookup c3
       in if c3 == '='
            then
              if c2 == '='
                then
                  -- Two padding chars: one output byte
                  let combined = shiftL b0 2 .|. shiftR b1 4
                   in [fromIntegral combined]
                else
                  -- One padding char: two output bytes
                  let byte0 = shiftL b0 2 .|. shiftR b1 4
                      byte1 = shiftL (b1 .&. 0x0F) 4 .|. shiftR b2 2
                   in [fromIntegral byte0, fromIntegral byte1]
            else
              let byte0 = shiftL b0 2 .|. shiftR b1 4
                  byte1 = shiftL (b1 .&. 0x0F) 4 .|. shiftR b2 2
                  byte2 = shiftL (b2 .&. 0x03) 6 .|. b3
               in fromIntegral byte0 : fromIntegral byte1 : fromIntegral byte2 : go rest
    go _ = []

-- | Look up the 6-bit value for a base64 character.
b64Lookup :: Char -> Word32
b64Lookup c
  | isAsciiUpper c = fromIntegral (ord c - ord 'A')
  | isAsciiLower c = fromIntegral (ord c - ord 'a' + 26)
  | isDigit c = fromIntegral (ord c - ord '0' + 52)
  | c == '+' = 62
  | c == '/' = 63
  | otherwise = 0

-- ================================================================
-- Minimal JSON parser
-- ================================================================

-- | A simple JSON value representation sufficient for parsing
-- glTF 2.0.
data JValue
  = JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber !Float
  | JBool !Bool
  | JNull
  deriving (Show, Eq)

-- | Parse a JSON string into a 'JValue'. Returns 'Nothing' on
-- parse failure.
parseJSON :: String -> Maybe JValue
parseJSON input =
  case parseValue (dropSpaces input) of
    Just (val, _) -> Just val
    Nothing -> Nothing

-- | Parse a JSON value and return the unconsumed remainder.
parseValue :: String -> Maybe (JValue, String)
parseValue [] = Nothing
parseValue ('{' : rest) = parseObject rest
parseValue ('[' : rest) = parseArray rest
parseValue ('"' : rest) = do
  (s, remaining) <- parseString rest
  pure (JString s, remaining)
parseValue ('t' : 'r' : 'u' : 'e' : rest) = Just (JBool True, rest)
parseValue ('f' : 'a' : 'l' : 's' : 'e' : rest) = Just (JBool False, rest)
parseValue ('n' : 'u' : 'l' : 'l' : rest) = Just (JNull, rest)
parseValue input@(c : _)
  | c == '-' || isDigit c = parseNumber input
  | otherwise = Nothing

-- | Parse a JSON object (after the opening brace).
parseObject :: String -> Maybe (JValue, String)
parseObject input =
  let s = dropSpaces input
   in case s of
        ('}' : rest) -> Just (JObject [], rest)
        _ -> do
          (pairs, remaining) <- parsePairs s
          pure (JObject pairs, remaining)
  where
    parsePairs s = do
      (key, afterKey) <- parseQuotedKey (dropSpaces s)
      afterColon <- expectChar ':' (dropSpaces afterKey)
      (val, afterVal) <- parseValue (dropSpaces afterColon)
      let rest = dropSpaces afterVal
      case rest of
        (',' : more) -> do
          (morePairs, final) <- parsePairs (dropSpaces more)
          pure ((key, val) : morePairs, final)
        ('}' : final) -> pure ([(key, val)], final)
        _ -> Nothing

-- | Parse a JSON array (after the opening bracket).
parseArray :: String -> Maybe (JValue, String)
parseArray input =
  let s = dropSpaces input
   in case s of
        (']' : rest) -> Just (JArray [], rest)
        _ -> do
          (elems, remaining) <- parseElements s
          pure (JArray elems, remaining)
  where
    parseElements s = do
      (val, afterVal) <- parseValue (dropSpaces s)
      let rest = dropSpaces afterVal
      case rest of
        (',' : more) -> do
          (moreElems, final) <- parseElements (dropSpaces more)
          pure (val : moreElems, final)
        (']' : final) -> pure ([val], final)
        _ -> Nothing

-- | Parse a JSON string (after the opening quote). Handles basic
-- escape sequences.
parseString :: String -> Maybe (String, String)
parseString = go []
  where
    go acc ('"' : rest) = Just (reverse acc, rest)
    go acc ('\\' : '"' : rest) = go ('"' : acc) rest
    go acc ('\\' : '\\' : rest) = go ('\\' : acc) rest
    go acc ('\\' : '/' : rest) = go ('/' : acc) rest
    go acc ('\\' : 'n' : rest) = go ('\n' : acc) rest
    go acc ('\\' : 'r' : rest) = go ('\r' : acc) rest
    go acc ('\\' : 't' : rest) = go ('\t' : acc) rest
    go acc ('\\' : 'u' : h3 : h2 : h1 : h0 : rest) =
      go (chr (hexVal h3 * 4096 + hexVal h2 * 256 + hexVal h1 * 16 + hexVal h0) : acc) rest
    go acc (c : rest) = go (c : acc) rest
    go _ [] = Nothing

-- | Parse a JSON number (integer or floating point).
parseNumber :: String -> Maybe (JValue, String)
parseNumber input =
  let (numStr, rest) = span isNumChar input
   in case numStr of
        [] -> Nothing
        _ -> Just (JNumber (readFloatSafe numStr), rest)
  where
    isNumChar c = isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E'

-- | Parse a quoted key string.
parseQuotedKey :: String -> Maybe (String, String)
parseQuotedKey ('"' : rest) = parseString rest
parseQuotedKey _ = Nothing

-- | Expect a specific character, consuming it.
expectChar :: Char -> String -> Maybe String
expectChar c (x : rest) | c == x = Just rest
expectChar _ _ = Nothing

-- | Drop leading whitespace.
dropSpaces :: String -> String
dropSpaces = dropWhile isSpace

-- | Decode a single hex digit.
hexVal :: Char -> Int
hexVal c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = 0

-- ----------------------------------------------------------------
-- JSON accessors
-- ----------------------------------------------------------------

-- | Look up a key in a JSON object.
jLookup :: String -> JValue -> Maybe JValue
jLookup key (JObject pairs) = lookup key pairs
jLookup _ _ = Nothing

-- | Extract the contents of a JSON array.
getJArray :: JValue -> Maybe [JValue]
getJArray (JArray xs) = Just xs
getJArray _ = Nothing

-- | Extract a JSON string value.
getJString :: JValue -> Maybe String
getJString (JString s) = Just s
getJString _ = Nothing

-- | Extract a JSON number as an 'Int'.
getJInt :: JValue -> Maybe Int
getJInt (JNumber f) = Just (round f)
getJInt _ = Nothing

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Read a 'Float' from a string, returning 0 on failure.
readFloatSafe :: String -> Float
readFloatSafe s = case reads s of
  [(f, "")] -> f
  [(f, _)] -> f
  _ -> 0

-- | Read an 'Int' from a string, returning 0 on failure.
readIntSafe :: String -> Int
readIntSafe s = case reads s of
  [(n, "")] -> n
  [(n, _)] -> n
  _ -> 0

-- | Safe array lookup with bounds checking.
safeArrayLookup :: Array Int a -> Int -> a -> a
safeArrayLookup arr idx def
  | idx < lo || idx > hi = def
  | otherwise = arr ! idx
  where
    (lo, hi) = Array.bounds arr

-- | Create an 'Array' from a list, returning an empty-range array
-- for empty lists.
listArraySafe :: [a] -> Array Int a
listArraySafe [] = Array.listArray (0, -1) []
listArraySafe xs = listArray (0, length xs - 1) xs

-- | Safe head of a list.
safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing

-- | Zip four lists, truncating to the shortest.
zip4Safe :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4Safe (a : restA) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4Safe restA bs cs ds
zip4Safe _ _ _ _ = []

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Sentinel for a missing OBJ index component.
missingIndex :: Int
missingIndex = -1

-- | Default object name for faces that appear before any @o@ line.
defaultObjectName :: String
defaultObjectName = "default"

-- | Default normal used when OBJ normals are absent.
defaultNormal :: V3
defaultNormal = V3 0 1 0

-- | Default UV used when OBJ texture coordinates are absent.
defaultUV :: V2
defaultUV = V2 0 0

-- | Default tangent assigned to imported vertices.
defaultTangent :: V4
defaultTangent = V4 1 0 0 1
