-- | Core mesh types shared across all modules.
--
-- Vertex and mesh representations, mesh combination with index
-- offset arithmetic, and validation predicates. Re-exports math
-- types for convenience so consumer modules need only one import.
module NovaEngine.Mesh.Types
  ( -- * Vector types (re-exported from Math.Types)
    V2 (..),
    V3 (..),
    V4 (..),
    Quaternion (..),
    VecSpace (..),
    Word32,

    -- * V2 operations (re-exported from Math.Vector)
    dot2,
    vlength2,
    vlengthSq2,
    normalize2,
    vlerp2,

    -- * V3 operations (re-exported from Math.Vector)
    dot,
    cross,
    vlength,
    vlengthSq,
    normalize,
    vlerp,
    distanceSq,

    -- * Quaternion operations (re-exported from Math.Quaternion)
    axisAngle,
    mulQuat,
    inverseQuat,
    slerpQuat,
    rotateV3,

    -- * Safe operations (re-exported from Math.Vector)
    safeNormalize,
    pickPerpendicular,

    -- * Generic interpolation (re-exported from Math.Vector)
    lerp,
    lerpFloat,
    pairwiseLerp,

    -- * Constants (re-exported from Math.Vector)
    nearZeroLength,

    -- * Core mesh types
    Vertex (..),
    Mesh (..),
    mkMesh,
    vertex,
    defaultColor,

    -- * Mesh validation
    validateMesh,
    validIndices,
    validTriangleCount,
    validNormals,

    -- * Shared helpers
    safeIndex,
    safeLast,
    identityQuat,
    rootParent,
    clampF,
    groupTriangles,
    fastFloor,
    applyIterations,
  )
where

import Data.List (foldl')
import Foreign.Storable (Storable (..))
import NovaEngine.Math.Quaternion
  ( axisAngle,
    identityQuat,
    inverseQuat,
    mulQuat,
    rotateV3,
    slerpQuat,
  )
import NovaEngine.Math.Types
import NovaEngine.Math.Vector

-- ----------------------------------------------------------------
-- Core mesh types
-- ----------------------------------------------------------------

-- | A vertex with position, normal, UV coordinates, tangent, and
-- colour.
--
-- Tangent w stores bitangent handedness (+1 or −1). The engine
-- reconstructs the bitangent as @cross(normal, tangent.xyz) * tangent.w@.
-- Colour defaults to opaque white via 'vertex'.
data Vertex = Vertex
  { vPosition :: !V3,
    vNormal :: !V3,
    vUV :: !V2,
    vTangent :: !V4,
    vColor :: !V4
  }
  deriving (Show, Eq)

-- | Smart constructor that sets 'vColor' to opaque white.
vertex :: V3 -> V3 -> V2 -> V4 -> Vertex
vertex pos nrm uv tang = Vertex pos nrm uv tang defaultColor

-- | Default vertex colour: opaque white.
defaultColor :: V4
defaultColor = V4 1 1 1 1

-- | An indexed triangle mesh. Every three indices form one triangle.
-- Index values are offsets into the vertex list.
--
-- 'Mesh' is a 'Monoid': 'mempty' is the empty mesh, and '<>' merges
-- meshes with index offset arithmetic.
data Mesh = Mesh
  { meshVertices :: ![Vertex],
    meshIndices :: ![Word32],
    meshVertexCount :: !Int
  }
  deriving (Show, Eq)

-- | Construct a 'Mesh' from vertices and indices, computing the
-- vertex count automatically.
mkMesh :: [Vertex] -> [Word32] -> Mesh
mkMesh vs is = Mesh vs is (length vs)

instance Semigroup Mesh where
  Mesh verticesA indicesA countA <> Mesh verticesB indicesB countB =
    Mesh
      (verticesA ++ verticesB)
      (indicesA ++ map (+ offset) indicesB)
      (countA + countB)
    where
      offset = fromIntegral countA

instance Monoid Mesh where
  mempty = Mesh [] [] 0

-- ----------------------------------------------------------------
-- Mesh validation
-- ----------------------------------------------------------------

-- | Check that a mesh is well-formed: all indices are valid, the
-- index count is divisible by 3, and all normals are approximately
-- unit length (within a tolerance of 0.01).
validateMesh :: Mesh -> Bool
validateMesh mesh =
  validIndices mesh
    && validTriangleCount mesh
    && validNormals validateNormalTolerance mesh

validateNormalTolerance :: Float
validateNormalTolerance = 0.01

-- | Check that all indices reference valid vertex positions.
validIndices :: Mesh -> Bool
validIndices (Mesh _ indices vertexCount) =
  all (\idx -> fromIntegral idx < vertexCount) indices

-- | Check that the index count is divisible by 3 (complete triangles).
validTriangleCount :: Mesh -> Bool
validTriangleCount (Mesh _ indices _) = length indices `mod` 3 == 0

-- | Check that all vertex normals are approximately unit length.
validNormals :: Float -> Mesh -> Bool
validNormals tolerance (Mesh vertices _ _) =
  all isUnitNormal vertices
  where
    isUnitNormal v = abs (vlength (vNormal v) - 1.0) < tolerance

-- ----------------------------------------------------------------
-- Shared helpers
-- ----------------------------------------------------------------

-- | Total safe list indexing.
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x : _) 0 = Just x
safeIndex (_ : rest) n
  | n < 0 = Nothing
  | otherwise = safeIndex rest (n - 1)

-- | Total safe extraction of the last element.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : rest) = safeLast rest

-- | Sentinel value for joints with no parent.
rootParent :: Int
rootParent = -1

-- | Clamp a value to a range.
clampF :: Float -> Float -> Float -> Float
clampF lo hi x = max lo (min hi x)

-- ----------------------------------------------------------------
-- Storable Vertex
-- ----------------------------------------------------------------

instance Storable Vertex where
  sizeOf _ = 64
  alignment _ = 4
  peek p = do
    pos <- peekByteOff p 0
    nrm <- peekByteOff p 12
    uv <- peekByteOff p 24
    tang <- peekByteOff p 32
    col <- peekByteOff p 48
    pure (Vertex pos nrm uv tang col)
  poke p (Vertex pos nrm uv tang col) = do
    pokeByteOff p 0 pos
    pokeByteOff p 12 nrm
    pokeByteOff p 24 uv
    pokeByteOff p 32 tang
    pokeByteOff p 48 col

-- | Group a flat index list into triples representing triangles.
groupTriangles :: [Word32] -> [(Word32, Word32, Word32)]
groupTriangles (a : b : c : rest) = (a, b, c) : groupTriangles rest
groupTriangles _ = []

-- | Fast floor via truncation with correction for negatives.
fastFloor :: Float -> Int
fastFloor x =
  let truncated = truncate x :: Int
   in if fromIntegral truncated > x
        then truncated - 1
        else truncated

-- | Apply a transformation @n@ times.
applyIterations :: Int -> (a -> a) -> a -> a
applyIterations n step x = foldl' (\acc _ -> step acc) x [1 .. n]
