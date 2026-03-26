-- | Core vector, matrix, and quaternion types.
--
-- Right-handed coordinate system, Y-up. All angles in radians.
module NovaEngine.Math.Types
  ( -- * Vector types
    V2 (..),
    V3 (..),
    V4 (..),
    Quaternion (..),

    -- * Matrix type
    M44 (..),

    -- * Vector space abstraction
    VecSpace (..),

    -- * Re-exports
    Word32,
  )
where

import Data.Word (Word32)

-- | 2D vector. Used for UV coordinates.
data V2 = V2 !Float !Float
  deriving (Show, Eq)

-- | 3D vector. Used for positions and normals.
data V3 = V3 !Float !Float !Float
  deriving (Show, Eq)

-- | 4D vector. Used for tangents with bitangent handedness in w.
data V4 = V4 !Float !Float !Float !Float
  deriving (Show, Eq)

-- | Quaternion for rotations. Scalar part followed by vector part.
data Quaternion = Quaternion !Float !V3
  deriving (Show, Eq)

-- | Column-major 4×4 matrix. Stored as four column vectors.
--
-- @M44 c0 c1 c2 c3@ where each column is @V4 row0 row1 row2 row3@.
-- Column-major matches Vulkan and OpenGL conventions.
data M44 = M44 !V4 !V4 !V4 !V4
  deriving (Show, Eq)

-- | Types supporting addition, subtraction, and scalar multiplication.
-- Allows generic algorithms to work over both 'V2' and 'V3'.
class VecSpace a where
  vzero :: a
  (^+^) :: a -> a -> a
  (^-^) :: a -> a -> a
  (*^) :: Float -> a -> a
  negateV :: a -> a
  negateV a = vzero ^-^ a

infixl 6 ^+^

infixl 6 ^-^

infixl 7 *^

instance VecSpace Float where
  vzero = 0
  (^+^) = (+)
  (^-^) = (-)
  s *^ x = s * x

instance VecSpace V2 where
  vzero = V2 0 0
  V2 x1 y1 ^+^ V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
  V2 x1 y1 ^-^ V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
  s *^ V2 x y = V2 (s * x) (s * y)

instance VecSpace V3 where
  vzero = V3 0 0 0
  V3 x1 y1 z1 ^+^ V3 x2 y2 z2 = V3 (x1 + x2) (y1 + y2) (z1 + z2)
  V3 x1 y1 z1 ^-^ V3 x2 y2 z2 = V3 (x1 - x2) (y1 - y2) (z1 - z2)
  s *^ V3 x y z = V3 (s * x) (s * y) (s * z)

instance VecSpace V4 where
  vzero = V4 0 0 0 0
  V4 x1 y1 z1 w1 ^+^ V4 x2 y2 z2 w2 = V4 (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)
  V4 x1 y1 z1 w1 ^-^ V4 x2 y2 z2 w2 = V4 (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
  s *^ V4 x y z w = V4 (s * x) (s * y) (s * z) (s * w)
