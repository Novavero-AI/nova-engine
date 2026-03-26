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
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))

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

-- ----------------------------------------------------------------
-- Storable instances
-- ----------------------------------------------------------------

instance Storable V2 where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    let fp = castPtr p
    x <- peekElemOff fp 0
    y <- peekElemOff fp 1
    pure (V2 x y)
  poke p (V2 x y) = do
    let fp = castPtr p
    pokeElemOff fp 0 x
    pokeElemOff fp 1 y

instance Storable V3 where
  sizeOf _ = 12
  alignment _ = 4
  peek p = do
    let fp = castPtr p
    x <- peekElemOff fp 0
    y <- peekElemOff fp 1
    z <- peekElemOff fp 2
    pure (V3 x y z)
  poke p (V3 x y z) = do
    let fp = castPtr p
    pokeElemOff fp 0 x
    pokeElemOff fp 1 y
    pokeElemOff fp 2 z

instance Storable V4 where
  sizeOf _ = 16
  alignment _ = 4
  peek p = do
    let fp = castPtr p
    x <- peekElemOff fp 0
    y <- peekElemOff fp 1
    z <- peekElemOff fp 2
    w <- peekElemOff fp 3
    pure (V4 x y z w)
  poke p (V4 x y z w) = do
    let fp = castPtr p
    pokeElemOff fp 0 x
    pokeElemOff fp 1 y
    pokeElemOff fp 2 z
    pokeElemOff fp 3 w

instance Storable Quaternion where
  sizeOf _ = 16
  alignment _ = 4
  peek p = do
    let fp = castPtr p
    w <- peekElemOff fp 0
    x <- peekElemOff fp 1
    y <- peekElemOff fp 2
    z <- peekElemOff fp 3
    pure (Quaternion w (V3 x y z))
  poke p (Quaternion w (V3 x y z)) = do
    let fp = castPtr p
    pokeElemOff fp 0 w
    pokeElemOff fp 1 x
    pokeElemOff fp 2 y
    pokeElemOff fp 3 z

instance Storable M44 where
  sizeOf _ = 64
  alignment _ = 4
  peek p = do
    let fp = castPtr p
    c0 <- peekElemOff fp 0
    c1 <- peekElemOff fp 1
    c2 <- peekElemOff fp 2
    c3 <- peekElemOff fp 3
    pure (M44 c0 c1 c2 c3)
  poke p (M44 c0 c1 c2 c3) = do
    let fp = castPtr p
    pokeElemOff fp 0 c0
    pokeElemOff fp 1 c1
    pokeElemOff fp 2 c2
    pokeElemOff fp 3 c3
