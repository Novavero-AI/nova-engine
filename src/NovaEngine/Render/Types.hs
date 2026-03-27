-- | Pure render types shared between the test suite and the FFI layer.
--
-- This module carries no native dependencies and lives in the
-- @nova-pure@ internal library.  The FFI modules in
-- @NovaEngine.Render.*@ re-export everything defined here so
-- the public API is unchanged.
module NovaEngine.Render.Types
  ( -- * Texture handle
    Texture (..),

    -- * Material parameters
    MaterialParams (..),
    defaultParams,
    packParams,

    -- * Material
    Material (..),
    defaultMaterial,

    -- * Skinned vertex
    SkinnedVertex (..),

    -- * Mip levels
    calcMipLevels,
  )
where

import Data.Bits (shiftR)
import Data.Word (Word32)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..), peekByteOff, peekElemOff, pokeByteOff, pokeElemOff)
import NovaEngine.Math.Types (V2 (..), V3, V4 (..))

-- ----------------------------------------------------------------
-- Texture handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan texture (image + view + sampler).
--
-- Constructed by 'NovaEngine.Render.Texture.createTexture' and
-- friends.  This type lives here (rather than in the FFI module)
-- so that 'Material' can reference it without pulling in native
-- dependencies.
newtype Texture = Texture (ForeignPtr ())

-- ----------------------------------------------------------------
-- Material parameters
-- ----------------------------------------------------------------

-- | Scalar PBR parameters.
--
-- Packed to 64 bytes (16 floats) via 'packParams' for push
-- constants at offset 64.
data MaterialParams = MaterialParams
  { paramMetallic :: !Float,
    paramRoughness :: !Float,
    paramAOStrength :: !Float,
    paramAlbedoFactor :: !V4,
    paramEmissiveFactor :: !V4,
    paramUVScale :: !V2,
    paramUVOffset :: !V2
  }
  deriving (Show, Eq)

-- | Sensible defaults: non-metal, medium roughness, full AO,
-- white albedo, no emission, UV identity.
defaultParams :: MaterialParams
defaultParams =
  MaterialParams
    { paramMetallic = 0.0,
      paramRoughness = 0.5,
      paramAOStrength = 1.0,
      paramAlbedoFactor = V4 1 1 1 1,
      paramEmissiveFactor = V4 0 0 0 0,
      paramUVScale = V2 1 1,
      paramUVOffset = V2 0 0
    }

-- | Pack material parameters into 16 floats (64 bytes) matching
-- the push constant layout at offset 64.
--
-- Layout:
--
-- @
-- [0-3]   albedoFactor (RGBA)
-- [4-7]   emissiveFactor (RGB + intensity)
-- [8]     metallic
-- [9]     roughness
-- [10]    aoStrength
-- [11]    _pad
-- [12-13] uvScale
-- [14-15] uvOffset
-- @
packParams :: MaterialParams -> [Float]
packParams p =
  let V4 ar ag ab aa = paramAlbedoFactor p
      V4 er eg eb ei = paramEmissiveFactor p
      V2 su sv = paramUVScale p
      V2 ou ov = paramUVOffset p
   in [ ar,
        ag,
        ab,
        aa,
        er,
        eg,
        eb,
        ei,
        paramMetallic p,
        paramRoughness p,
        paramAOStrength p,
        0, -- padding
        su,
        sv,
        ou,
        ov
      ]

-- ----------------------------------------------------------------
-- Storable MaterialParams
-- ----------------------------------------------------------------

instance Storable MaterialParams where
  sizeOf _ = 64
  alignment _ = 4
  peek p = do
    albedo <- peekByteOff p 0
    emissive <- peekByteOff p 16
    let fp = castPtr p :: Ptr Float
    met <- peekElemOff fp 8
    rough <- peekElemOff fp 9
    ao <- peekElemOff fp 10
    uvS <- peekByteOff p 48
    uvO <- peekByteOff p 56
    pure
      MaterialParams
        { paramMetallic = met,
          paramRoughness = rough,
          paramAOStrength = ao,
          paramAlbedoFactor = albedo,
          paramEmissiveFactor = emissive,
          paramUVScale = uvS,
          paramUVOffset = uvO
        }
  poke p mp = do
    let fp = castPtr p :: Ptr Float
    pokeByteOff p 0 (paramAlbedoFactor mp)
    pokeByteOff p 16 (paramEmissiveFactor mp)
    pokeElemOff fp 8 (paramMetallic mp)
    pokeElemOff fp 9 (paramRoughness mp)
    pokeElemOff fp 10 (paramAOStrength mp)
    pokeElemOff fp 11 (0.0 :: Float)
    pokeByteOff p 48 (paramUVScale mp)
    pokeByteOff p 56 (paramUVOffset mp)

-- ----------------------------------------------------------------
-- Material
-- ----------------------------------------------------------------

-- | A PBR material: optional textures + scalar parameters.
--
-- Missing textures ('Nothing') should be replaced with the
-- appropriate 'NovaEngine.Render.Material.DefaultTextures' entry
-- at bind time.
data Material = Material
  { materialAlbedo :: !(Maybe Texture),
    materialNormal :: !(Maybe Texture),
    materialMetallicRoughness :: !(Maybe Texture),
    materialAO :: !(Maybe Texture),
    materialEmissive :: !(Maybe Texture),
    materialParams :: !MaterialParams
  }

-- | Default material: no textures, default parameters.
defaultMaterial :: Material
defaultMaterial =
  Material
    { materialAlbedo = Nothing,
      materialNormal = Nothing,
      materialMetallicRoughness = Nothing,
      materialAO = Nothing,
      materialEmissive = Nothing,
      materialParams = defaultParams
    }

-- ----------------------------------------------------------------
-- Skinned vertex (80 bytes)
-- ----------------------------------------------------------------

-- | GPU-ready vertex for skeletal animation.
--
-- 80 bytes: position(12) + normal(12) + uv(8) + tangent(16) +
-- boneIndices(16) + boneWeights(16). No color attribute — the
-- skinned shader uses white.
data SkinnedVertex = SkinnedVertex
  { skPosition :: !V3,
    skNormal :: !V3,
    skUV :: !V2,
    skTangent :: !V4,
    skBoneIndices :: !V4,
    skBoneWeights :: !V4
  }
  deriving (Show, Eq)

instance Storable SkinnedVertex where
  sizeOf _ = 80
  alignment _ = 4
  peek p = do
    pos <- peekByteOff p 0
    nrm <- peekByteOff p 12
    uv <- peekByteOff p 24
    tang <- peekByteOff p 32
    idx <- peekByteOff p 48
    wt <- peekByteOff p 64
    pure (SkinnedVertex pos nrm uv tang idx wt)
  poke p (SkinnedVertex pos nrm uv tang idx wt) = do
    pokeByteOff p 0 pos
    pokeByteOff p 12 nrm
    pokeByteOff p 24 uv
    pokeByteOff p 32 tang
    pokeByteOff p 48 idx
    pokeByteOff p 64 wt

-- ----------------------------------------------------------------
-- Mip levels
-- ----------------------------------------------------------------

-- | Compute the number of mip levels for given dimensions.
--
-- @calcMipLevels 256 256 == 9@
--
-- @calcMipLevels 1 1 == 1@
calcMipLevels :: Word32 -> Word32 -> Word32
calcMipLevels w h
  | w == 0 || h == 0 = 1
  | otherwise = go 1 (max w h)
  where
    go !levels 1 = levels
    go !levels d = go (levels + 1) (d `shiftR` 1)
