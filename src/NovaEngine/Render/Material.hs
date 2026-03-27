-- | PBR material system.
--
-- A 'Material' bundles up to five textures (albedo, normal,
-- metallic-roughness, AO, emissive) with scalar 'MaterialParams'.
-- Material parameters are packed into 64 bytes for push constants
-- (offset 64–127, after the model matrix).
--
-- @
-- let mat = defaultMaterial
--       { materialAlbedo = Just myTexture
--       , materialParams = defaultParams { paramRoughness = 0.8 }
--       }
--     pushData = packParams (materialParams mat)
-- @
module NovaEngine.Render.Material
  ( -- * Material
    Material (..),
    defaultMaterial,

    -- * Parameters
    MaterialParams (..),
    defaultParams,
    packParams,

    -- * Default textures
    DefaultTextures (..),
    createDefaultTextures,
    destroyDefaultTextures,
  )
where

import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..), peekElemOff, pokeElemOff)
import NovaEngine.Math.Types (V2 (..), V4 (..))
import NovaEngine.Render.Allocator (Allocator)
import NovaEngine.Render.Device (Device)
import NovaEngine.Render.Texture
  ( Texture,
    TextureFilter (..),
    TextureWrap (..),
    createTexture,
    destroyTexture,
  )

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
-- appropriate 'DefaultTextures' entry at bind time.
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
-- Default textures
-- ----------------------------------------------------------------

-- | Pre-created 1×1 fallback textures for missing material slots.
data DefaultTextures = DefaultTextures
  { defWhite :: !Texture,
    defFlatNormal :: !Texture,
    defBlack :: !Texture
  }

-- | Create the three default textures.
--
-- * White (255,255,255,255) — for albedo and AO
-- * Flat normal (128,128,255,255) — tangent-space +Z
-- * Black (0,0,0,255) — for emissive
--
-- Returns 'Nothing' if any creation fails.
createDefaultTextures ::
  Device -> Allocator -> IO (Maybe DefaultTextures)
createDefaultTextures dev alloc = do
  mWhite <- mkSolid [255, 255, 255, 255]
  mNormal <- mkSolid [128, 128, 255, 255]
  mBlack <- mkSolid [0, 0, 0, 255]
  case (mWhite, mNormal, mBlack) of
    (Just w, Just n, Just b) ->
      pure (Just (DefaultTextures w n b))
    _ -> do
      mapM_ (mapM_ destroyTexture) [mWhite, mNormal, mBlack]
      pure Nothing
  where
    mkSolid :: [Word8] -> IO (Maybe Texture)
    mkSolid rgba =
      createTexture dev alloc 1 1 rgba False FilterNearest WrapRepeat

-- | Destroy the default textures.
destroyDefaultTextures :: DefaultTextures -> IO ()
destroyDefaultTextures defs = do
  destroyTexture (defWhite defs)
  destroyTexture (defFlatNormal defs)
  destroyTexture (defBlack defs)
