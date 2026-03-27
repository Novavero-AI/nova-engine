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
  ( -- * Material (re-exported from Render.Types)
    Material (..),
    defaultMaterial,

    -- * Parameters (re-exported from Render.Types)
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
import NovaEngine.Render.Allocator (Allocator)
import NovaEngine.Render.Device (Device)
import NovaEngine.Render.Texture
  ( TextureFilter (..),
    TextureWrap (..),
    createTexture,
    destroyTexture,
  )
import NovaEngine.Render.Types
  ( Material (..),
    MaterialParams (..),
    Texture,
    defaultMaterial,
    defaultParams,
    packParams,
  )

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
