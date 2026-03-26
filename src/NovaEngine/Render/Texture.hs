-- | Vulkan texture creation, mipmaps, and sampling.
--
-- Wraps the C99 @nv_texture_*@ functions.  Creates VMA-backed
-- textures from raw RGBA pixel data or image files (via stb_image).
-- Optionally generates a full mip chain.  Each texture bundles a
-- 'VkImageView' and 'VkSampler' ready for descriptor binding.
--
-- @
-- tex <- createTextureFromFile dev alloc "diffuse.png"
--          True FilterLinear WrapRepeat
-- @
module NovaEngine.Render.Texture
  ( -- * Handle
    Texture,

    -- * Configuration
    TextureFilter (..),
    TextureWrap (..),

    -- * Lifecycle
    createTexture,
    createTextureFromFile,
    destroyTexture,

    -- * Queries
    textureView,
    textureSampler,
    textureWidth,
    textureHeight,
    textureMipLevels,

    -- * Utilities
    calcMipLevels,

    -- * Internal (for other Render modules)
    withTexturePtr,
  )
where

import Data.Bits (shiftR)
import Data.Word (Word32, Word8)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import NovaEngine.Render.Allocator (Allocator, withAllocatorPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan texture (image + view + sampler).
newtype Texture = Texture (ForeignPtr ())

-- ----------------------------------------------------------------
-- Configuration
-- ----------------------------------------------------------------

-- | Texture minification and magnification filter.
data TextureFilter
  = -- | Nearest-neighbor (pixelated).
    FilterNearest
  | -- | Bilinear filtering (smooth).
    FilterLinear
  deriving (Show, Eq)

-- | Texture addressing (wrap) mode.
data TextureWrap
  = -- | Tile the texture.
    WrapRepeat
  | -- | Clamp to edge pixels.
    WrapClamp
  | -- | Mirror and tile.
    WrapMirror
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_texture_create"
  c_nv_texture_create ::
    Ptr () ->
    Ptr () ->
    Word32 ->
    Word32 ->
    Ptr () ->
    CInt ->
    CInt ->
    CInt ->
    IO (Ptr ())

foreign import ccall unsafe "nv_texture_create_from_file"
  c_nv_texture_create_from_file ::
    Ptr () ->
    Ptr () ->
    Ptr () ->
    CInt ->
    CInt ->
    CInt ->
    IO (Ptr ())

foreign import ccall unsafe "&nv_texture_destroy"
  c_nv_texture_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_texture_view"
  c_nv_texture_view :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nv_texture_sampler"
  c_nv_texture_sampler :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nv_texture_width"
  c_nv_texture_width :: Ptr () -> IO Word32

foreign import ccall unsafe "nv_texture_height"
  c_nv_texture_height :: Ptr () -> IO Word32

foreign import ccall unsafe "nv_texture_mip_levels"
  c_nv_texture_mip_levels :: Ptr () -> IO Word32

-- ----------------------------------------------------------------
-- Internal conversions
-- ----------------------------------------------------------------

filterToC :: TextureFilter -> CInt
filterToC FilterNearest = 0
filterToC FilterLinear = 1

wrapToC :: TextureWrap -> CInt
wrapToC WrapRepeat = 0
wrapToC WrapClamp = 1
wrapToC WrapMirror = 2

wrapTexture :: Ptr () -> IO (Maybe Texture)
wrapTexture ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = do
      fptr <- newForeignPtr c_nv_texture_destroy ptr
      pure (Just (Texture fptr))

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a texture from raw RGBA8 pixel data.
--
-- The list must contain exactly @width * height * 4@ bytes
-- (one byte per channel, RGBA order).
--
-- Returns 'Nothing' if image creation, upload, or sampler
-- creation fails.
createTexture ::
  Device ->
  Allocator ->
  Word32 ->
  Word32 ->
  [Word8] ->
  Bool ->
  TextureFilter ->
  TextureWrap ->
  IO (Maybe Texture)
createTexture dev alloc w h pixels mipmaps filt wrap =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withArrayLen pixels $ \_ dataPtr ->
        c_nv_texture_create
          devPtr
          allocPtr
          w
          h
          (castPtr dataPtr)
          (if mipmaps then 1 else 0)
          (filterToC filt)
          (wrapToC wrap)
          >>= wrapTexture

-- | Create a texture by loading an image file.
--
-- Supports PNG, JPG, BMP, TGA, and HDR via stb_image.
-- Returns 'Nothing' if the file cannot be loaded or GPU
-- upload fails.
createTextureFromFile ::
  Device ->
  Allocator ->
  FilePath ->
  Bool ->
  TextureFilter ->
  TextureWrap ->
  IO (Maybe Texture)
createTextureFromFile dev alloc path mipmaps filt wrap =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withCString path $ \cPath ->
        c_nv_texture_create_from_file
          devPtr
          allocPtr
          (castPtr cPath)
          (if mipmaps then 1 else 0)
          (filterToC filt)
          (wrapToC wrap)
          >>= wrapTexture

-- | Destroy the texture and free GPU resources.
destroyTexture :: Texture -> IO ()
destroyTexture (Texture fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the raw @VkImageView@ handle (for descriptor writes).
--
-- The returned pointer is the Vulkan non-dispatchable handle,
-- suitable for passing to 'NovaEngine.Render.Descriptor.descriptorWriteImage'.
textureView :: Texture -> IO (Ptr ())
textureView (Texture fptr) =
  withForeignPtr fptr c_nv_texture_view

-- | Get the raw @VkSampler@ handle (for descriptor writes).
textureSampler :: Texture -> IO (Ptr ())
textureSampler (Texture fptr) =
  withForeignPtr fptr c_nv_texture_sampler

-- | Get texture width in pixels.
textureWidth :: Texture -> IO Word32
textureWidth (Texture fptr) =
  withForeignPtr fptr c_nv_texture_width

-- | Get texture height in pixels.
textureHeight :: Texture -> IO Word32
textureHeight (Texture fptr) =
  withForeignPtr fptr c_nv_texture_height

-- | Get the number of mip levels.
textureMipLevels :: Texture -> IO Word32
textureMipLevels (Texture fptr) =
  withForeignPtr fptr c_nv_texture_mip_levels

-- ----------------------------------------------------------------
-- Utilities
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

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the texture.
withTexturePtr :: Texture -> (Ptr () -> IO a) -> IO a
withTexturePtr (Texture fptr) = withForeignPtr fptr
