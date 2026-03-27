-- | Cascaded shadow map resources.
--
-- Wraps the C99 @nv_shadow_*@ functions.  Manages a depth-only
-- array texture (4 cascades), render pass, shadow pipeline, and
-- per-cascade framebuffers.
module NovaEngine.Render.Shadow
  ( -- * Handle
    Shadow,

    -- * Lifecycle
    createShadow,
    destroyShadow,

    -- * Pass management
    shadowBeginPass,
    shadowEndPass,

    -- * Queries (for descriptor binding)
    shadowArrayView,
    shadowSampler,
  )
where

import Data.Word (Word32)
import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import NovaEngine.Render.Allocator (Allocator, withAllocatorPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Frame (Frame, withFramePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to cascaded shadow map resources.
newtype Shadow = Shadow (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_shadow_create"
  c_nv_shadow_create ::
    Ptr () -> Ptr () -> Word32 -> Word32 -> CString -> IO (Ptr ())

foreign import ccall unsafe "&nv_shadow_destroy"
  c_nv_shadow_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_shadow_begin_pass"
  c_nv_shadow_begin_pass ::
    Ptr () -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "nv_shadow_end_pass"
  c_nv_shadow_end_pass :: Ptr () -> IO ()

foreign import ccall unsafe "nv_shadow_array_view"
  c_nv_shadow_array_view :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nv_shadow_sampler"
  c_nv_shadow_sampler :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create shadow map resources.
--
-- @depthFormat@ should match the swapchain depth format (typically
-- @VK_FORMAT_D32_SFLOAT@, numeric value 126).
-- @resolution@ is the shadow map size per cascade (e.g. 2048).
-- @vertPath@ is the path to the compiled @shadow.vert.spv@.
--
-- Returns 'Nothing' on failure.
createShadow ::
  Device ->
  Allocator ->
  Word32 ->
  Word32 ->
  FilePath ->
  IO (Maybe Shadow)
createShadow dev alloc depthFormat resolution vertPath =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withCString vertPath $ \pathPtr -> do
        ptr <-
          c_nv_shadow_create
            devPtr
            allocPtr
            depthFormat
            resolution
            pathPtr
        if ptr == nullPtr
          then pure Nothing
          else do
            fptr <- newForeignPtr c_nv_shadow_destroy ptr
            pure (Just (Shadow fptr))

-- | Destroy shadow map resources.
destroyShadow :: Shadow -> IO ()
destroyShadow (Shadow fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Pass management
-- ----------------------------------------------------------------

-- | Begin a shadow render pass for the given cascade (0..3).
--
-- Call between 'frameAcquire' and 'frameBeginRenderPass'.
-- After this, push light VP at offset 0 and model matrix at
-- offset 64 for each draw.
shadowBeginPass :: Shadow -> Frame -> Word32 -> IO ()
shadowBeginPass (Shadow sfptr) fr cascadeIndex =
  withForeignPtr sfptr $ \shadowPtr ->
    withFramePtr fr $ \frPtr ->
      c_nv_shadow_begin_pass shadowPtr frPtr cascadeIndex

-- | End the shadow render pass for the current cascade.
shadowEndPass :: Frame -> IO ()
shadowEndPass fr =
  withFramePtr fr c_nv_shadow_end_pass

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the array image view for descriptor binding.
shadowArrayView :: Shadow -> IO (Ptr ())
shadowArrayView (Shadow fptr) =
  withForeignPtr fptr c_nv_shadow_array_view

-- | Get the comparison sampler for descriptor binding.
shadowSampler :: Shadow -> IO (Ptr ())
shadowSampler (Shadow fptr) =
  withForeignPtr fptr c_nv_shadow_sampler
