-- | HDR framebuffer, bloom, ACES tonemapping, and FXAA.
--
-- Wraps the C99 @nv_postprocess_*@ functions.  Manages the full
-- post-processing chain: an RGBA16F HDR colour target, a 5-level
-- bloom mip chain (downsample + upsample), and a final
-- tonemap+FXAA pass to the swapchain.
module NovaEngine.Render.PostProcess
  ( -- * Handle
    PostProcess,

    -- * Lifecycle
    createPostProcess,
    destroyPostProcess,
    recreatePostProcess,

    -- * Pass management
    postProcessBeginHdrPass,
    postProcessEndHdrPass,
    postProcessRecord,

    -- * Queries
    postProcessHdrRenderPass,
  )
where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
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
import NovaEngine.Render.Pipeline (Pipeline, withPipelinePtr)
import NovaEngine.Render.Swapchain (Swapchain, withSwapchainPtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to post-processing resources.
newtype PostProcess = PostProcess (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_postprocess_create"
  c_nv_postprocess_create ::
    Ptr () ->
    Ptr () ->
    Ptr () ->
    CString ->
    CString ->
    CString ->
    CString ->
    IO (Ptr ())

foreign import ccall unsafe "&nv_postprocess_destroy"
  c_nv_postprocess_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_postprocess_recreate"
  c_nv_postprocess_recreate :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_postprocess_begin_hdr_pass"
  c_nv_postprocess_begin_hdr_pass ::
    Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_postprocess_end_hdr_pass"
  c_nv_postprocess_end_hdr_pass :: Ptr () -> IO ()

foreign import ccall unsafe "nv_postprocess_record"
  c_nv_postprocess_record :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_postprocess_hdr_render_pass"
  c_nv_postprocess_hdr_render_pass :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create post-processing resources.
--
-- Takes paths to the four compiled SPIR-V shaders:
-- fullscreen.vert.spv, bloom_downsample.frag.spv,
-- bloom_upsample.frag.spv, tonemap.frag.spv.
--
-- Returns 'Nothing' on failure.
createPostProcess ::
  Device ->
  Allocator ->
  Swapchain ->
  FilePath ->
  FilePath ->
  FilePath ->
  FilePath ->
  IO (Maybe PostProcess)
createPostProcess dev alloc sc vertPath downPath upPath tonemapPath =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withSwapchainPtr sc $ \scPtr ->
        withCString vertPath $ \vp ->
          withCString downPath $ \dp ->
            withCString upPath $ \up ->
              withCString tonemapPath $ \tp -> do
                ptr <- c_nv_postprocess_create devPtr allocPtr scPtr vp dp up tp
                if ptr == nullPtr
                  then pure Nothing
                  else do
                    fptr <- newForeignPtr c_nv_postprocess_destroy ptr
                    pure (Just (PostProcess fptr))

-- | Destroy post-processing resources.
destroyPostProcess :: PostProcess -> IO ()
destroyPostProcess (PostProcess fptr) = finalizeForeignPtr fptr

-- | Recreate size-dependent resources after swapchain resize.
recreatePostProcess :: PostProcess -> Swapchain -> IO Bool
recreatePostProcess (PostProcess fptr) sc =
  withForeignPtr fptr $ \ppPtr ->
    withSwapchainPtr sc $
      fmap (== 1) . c_nv_postprocess_recreate ppPtr

-- ----------------------------------------------------------------
-- Pass management
-- ----------------------------------------------------------------

-- | Begin the HDR render pass (PBR scene draws into this).
--
-- Binds the given PBR pipeline and sets viewport\/scissor.
postProcessBeginHdrPass ::
  PostProcess -> Frame -> Swapchain -> Pipeline -> IO ()
postProcessBeginHdrPass (PostProcess fptr) fr sc pip =
  withForeignPtr fptr $ \ppPtr ->
    withFramePtr fr $ \frPtr ->
      withSwapchainPtr sc $ \scPtr ->
        withPipelinePtr pip $
          c_nv_postprocess_begin_hdr_pass ppPtr frPtr scPtr

-- | End the HDR render pass.
postProcessEndHdrPass :: Frame -> IO ()
postProcessEndHdrPass fr =
  withFramePtr fr c_nv_postprocess_end_hdr_pass

-- | Record the full post-processing chain: bloom downsample (5),
-- bloom upsample (4), tonemap+FXAA (1).
--
-- Call after ending the HDR pass, before 'frameSubmit'.
postProcessRecord :: PostProcess -> Frame -> Swapchain -> IO ()
postProcessRecord (PostProcess fptr) fr sc =
  withForeignPtr fptr $ \ppPtr ->
    withFramePtr fr $ \frPtr ->
      withSwapchainPtr sc $
        c_nv_postprocess_record ppPtr frPtr

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the HDR render pass handle for PBR pipeline creation.
postProcessHdrRenderPass :: PostProcess -> IO (Ptr ())
postProcessHdrRenderPass (PostProcess fptr) =
  withForeignPtr fptr c_nv_postprocess_hdr_render_pass
