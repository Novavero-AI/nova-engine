-- | Vulkan render pass, graphics pipeline, and framebuffers.
--
-- Wraps the C99 @nv_pipeline_*@ functions.  Creates a render pass
-- (color + depth), a graphics pipeline matching the 64-byte
-- 'Vertex' layout with push-constant MVP, and per-swapchain-image
-- framebuffers.
module NovaEngine.Render.Pipeline
  ( -- * Handle
    Pipeline,

    -- * Lifecycle
    createPipeline,
    destroyPipeline,
    recreateFramebuffers,

    -- * Constants
    pushConstantSize,

    -- * Internal (for other Render modules)
    withPipelinePtr,
  )
where

import Data.Word (Word32, Word64)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Swapchain (Swapchain, withSwapchainPtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan graphics pipeline with render pass
-- and framebuffers.
newtype Pipeline = Pipeline (ForeignPtr ())

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Size of the push constant block in bytes (one mat4 MVP = 64).
pushConstantSize :: Int
pushConstantSize = 64

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_pipeline_create"
  c_nv_pipeline_create ::
    Ptr () -> Ptr () -> CString -> CString -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "&nv_pipeline_destroy"
  c_nv_pipeline_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_pipeline_recreate_framebuffers"
  c_nv_pipeline_recreate_framebuffers ::
    Ptr () -> Ptr () -> IO CInt

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a render pass, graphics pipeline, and framebuffers.
--
-- Takes paths to compiled SPIR-V vertex and fragment shaders,
-- and an optional list of raw @VkDescriptorSetLayout@ handles
-- (as 'Word64') to include in the pipeline layout.
-- Returns 'Nothing' if any Vulkan object creation fails or the
-- shader files cannot be read.
createPipeline ::
  Device -> Swapchain -> FilePath -> FilePath -> [Word64] -> IO (Maybe Pipeline)
createPipeline dev sc vertPath fragPath layouts =
  withDevicePtr dev $ \devPtr ->
    withSwapchainPtr sc $ \scPtr ->
      withCString vertPath $ \cVert ->
        withCString fragPath $ \cFrag ->
          withArrayLen layouts $ \len layoutPtr -> do
            ptr <-
              c_nv_pipeline_create
                devPtr
                scPtr
                cVert
                cFrag
                (castPtr layoutPtr)
                (fromIntegral len)
            if ptr == nullPtr
              then pure Nothing
              else do
                fptr <- newForeignPtr c_nv_pipeline_destroy ptr
                pure (Just (Pipeline fptr))

-- | Destroy the pipeline, render pass, and framebuffers.
destroyPipeline :: Pipeline -> IO ()
destroyPipeline (Pipeline fptr) = finalizeForeignPtr fptr

-- | Recreate framebuffers after a swapchain resize.
--
-- The render pass and pipeline are unaffected (dynamic viewport).
recreateFramebuffers :: Pipeline -> Swapchain -> IO Bool
recreateFramebuffers (Pipeline fptr) sc =
  withForeignPtr fptr $ \pipPtr ->
    withSwapchainPtr sc $ \scPtr -> do
      r <- c_nv_pipeline_recreate_framebuffers pipPtr scPtr
      pure (r /= 0)

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the pipeline.
withPipelinePtr :: Pipeline -> (Ptr () -> IO a) -> IO a
withPipelinePtr (Pipeline fptr) = withForeignPtr fptr
