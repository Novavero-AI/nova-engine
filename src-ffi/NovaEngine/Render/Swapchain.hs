-- | Vulkan swapchain, image views, and depth buffer.
--
-- Wraps the C99 @nv_swapchain_*@ functions.  Creates a swapchain
-- with SRGB format preference, mailbox present mode, and a
-- matching depth buffer.  Supports recreation on window resize.
module NovaEngine.Render.Swapchain
  ( -- * Handle
    Swapchain,

    -- * Lifecycle
    createSwapchain,
    destroySwapchain,
    recreateSwapchain,

    -- * Queries
    swapchainImageCount,
    swapchainWidth,
    swapchainHeight,

    -- * Internal (for other Render modules)
    withSwapchainPtr,
  )
where

import Data.Word (Word32)
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
import NovaEngine.Render.Instance (Instance, withInstancePtr)
import NovaEngine.Render.Window (Window, withWindowPtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan swapchain with image views and
-- depth buffer.
newtype Swapchain = Swapchain (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_swapchain_create"
  c_nv_swapchain_create :: Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "&nv_swapchain_destroy"
  c_nv_swapchain_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_swapchain_recreate"
  c_nv_swapchain_recreate :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_swapchain_image_count"
  c_nv_swapchain_image_count :: Ptr () -> IO Word32

foreign import ccall unsafe "nv_swapchain_width"
  c_nv_swapchain_width :: Ptr () -> IO Word32

foreign import ccall unsafe "nv_swapchain_height"
  c_nv_swapchain_height :: Ptr () -> IO Word32

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a swapchain with image views and depth buffer.
--
-- Prefers B8G8R8A8_SRGB, mailbox present mode, and D32_SFLOAT
-- depth.  Returns 'Nothing' on failure.
createSwapchain ::
  Instance -> Device -> Allocator -> Window -> IO (Maybe Swapchain)
createSwapchain inst dev alloc win =
  withInstancePtr inst $ \instPtr ->
    withDevicePtr dev $ \devPtr ->
      withAllocatorPtr alloc $ \allocPtr ->
        withWindowPtr win $ \winPtr -> do
          ptr <- c_nv_swapchain_create instPtr devPtr allocPtr winPtr
          if ptr == nullPtr
            then pure Nothing
            else do
              fptr <- newForeignPtr c_nv_swapchain_destroy ptr
              pure (Just (Swapchain fptr))

-- | Destroy the swapchain, image views, and depth resources.
destroySwapchain :: Swapchain -> IO ()
destroySwapchain (Swapchain fptr) = finalizeForeignPtr fptr

-- | Recreate the swapchain after a window resize.
--
-- Call 'deviceWaitIdle' before this.  Passes the old swapchain
-- handle to the driver for resource reuse.
recreateSwapchain :: Swapchain -> Window -> IO Bool
recreateSwapchain (Swapchain fptr) win =
  withForeignPtr fptr $ \scPtr ->
    withWindowPtr win $ \winPtr -> do
      r <- c_nv_swapchain_recreate scPtr winPtr
      pure (r /= 0)

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Number of images in the swapchain (typically 2 or 3).
swapchainImageCount :: Swapchain -> IO Word32
swapchainImageCount (Swapchain fptr) =
  withForeignPtr fptr c_nv_swapchain_image_count

-- | Current swapchain width in pixels.
swapchainWidth :: Swapchain -> IO Word32
swapchainWidth (Swapchain fptr) =
  withForeignPtr fptr c_nv_swapchain_width

-- | Current swapchain height in pixels.
swapchainHeight :: Swapchain -> IO Word32
swapchainHeight (Swapchain fptr) =
  withForeignPtr fptr c_nv_swapchain_height

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the swapchain.
withSwapchainPtr :: Swapchain -> (Ptr () -> IO a) -> IO a
withSwapchainPtr (Swapchain fptr) = withForeignPtr fptr
