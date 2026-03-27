-- | GPU memory allocator backed by VMA.
--
-- Wraps the C99 @nv_allocator_*@ functions.  Creates a VMA-backed
-- allocator from an 'Instance' and 'Device'.  Other render modules
-- accept an 'Allocator' for buffer and image creation.
module NovaEngine.Render.Allocator
  ( -- * Handle
    Allocator,

    -- * Lifecycle
    createAllocator,
    destroyAllocator,

    -- * Internal (for other Render modules)
    withAllocatorPtr,
  )
where

import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Instance (Instance, withInstancePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a VMA-backed GPU memory allocator.
newtype Allocator = Allocator (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_allocator_create"
  c_nv_allocator_create :: Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "&nv_allocator_destroy"
  c_nv_allocator_destroy :: FunPtr (Ptr () -> IO ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a VMA-backed allocator.
--
-- Returns 'Nothing' if VMA initialization fails.
createAllocator :: Instance -> Device -> IO (Maybe Allocator)
createAllocator inst dev =
  withInstancePtr inst $ \instPtr ->
    withDevicePtr dev $ \devPtr -> do
      ptr <- c_nv_allocator_create instPtr devPtr
      if ptr == nullPtr
        then pure Nothing
        else do
          fptr <- newForeignPtr c_nv_allocator_destroy ptr
          pure (Just (Allocator fptr))

-- | Destroy the allocator.  All buffers and images must be freed
-- before calling this.
destroyAllocator :: Allocator -> IO ()
destroyAllocator (Allocator fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the allocator.
withAllocatorPtr :: Allocator -> (Ptr () -> IO a) -> IO a
withAllocatorPtr (Allocator fptr) = withForeignPtr fptr
