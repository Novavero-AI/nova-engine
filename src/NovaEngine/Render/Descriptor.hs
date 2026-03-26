-- | Vulkan descriptor set layout, pool, and set allocation.
--
-- Wraps the C99 @nv_descriptor_*@ functions.  Provides creation of
-- descriptor set layouts and pools, set allocation, and write
-- helpers for binding buffers and images.
module NovaEngine.Render.Descriptor
  ( -- * Handles
    DescriptorLayout,
    DescriptorPool,

    -- * Lifecycle
    createDescriptorLayout,
    destroyDescriptorLayout,
    createDescriptorPool,
    destroyDescriptorPool,
    resetDescriptorPool,

    -- * Set allocation
    allocateDescriptorSet,

    -- * Writes
    descriptorWriteBuffer,
    descriptorWriteImage,

    -- * Queries
    descriptorLayoutHandle,

    -- * Internal (for other Render modules)
    withDescriptorLayoutPtr,
    withDescriptorPoolPtr,
  )
where

import Data.Word (Word32, Word64)
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)

-- ----------------------------------------------------------------
-- Handles
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan descriptor set layout.
newtype DescriptorLayout = DescriptorLayout (ForeignPtr ())

-- | Opaque handle to a Vulkan descriptor pool.
newtype DescriptorPool = DescriptorPool (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_descriptor_layout_create"
  c_nv_descriptor_layout_create ::
    Ptr () -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "&nv_descriptor_layout_destroy"
  c_nv_descriptor_layout_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_descriptor_pool_create"
  c_nv_descriptor_pool_create ::
    Ptr () -> Word32 -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "&nv_descriptor_pool_destroy"
  c_nv_descriptor_pool_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_descriptor_pool_reset"
  c_nv_descriptor_pool_reset :: Ptr () -> IO ()

foreign import ccall unsafe "nv_descriptor_set_allocate"
  c_nv_descriptor_set_allocate ::
    Ptr () -> Ptr () -> IO Word64

foreign import ccall unsafe "nv_descriptor_layout_handle"
  c_nv_descriptor_layout_handle :: Ptr () -> IO Word64

foreign import ccall unsafe "nv_descriptor_write_buffer"
  c_nv_descriptor_write_buffer ::
    Ptr () -> Word64 -> Word32 -> Word32 -> Ptr () -> Word64 -> Word64 -> IO ()

foreign import ccall unsafe "nv_descriptor_write_image"
  c_nv_descriptor_write_image ::
    Ptr () -> Word64 -> Word32 -> Word32 -> Ptr () -> Ptr () -> Word32 -> IO ()

-- ----------------------------------------------------------------
-- Layout lifecycle
-- ----------------------------------------------------------------

-- | Create a descriptor set layout.
--
-- Each binding is specified as 4 'Word32' values packed
-- contiguously: (binding index, descriptor type, stage flags,
-- descriptor count).
--
-- Returns 'Nothing' on failure.
createDescriptorLayout ::
  Device -> [Word32] -> IO (Maybe DescriptorLayout)
createDescriptorLayout dev bindingData =
  withDevicePtr dev $ \devPtr ->
    withArrayLen bindingData $ \len dataPtr -> do
      let count = fromIntegral len `div` 4
      ptr <- c_nv_descriptor_layout_create devPtr (castPtr dataPtr) count
      if ptr == nullPtr
        then pure Nothing
        else do
          fptr <- newForeignPtr c_nv_descriptor_layout_destroy ptr
          pure (Just (DescriptorLayout fptr))

-- | Destroy a descriptor set layout.
destroyDescriptorLayout :: DescriptorLayout -> IO ()
destroyDescriptorLayout (DescriptorLayout fptr) =
  finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Pool lifecycle
-- ----------------------------------------------------------------

-- | Create a descriptor pool.
--
-- Pool sizes are specified as pairs of 'Word32': (descriptor type,
-- descriptor count), packed contiguously.
--
-- Returns 'Nothing' on failure.
createDescriptorPool ::
  Device -> Word32 -> [Word32] -> IO (Maybe DescriptorPool)
createDescriptorPool dev maxSets poolSizeData =
  withDevicePtr dev $ \devPtr ->
    withArrayLen poolSizeData $ \len dataPtr -> do
      let count = fromIntegral len `div` 2
      ptr <- c_nv_descriptor_pool_create devPtr maxSets (castPtr dataPtr) count
      if ptr == nullPtr
        then pure Nothing
        else do
          fptr <- newForeignPtr c_nv_descriptor_pool_destroy ptr
          pure (Just (DescriptorPool fptr))

-- | Destroy a descriptor pool (frees all allocated sets).
destroyDescriptorPool :: DescriptorPool -> IO ()
destroyDescriptorPool (DescriptorPool fptr) =
  finalizeForeignPtr fptr

-- | Reset the pool, freeing all descriptor sets.
resetDescriptorPool :: DescriptorPool -> IO ()
resetDescriptorPool (DescriptorPool fptr) =
  withForeignPtr fptr c_nv_descriptor_pool_reset

-- ----------------------------------------------------------------
-- Set allocation
-- ----------------------------------------------------------------

-- | Allocate a single descriptor set from the pool.
--
-- Returns 0 on failure.  The returned 'Word64' is the raw
-- @VkDescriptorSet@ handle (non-dispatchable).
allocateDescriptorSet ::
  DescriptorPool -> DescriptorLayout -> IO Word64
allocateDescriptorSet (DescriptorPool poolFptr) (DescriptorLayout layoutFptr) =
  withForeignPtr poolFptr $ \poolPtr ->
    withForeignPtr layoutFptr $ \layoutPtr ->
      c_nv_descriptor_set_allocate poolPtr layoutPtr

-- ----------------------------------------------------------------
-- Writes
-- ----------------------------------------------------------------

-- | Write a buffer descriptor (UBO or SSBO) to a descriptor set.
--
-- Parameters: device, descriptor set handle, binding index,
-- descriptor type (as Vulkan enum value), buffer, offset, range.
descriptorWriteBuffer ::
  Device -> Word64 -> Word32 -> Word32 -> Ptr () -> Word64 -> Word64 -> IO ()
descriptorWriteBuffer dev set binding descType buffer offset range =
  withDevicePtr dev $ \devPtr ->
    c_nv_descriptor_write_buffer
      devPtr
      set
      binding
      descType
      buffer
      offset
      range

-- | Write an image descriptor (combined image sampler, etc).
--
-- Parameters: device, descriptor set handle, binding index,
-- descriptor type (as Vulkan enum value), image view, sampler,
-- image layout (as Vulkan enum value).
descriptorWriteImage ::
  Device -> Word64 -> Word32 -> Word32 -> Ptr () -> Ptr () -> Word32 -> IO ()
descriptorWriteImage dev set binding descType view sampler imgLayout =
  withDevicePtr dev $ \devPtr ->
    c_nv_descriptor_write_image
      devPtr
      set
      binding
      descType
      view
      sampler
      imgLayout

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the raw @VkDescriptorSetLayout@ handle as a 'Word64'.
--
-- Use this to pass layouts to 'NovaEngine.Render.Pipeline.createPipeline'.
descriptorLayoutHandle :: DescriptorLayout -> IO Word64
descriptorLayoutHandle (DescriptorLayout fptr) =
  withForeignPtr fptr c_nv_descriptor_layout_handle

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the layout.
withDescriptorLayoutPtr :: DescriptorLayout -> (Ptr () -> IO a) -> IO a
withDescriptorLayoutPtr (DescriptorLayout fptr) = withForeignPtr fptr

-- | Run an action with the raw C pointer to the pool.
withDescriptorPoolPtr :: DescriptorPool -> (Ptr () -> IO a) -> IO a
withDescriptorPoolPtr (DescriptorPool fptr) = withForeignPtr fptr
