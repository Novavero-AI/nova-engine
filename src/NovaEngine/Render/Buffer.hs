-- | Vulkan vertex and index buffer upload.
--
-- Wraps the C99 @nv_buffer_*@ functions.  Uploads mesh data to
-- device-local GPU memory via a VMA-backed staging buffer.
-- Integrates with 'NovaEngine.Mesh.Buffer.packInterleaved' and
-- 'NovaEngine.Mesh.Buffer.packIndices'.
module NovaEngine.Render.Buffer
  ( -- * Handle
    Buffer,

    -- * Lifecycle
    createVertexBuffer,
    createIndexBuffer,
    createHostBuffer,
    createHostStorageBuffer,
    destroyBuffer,

    -- * Host-visible buffer operations
    withMappedBuffer,
    bufferVkHandle,

    -- * Internal (for other Render modules)
    withBufferPtr,
  )
where

import Data.Word (Word32)
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (sizeOf)
import NovaEngine.Render.Allocator (Allocator, withAllocatorPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a device-local Vulkan buffer.
newtype Buffer = Buffer (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_buffer_create_vertex"
  c_nv_buffer_create_vertex ::
    Ptr () -> Ptr () -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nv_buffer_create_index"
  c_nv_buffer_create_index ::
    Ptr () -> Ptr () -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nv_buffer_create_host"
  c_nv_buffer_create_host :: Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "nv_buffer_create_host_storage"
  c_nv_buffer_create_host_storage :: Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "&nv_buffer_destroy"
  c_nv_buffer_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_buffer_map"
  c_nv_buffer_map :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nv_buffer_unmap"
  c_nv_buffer_unmap :: Ptr () -> IO ()

foreign import ccall unsafe "nv_buffer_vk_handle"
  c_nv_buffer_vk_handle :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Upload vertex data to a device-local GPU buffer.
--
-- Accepts the output of 'NovaEngine.Mesh.Buffer.packInterleaved'.
-- Uses a VMA-backed staging buffer for the transfer.
--
-- Returns 'Nothing' if allocation or transfer fails.
createVertexBuffer ::
  Device -> Allocator -> [Float] -> IO (Maybe Buffer)
createVertexBuffer dev alloc floats =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withArrayLen floats $ \len dataPtr -> do
        let bytes =
              fromIntegral len
                * fromIntegral (sizeOf (undefined :: Float))
        ptr <-
          c_nv_buffer_create_vertex
            devPtr
            allocPtr
            (castPtr dataPtr)
            bytes
        wrapBuffer ptr

-- | Upload index data to a device-local GPU buffer.
--
-- Accepts the output of 'NovaEngine.Mesh.Buffer.packIndices'.
-- Uses a VMA-backed staging buffer for the transfer.
--
-- Returns 'Nothing' if allocation or transfer fails.
createIndexBuffer ::
  Device -> Allocator -> [Word32] -> IO (Maybe Buffer)
createIndexBuffer dev alloc indices =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withArrayLen indices $ \len dataPtr -> do
        let bytes =
              fromIntegral len
                * fromIntegral (sizeOf (undefined :: Word32))
        ptr <-
          c_nv_buffer_create_index
            devPtr
            allocPtr
            (castPtr dataPtr)
            bytes
        wrapBuffer ptr

-- | Create a host-visible buffer for per-frame uniform data.
--
-- Use 'withMappedBuffer' to write data each frame.
-- Returns 'Nothing' if allocation fails.
createHostBuffer :: Allocator -> Word32 -> IO (Maybe Buffer)
createHostBuffer alloc byteSize =
  withAllocatorPtr alloc $ \allocPtr -> do
    ptr <- c_nv_buffer_create_host allocPtr byteSize
    wrapBuffer ptr

-- | Create a host-visible storage buffer (SSBO).
--
-- Use for bone matrices, instance data, or other GPU-readable
-- arrays updated per frame. Returns 'Nothing' if allocation fails.
createHostStorageBuffer :: Allocator -> Word32 -> IO (Maybe Buffer)
createHostStorageBuffer alloc byteSize =
  withAllocatorPtr alloc $ \allocPtr -> do
    ptr <- c_nv_buffer_create_host_storage allocPtr byteSize
    wrapBuffer ptr

-- | Destroy the buffer and free GPU memory.
destroyBuffer :: Buffer -> IO ()
destroyBuffer (Buffer fptr) = finalizeForeignPtr fptr

-- | Map a host-visible buffer, run an action with the mapped
-- pointer, then unmap.  The pointer is only valid inside the
-- action.
withMappedBuffer :: Buffer -> (Ptr () -> IO a) -> IO a
withMappedBuffer (Buffer fptr) action =
  withForeignPtr fptr $ \bufPtr -> do
    mapped <- c_nv_buffer_map bufPtr
    result <- action mapped
    c_nv_buffer_unmap bufPtr
    pure result

-- | Get the raw @VkBuffer@ handle (for descriptor writes).
bufferVkHandle :: Buffer -> IO (Ptr ())
bufferVkHandle (Buffer fptr) =
  withForeignPtr fptr c_nv_buffer_vk_handle

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

wrapBuffer :: Ptr () -> IO (Maybe Buffer)
wrapBuffer ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = do
      fptr <- newForeignPtr c_nv_buffer_destroy ptr
      pure (Just (Buffer fptr))

-- | Run an action with the raw C pointer to the buffer.
withBufferPtr :: Buffer -> (Ptr () -> IO a) -> IO a
withBufferPtr (Buffer fptr) = withForeignPtr fptr
