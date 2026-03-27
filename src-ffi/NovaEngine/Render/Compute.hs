-- | Vulkan compute pipeline.
--
-- Wraps the C99 @nv_compute_*@ functions. Manages a
-- 'VkComputePipeline' with configurable descriptor bindings and
-- push constants. Used for morph target blending, particle
-- simulation, and general-purpose GPU compute.
module NovaEngine.Render.Compute
  ( -- * Handle
    Compute,

    -- * Lifecycle
    createCompute,
    destroyCompute,

    -- * Dispatch
    computeBind,
    computeDispatch,
    computePushConstants,
    computeBindDescriptorSet,

    -- * Queries
    computeSetLayout,
  )
where

import Data.Word (Word32, Word64)
import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, sizeOf)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Frame (Frame, withFramePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan compute pipeline.
newtype Compute = Compute (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_compute_create"
  c_nv_compute_create ::
    Ptr () -> Ptr () -> Word32 -> Word32 -> CString -> IO (Ptr ())

foreign import ccall unsafe "&nv_compute_destroy"
  c_nv_compute_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_compute_bind"
  c_nv_compute_bind :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_compute_dispatch"
  c_nv_compute_dispatch ::
    Ptr () -> Word32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nv_compute_push_constants"
  c_nv_compute_push_constants ::
    Ptr () -> Ptr () -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "nv_compute_bind_descriptor_set"
  c_nv_compute_bind_descriptor_set ::
    Ptr () -> Ptr () -> Word64 -> IO ()

foreign import ccall unsafe "nv_compute_set_layout"
  c_nv_compute_set_layout :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a compute pipeline.
--
-- @bindings@ is a flat list of 'Word32' quads:
-- @(binding, descriptorType, stageFlags, count)@ for each
-- descriptor binding.
--
-- @pushSize@ is the push constant size in bytes (0 for none).
--
-- Returns 'Nothing' on failure.
createCompute ::
  Device -> [Word32] -> Word32 -> FilePath -> IO (Maybe Compute)
createCompute dev bindings pushSize shaderPath =
  withDevicePtr dev $ \devPtr ->
    withArrayLen bindings $ \len dataPtr ->
      withCString shaderPath $ \sp -> do
        let count = fromIntegral len `div` 4
        ptr <- c_nv_compute_create devPtr (castPtr dataPtr) count pushSize sp
        if ptr == nullPtr
          then pure Nothing
          else do
            fptr <- newForeignPtr c_nv_compute_destroy ptr
            pure (Just (Compute fptr))

-- | Destroy the compute pipeline.
destroyCompute :: Compute -> IO ()
destroyCompute (Compute fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Dispatch
-- ----------------------------------------------------------------

-- | Bind the compute pipeline for subsequent dispatches.
computeBind :: Compute -> Frame -> IO ()
computeBind (Compute fptr) fr =
  withForeignPtr fptr $ \compPtr ->
    withFramePtr fr $ c_nv_compute_bind compPtr

-- | Dispatch compute work groups.
computeDispatch :: Frame -> Word32 -> Word32 -> Word32 -> IO ()
computeDispatch fr gx gy gz =
  withFramePtr fr $ \frPtr ->
    c_nv_compute_dispatch frPtr gx gy gz

-- | Push constants for the compute pipeline.
computePushConstants ::
  (Storable a) => Compute -> Frame -> a -> IO ()
computePushConstants (Compute fptr) fr val =
  withForeignPtr fptr $ \compPtr ->
    withFramePtr fr $ \frPtr ->
      with val $ \valPtr ->
        c_nv_compute_push_constants
          compPtr
          frPtr
          (castPtr valPtr)
          (fromIntegral (sizeOf val))

-- | Bind a descriptor set for the compute pipeline.
computeBindDescriptorSet :: Compute -> Frame -> Word64 -> IO ()
computeBindDescriptorSet (Compute fptr) fr set =
  withForeignPtr fptr $ \compPtr ->
    withFramePtr fr $ \frPtr ->
      c_nv_compute_bind_descriptor_set compPtr frPtr set

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the descriptor set layout handle.
computeSetLayout :: Compute -> IO (Ptr ())
computeSetLayout (Compute fptr) =
  withForeignPtr fptr c_nv_compute_set_layout
