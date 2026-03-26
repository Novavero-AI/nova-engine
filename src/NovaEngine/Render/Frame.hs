-- | Per-frame synchronisation, command recording, and presentation.
--
-- Wraps the C99 @nv_frame_*@ functions.  Manages 2 frames in
-- flight with semaphores and fences.  Provides a begin\/end bracket
-- with draw commands in between.
--
-- @
-- result <- frameBegin fr sc pip
-- when (result == 'FrameOk') $ do
--   framePushMVP fr pip mvp
--   frameBindVertexBuffer fr vb
--   frameBindIndexBuffer fr ib
--   frameDrawIndexed fr indexCount
--   frameEnd fr sc
-- @
module NovaEngine.Render.Frame
  ( -- * Handle
    Frame,

    -- * Result
    FrameResult (..),

    -- * Lifecycle
    createFrame,
    destroyFrame,

    -- * Frame bracket
    frameBegin,
    frameEnd,

    -- * Draw commands
    framePushMVP,
    frameBindVertexBuffer,
    frameBindIndexBuffer,
    frameDrawIndexed,
    frameDraw,
    frameBindDescriptorSet,
  )
where

import Data.Word (Word32, Word64)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import NovaEngine.Math.Types (M44)
import NovaEngine.Render.Buffer (Buffer, withBufferPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Pipeline (Pipeline, withPipelinePtr)
import NovaEngine.Render.Swapchain (Swapchain, withSwapchainPtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to per-frame Vulkan resources (command buffers,
-- semaphores, fences).
newtype Frame = Frame (ForeignPtr ())

-- | Result of 'frameBegin' or 'frameEnd'.
data FrameResult
  = -- | Frame completed successfully.
    FrameOk
  | -- | Swapchain is out of date — recreate and retry.
    FrameRecreate
  | -- | Unrecoverable error.
    FrameError
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_frame_create"
  c_nv_frame_create :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "&nv_frame_destroy"
  c_nv_frame_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_frame_begin"
  c_nv_frame_begin :: Ptr () -> Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_frame_end"
  c_nv_frame_end :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_frame_push_constants"
  c_nv_frame_push_constants ::
    Ptr () -> Ptr () -> Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "nv_frame_bind_vertex_buffer"
  c_nv_frame_bind_vertex_buffer :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_frame_bind_index_buffer"
  c_nv_frame_bind_index_buffer :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_frame_draw_indexed"
  c_nv_frame_draw_indexed :: Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "nv_frame_draw"
  c_nv_frame_draw :: Ptr () -> Word32 -> IO ()

foreign import ccall unsafe "nv_frame_bind_descriptor_set"
  c_nv_frame_bind_descriptor_set ::
    Ptr () -> Ptr () -> Word32 -> Word64 -> IO ()

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create per-frame resources (command buffers, sync objects).
createFrame :: Device -> IO (Maybe Frame)
createFrame dev =
  withDevicePtr dev $ \devPtr -> do
    ptr <- c_nv_frame_create devPtr
    if ptr == nullPtr
      then pure Nothing
      else do
        fptr <- newForeignPtr c_nv_frame_destroy ptr
        pure (Just (Frame fptr))

-- | Destroy sync objects and free command buffers.
destroyFrame :: Frame -> IO ()
destroyFrame (Frame fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Frame bracket
-- ----------------------------------------------------------------

toResult :: CInt -> FrameResult
toResult 1 = FrameOk
toResult 0 = FrameRecreate
toResult _ = FrameError

-- | Begin a frame: wait fence, acquire image, begin render pass.
frameBegin :: Frame -> Swapchain -> Pipeline -> IO FrameResult
frameBegin (Frame fptr) sc pip =
  withForeignPtr fptr $ \frPtr ->
    withSwapchainPtr sc $ \scPtr ->
      withPipelinePtr pip $
        fmap toResult . c_nv_frame_begin frPtr scPtr

-- | End a frame: end render pass, submit, present.
frameEnd :: Frame -> Swapchain -> IO FrameResult
frameEnd (Frame fptr) sc =
  withForeignPtr fptr $ \frPtr ->
    withSwapchainPtr sc $
      fmap toResult . c_nv_frame_end frPtr

-- ----------------------------------------------------------------
-- Draw commands (call between frameBegin and frameEnd)
-- ----------------------------------------------------------------

-- | Push an MVP matrix to the vertex shader via push constants.
framePushMVP :: Frame -> Pipeline -> M44 -> IO ()
framePushMVP (Frame fptr) pip mvp =
  withForeignPtr fptr $ \frPtr ->
    withPipelinePtr pip $ \pipPtr ->
      with mvp $ \mvpPtr ->
        c_nv_frame_push_constants frPtr pipPtr (castPtr mvpPtr) 64

-- | Bind a vertex buffer at binding 0.
frameBindVertexBuffer :: Frame -> Buffer -> IO ()
frameBindVertexBuffer (Frame fptr) buf =
  withForeignPtr fptr $ \frPtr ->
    withBufferPtr buf $ \bufPtr ->
      c_nv_frame_bind_vertex_buffer frPtr bufPtr

-- | Bind an index buffer (uint32 indices).
frameBindIndexBuffer :: Frame -> Buffer -> IO ()
frameBindIndexBuffer (Frame fptr) buf =
  withForeignPtr fptr $ \frPtr ->
    withBufferPtr buf $ \bufPtr ->
      c_nv_frame_bind_index_buffer frPtr bufPtr

-- | Issue an indexed draw call.
frameDrawIndexed :: Frame -> Word32 -> IO ()
frameDrawIndexed (Frame fptr) n =
  withForeignPtr fptr $ \frPtr ->
    c_nv_frame_draw_indexed frPtr n

-- | Issue a non-indexed draw call.
frameDraw :: Frame -> Word32 -> IO ()
frameDraw (Frame fptr) n =
  withForeignPtr fptr $ \frPtr ->
    c_nv_frame_draw frPtr n

-- | Bind a descriptor set at the given set index.
--
-- The 'Word64' is a raw @VkDescriptorSet@ handle returned by
-- 'NovaEngine.Render.Descriptor.allocateDescriptorSet'.
frameBindDescriptorSet :: Frame -> Pipeline -> Word32 -> Word64 -> IO ()
frameBindDescriptorSet (Frame fptr) pip setIndex descriptorSet =
  withForeignPtr fptr $ \frPtr ->
    withPipelinePtr pip $ \pipPtr ->
      c_nv_frame_bind_descriptor_set frPtr pipPtr setIndex descriptorSet
