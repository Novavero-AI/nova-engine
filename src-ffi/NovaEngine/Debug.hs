-- | Debug line rendering and GPU timestamp profiling.
--
-- Wraps the C99 @nv_debug_*@ functions. Immediate-mode line API:
-- accumulate lines each frame, flush to draw in one call. GPU
-- timestamps for frame profiling.
module NovaEngine.Debug
  ( -- * Handle
    Debug,

    -- * Lifecycle
    createDebug,
    destroyDebug,

    -- * Line drawing
    debugLine,
    debugBox,
    debugSphere,
    debugFlush,

    -- * GPU timestamps
    debugTimestampReset,
    debugTimestampWrite,
    debugTimestampRead,
  )
where

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import NovaEngine.Math.Types (M44)
import NovaEngine.Render.Allocator (Allocator, withAllocatorPtr)
import NovaEngine.Render.Device (Device, withDevicePtr)
import NovaEngine.Render.Frame (Frame, withFramePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to debug rendering resources.
newtype Debug = Debug (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_debug_create"
  c_nv_debug_create ::
    Ptr () -> Ptr () -> Ptr () -> CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "&nv_debug_destroy"
  c_nv_debug_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_debug_line"
  c_nv_debug_line ::
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()

foreign import ccall unsafe "nv_debug_box"
  c_nv_debug_box ::
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()

foreign import ccall unsafe "nv_debug_sphere"
  c_nv_debug_sphere ::
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CInt ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()

foreign import ccall unsafe "nv_debug_flush"
  c_nv_debug_flush :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_debug_timestamp_reset"
  c_nv_debug_timestamp_reset :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nv_debug_timestamp_write"
  c_nv_debug_timestamp_write :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_debug_timestamp_read"
  c_nv_debug_timestamp_read :: Ptr () -> Ptr CFloat -> IO CInt

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create debug resources.
createDebug ::
  Device -> Allocator -> Ptr () -> FilePath -> FilePath -> IO (Maybe Debug)
createDebug dev alloc renderPass vertPath fragPath =
  withDevicePtr dev $ \devPtr ->
    withAllocatorPtr alloc $ \allocPtr ->
      withCString vertPath $ \vp ->
        withCString fragPath $ \fp -> do
          ptr <- c_nv_debug_create devPtr allocPtr renderPass vp fp
          if ptr == nullPtr
            then pure Nothing
            else do
              fptr <- newForeignPtr c_nv_debug_destroy ptr
              pure (Just (Debug fptr))

-- | Destroy debug resources.
destroyDebug :: Debug -> IO ()
destroyDebug (Debug fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Line drawing
-- ----------------------------------------------------------------

-- | Add a line segment with RGBA color.
debugLine ::
  Debug ->
  (Float, Float, Float) ->
  (Float, Float, Float) ->
  (Float, Float, Float, Float) ->
  IO ()
debugLine (Debug fptr) (x0, y0, z0) (x1, y1, z1) (r, g, b, a) =
  withForeignPtr fptr $ \p ->
    c_nv_debug_line
      p
      (CFloat x0)
      (CFloat y0)
      (CFloat z0)
      (CFloat x1)
      (CFloat y1)
      (CFloat z1)
      (CFloat r)
      (CFloat g)
      (CFloat b)
      (CFloat a)

-- | Add a wireframe box.
debugBox ::
  Debug ->
  (Float, Float, Float) ->
  (Float, Float, Float) ->
  (Float, Float, Float, Float) ->
  IO ()
debugBox (Debug fptr) (cx, cy, cz) (hx, hy, hz) (r, g, b, a) =
  withForeignPtr fptr $ \p ->
    c_nv_debug_box
      p
      (CFloat cx)
      (CFloat cy)
      (CFloat cz)
      (CFloat hx)
      (CFloat hy)
      (CFloat hz)
      (CFloat r)
      (CFloat g)
      (CFloat b)
      (CFloat a)

-- | Add a wireframe sphere.
debugSphere ::
  Debug ->
  (Float, Float, Float) ->
  Float ->
  Int ->
  (Float, Float, Float, Float) ->
  IO ()
debugSphere (Debug fptr) (cx, cy, cz) radius segs (r, g, b, a) =
  withForeignPtr fptr $ \p ->
    c_nv_debug_sphere
      p
      (CFloat cx)
      (CFloat cy)
      (CFloat cz)
      (CFloat radius)
      (fromIntegral segs)
      (CFloat r)
      (CFloat g)
      (CFloat b)
      (CFloat a)

-- | Flush all accumulated lines: upload and draw.
-- Must be called inside an active render pass.
debugFlush :: Debug -> Frame -> M44 -> IO ()
debugFlush (Debug fptr) fr vp =
  withForeignPtr fptr $ \dbgPtr ->
    withFramePtr fr $ \frPtr ->
      with vp $ \vpPtr ->
        c_nv_debug_flush dbgPtr frPtr (castPtr vpPtr)

-- ----------------------------------------------------------------
-- GPU timestamps
-- ----------------------------------------------------------------

-- | Reset timestamps for a new frame.
debugTimestampReset :: Debug -> Frame -> IO ()
debugTimestampReset (Debug fptr) fr =
  withForeignPtr fptr $ \dbgPtr ->
    withFramePtr fr $ c_nv_debug_timestamp_reset dbgPtr

-- | Write a GPU timestamp. Returns the index (0-15) or -1 if full.
debugTimestampWrite :: Debug -> Frame -> IO Int
debugTimestampWrite (Debug fptr) fr =
  withForeignPtr fptr $ \dbgPtr ->
    withFramePtr fr $
      fmap fromIntegral . c_nv_debug_timestamp_write dbgPtr

-- | Read back all timestamp results in nanoseconds.
debugTimestampRead :: Debug -> IO [Float]
debugTimestampRead (Debug fptr) =
  withForeignPtr fptr $ \dbgPtr ->
    allocaArray 16 $ \arr -> do
      count <- c_nv_debug_timestamp_read dbgPtr arr
      if count <= 0
        then pure []
        else map (\(CFloat f) -> f) <$> peekArray (fromIntegral count) arr
