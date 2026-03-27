-- | SDL3 window management with Vulkan surface support.
--
-- Wraps the C99 @nv_window_*@ functions.  The 'Window' handle is an
-- opaque 'ForeignPtr' whose finaliser calls @nv_window_destroy@, so
-- explicit cleanup is optional but recommended via 'destroyWindow'.
module NovaEngine.Render.Window
  ( -- * Handle
    Window,

    -- * Lifecycle
    createWindow,
    destroyWindow,

    -- * Per-frame
    pollEvents,
    shouldClose,
    wasResized,

    -- * Queries
    drawableSize,

    -- * Internal (for other Render modules)
    withWindowPtr,
  )
where

import Data.Word (Word32)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to an SDL3 window with Vulkan support.
newtype Window = Window (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_window_create"
  c_nv_window_create :: CString -> Word32 -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "&nv_window_destroy"
  c_nv_window_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_window_poll_events"
  c_nv_window_poll_events :: Ptr () -> IO ()

foreign import ccall unsafe "nv_window_should_close"
  c_nv_window_should_close :: Ptr () -> IO CInt

foreign import ccall unsafe "nv_window_was_resized"
  c_nv_window_was_resized :: Ptr () -> IO CInt

foreign import ccall unsafe "nv_window_drawable_size"
  c_nv_window_drawable_size :: Ptr () -> Ptr Word32 -> Ptr Word32 -> IO ()

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create an SDL3 window configured for Vulkan rendering.
--
-- Initialises the SDL video subsystem and creates a resizable,
-- HiDPI-aware window.  Returns 'Nothing' if SDL or window
-- creation fails.
createWindow :: String -> Word32 -> Word32 -> IO (Maybe Window)
createWindow title width height = do
  ptr <- withCString title $ \cTitle ->
    c_nv_window_create cTitle width height
  if ptr == nullPtr
    then pure Nothing
    else do
      fptr <- newForeignPtr c_nv_window_destroy ptr
      pure (Just (Window fptr))

-- | Destroy the window and shut down SDL.
--
-- Safe to call multiple times — the 'ForeignPtr' ensures the
-- underlying C resource is freed exactly once.
destroyWindow :: Window -> IO ()
destroyWindow (Window fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Per-frame
-- ----------------------------------------------------------------

-- | Poll all pending SDL events.  Call exactly once per frame.
--
-- Updates the 'shouldClose' and 'wasResized' flags.
pollEvents :: Window -> IO ()
pollEvents (Window fptr) =
  withForeignPtr fptr c_nv_window_poll_events

-- | Returns 'True' after a quit event (close button, Cmd-Q, etc.).
shouldClose :: Window -> IO Bool
shouldClose (Window fptr) =
  withForeignPtr fptr $ \ptr -> do
    r <- c_nv_window_should_close ptr
    pure (r /= 0)

-- | Returns 'True' if the window was resized during the last
-- 'pollEvents' call.  Use this to trigger swapchain recreation.
wasResized :: Window -> IO Bool
wasResized (Window fptr) =
  withForeignPtr fptr $ \ptr -> do
    r <- c_nv_window_was_resized ptr
    pure (r /= 0)

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the window.
--
-- For use by other @Render@ modules that need to pass the window
-- handle across the FFI boundary.
withWindowPtr :: Window -> (Ptr () -> IO a) -> IO a
withWindowPtr (Window fptr) = withForeignPtr fptr

-- | Get the drawable size in pixels.
--
-- On HiDPI \/ Retina displays this may be larger than the window
-- size in screen coordinates.
drawableSize :: Window -> IO (Word32, Word32)
drawableSize (Window fptr) =
  withForeignPtr fptr $ \ptr ->
    alloca $ \wPtr ->
      alloca $ \hPtr -> do
        c_nv_window_drawable_size ptr wPtr hPtr
        w <- peek wPtr
        h <- peek hPtr
        pure (w, h)
