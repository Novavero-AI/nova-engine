-- | SDL3 input polling and action mapping.
--
-- Wraps the C99 @nv_input_*@ functions.  Provides raw keyboard,
-- mouse, and scroll state queries plus a simple action mapping
-- system.
module NovaEngine.Input
  ( -- * Handle
    Input,

    -- * Lifecycle
    createInput,
    destroyInput,

    -- * Per-frame
    inputPoll,
    inputPollWithWindow,

    -- * Keyboard
    inputKeyDown,
    inputKeyPressed,
    inputKeyReleased,

    -- * Mouse
    inputMouseX,
    inputMouseY,
    inputMouseDX,
    inputMouseDY,
    inputScrollX,
    inputScrollY,
    inputMouseDown,
    inputMousePressed,

    -- * Action mapping
    inputBindKey,
    inputBindMouse,
    inputActionActive,
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
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import NovaEngine.Render.Window (Window, withWindowPtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to the input system.
newtype Input = Input (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_input_create"
  c_nv_input_create :: IO (Ptr ())

foreign import ccall unsafe "&nv_input_destroy"
  c_nv_input_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_input_poll"
  c_nv_input_poll :: Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "nv_input_key_down"
  c_nv_input_key_down :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_key_pressed"
  c_nv_input_key_pressed :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_key_released"
  c_nv_input_key_released :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_mouse_x"
  c_nv_input_mouse_x :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_mouse_y"
  c_nv_input_mouse_y :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_mouse_dx"
  c_nv_input_mouse_dx :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_mouse_dy"
  c_nv_input_mouse_dy :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_scroll_x"
  c_nv_input_scroll_x :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_scroll_y"
  c_nv_input_scroll_y :: Ptr () -> IO CFloat

foreign import ccall unsafe "nv_input_mouse_down"
  c_nv_input_mouse_down :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_mouse_pressed"
  c_nv_input_mouse_pressed :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_bind_key"
  c_nv_input_bind_key :: Ptr () -> CString -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_bind_mouse"
  c_nv_input_bind_mouse :: Ptr () -> CString -> CInt -> IO CInt

foreign import ccall unsafe "nv_input_action_active"
  c_nv_input_action_active :: Ptr () -> CString -> IO CInt

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create the input system. Returns 'Nothing' on failure.
createInput :: IO (Maybe Input)
createInput = do
  ptr <- c_nv_input_create
  if ptr == nullPtr
    then pure Nothing
    else do
      fptr <- newForeignPtr c_nv_input_destroy ptr
      pure (Just (Input fptr))

-- | Destroy the input system.
destroyInput :: Input -> IO ()
destroyInput (Input fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Per-frame
-- ----------------------------------------------------------------

-- | Poll SDL3 events and update input state.
-- Returns 'True' to continue, 'False' on quit.
-- Does not forward window events — use 'inputPollWithWindow'
-- instead when both input and window are active.
inputPoll :: Input -> IO Bool
inputPoll (Input fptr) =
  withForeignPtr fptr $ \p ->
    fmap (== 1) (c_nv_input_poll p nullPtr)

-- | Poll SDL3 events, forwarding quit and resize events to the
-- window. Use this instead of calling both 'inputPoll' and
-- 'NovaEngine.Render.Window.pollEvents' — one event poller per
-- frame avoids events being split between consumers.
inputPollWithWindow :: Input -> Window -> IO Bool
inputPollWithWindow (Input fptr) win =
  withForeignPtr fptr $ \p ->
    withWindowPtr win $ \wPtr ->
      fmap (== 1) (c_nv_input_poll p wPtr)

-- ----------------------------------------------------------------
-- Keyboard
-- ----------------------------------------------------------------

-- | Is the key held down? Takes an SDL scancode.
inputKeyDown :: Input -> Int -> IO Bool
inputKeyDown (Input fptr) sc =
  withForeignPtr fptr $ \p ->
    (== 1) <$> c_nv_input_key_down p (fromIntegral sc)

-- | Was the key pressed this frame?
inputKeyPressed :: Input -> Int -> IO Bool
inputKeyPressed (Input fptr) sc =
  withForeignPtr fptr $ \p ->
    (== 1) <$> c_nv_input_key_pressed p (fromIntegral sc)

-- | Was the key released this frame?
inputKeyReleased :: Input -> Int -> IO Bool
inputKeyReleased (Input fptr) sc =
  withForeignPtr fptr $ \p ->
    (== 1) <$> c_nv_input_key_released p (fromIntegral sc)

-- ----------------------------------------------------------------
-- Mouse
-- ----------------------------------------------------------------

-- | Mouse X position (screen pixels).
inputMouseX :: Input -> IO Float
inputMouseX (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_mouse_x

-- | Mouse Y position.
inputMouseY :: Input -> IO Float
inputMouseY (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_mouse_y

-- | Mouse X delta since last frame.
inputMouseDX :: Input -> IO Float
inputMouseDX (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_mouse_dx

-- | Mouse Y delta.
inputMouseDY :: Input -> IO Float
inputMouseDY (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_mouse_dy

-- | Scroll wheel X delta.
inputScrollX :: Input -> IO Float
inputScrollX (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_scroll_x

-- | Scroll wheel Y delta.
inputScrollY :: Input -> IO Float
inputScrollY (Input fptr) =
  withForeignPtr fptr $ fmap realToFrac . c_nv_input_scroll_y

-- | Is a mouse button held? (0=left, 1=right, 2=middle)
inputMouseDown :: Input -> Int -> IO Bool
inputMouseDown (Input fptr) btn =
  withForeignPtr fptr $ \p ->
    (== 1) <$> c_nv_input_mouse_down p (fromIntegral btn)

-- | Was a mouse button pressed this frame?
inputMousePressed :: Input -> Int -> IO Bool
inputMousePressed (Input fptr) btn =
  withForeignPtr fptr $ \p ->
    (== 1) <$> c_nv_input_mouse_pressed p (fromIntegral btn)

-- ----------------------------------------------------------------
-- Action mapping
-- ----------------------------------------------------------------

-- | Bind an action name to a keyboard scancode.
inputBindKey :: Input -> String -> Int -> IO Int
inputBindKey (Input fptr) name sc =
  withForeignPtr fptr $ \p ->
    withCString name $ \n ->
      fromIntegral <$> c_nv_input_bind_key p n (fromIntegral sc)

-- | Bind an action name to a mouse button.
inputBindMouse :: Input -> String -> Int -> IO Int
inputBindMouse (Input fptr) name btn =
  withForeignPtr fptr $ \p ->
    withCString name $ \n ->
      fromIntegral <$> c_nv_input_bind_mouse p n (fromIntegral btn)

-- | Is the named action active this frame?
inputActionActive :: Input -> String -> IO Bool
inputActionActive (Input fptr) name =
  withForeignPtr fptr $ \p ->
    withCString name $
      fmap (== 1) . c_nv_input_action_active p
