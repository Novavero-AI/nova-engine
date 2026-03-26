-- | Vulkan instance, surface, and physical device management.
--
-- Wraps the C99 @nv_instance_*@ functions.  Creates a 'VkInstance'
-- with SDL3 extensions, an optional debug messenger, a Vulkan
-- surface, and selects the best available GPU.
module NovaEngine.Render.Instance
  ( -- * Handle
    Instance,

    -- * Lifecycle
    createInstance,
    destroyInstance,

    -- * Queries
    deviceName,

    -- * Internal (for other Render modules)
    withInstancePtr,
  )
where

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..))
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

-- | Opaque handle to a Vulkan instance with surface and selected GPU.
newtype Instance = Instance (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_instance_create"
  c_nv_instance_create :: Ptr () -> CString -> CInt -> IO (Ptr ())

foreign import ccall unsafe "&nv_instance_destroy"
  c_nv_instance_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_instance_device_name"
  c_nv_instance_device_name :: Ptr () -> IO CString

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a Vulkan instance, surface, and select a physical device.
--
-- When @enableValidation@ is 'True', enables the Khronos
-- validation layer and a debug messenger that prints warnings and
-- errors to @stderr@.
--
-- Returns 'Nothing' if Vulkan initialisation or GPU selection
-- fails.
createInstance :: Window -> String -> Bool -> IO (Maybe Instance)
createInstance win appName enableValidation =
  withWindowPtr win $ \winPtr ->
    withCString appName $ \cName -> do
      let validation = if enableValidation then 1 else 0
      ptr <- c_nv_instance_create winPtr cName validation
      if ptr == nullPtr
        then pure Nothing
        else do
          fptr <- newForeignPtr c_nv_instance_destroy ptr
          pure (Just (Instance fptr))

-- | Destroy the surface, debug messenger, and Vulkan instance.
destroyInstance :: Instance -> IO ()
destroyInstance (Instance fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Name of the selected GPU (e.g. @\"Apple M2 Pro\"@).
deviceName :: Instance -> IO String
deviceName (Instance fptr) =
  withForeignPtr fptr $ \ptr -> do
    cstr <- c_nv_instance_device_name ptr
    peekCString cstr

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the instance.
withInstancePtr :: Instance -> (Ptr () -> IO a) -> IO a
withInstancePtr (Instance fptr) = withForeignPtr fptr
