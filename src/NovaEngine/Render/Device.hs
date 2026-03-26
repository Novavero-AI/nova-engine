-- | Vulkan logical device, queues, and command pool.
--
-- Wraps the C99 @nv_device_*@ functions.  Creates a logical device
-- from the physical device selected by 'Instance', retrieves
-- graphics and present queues, and creates a resettable command
-- pool.
module NovaEngine.Render.Device
  ( -- * Handle
    Device,

    -- * Lifecycle
    createDevice,
    destroyDevice,

    -- * Operations
    deviceWaitIdle,

    -- * Internal (for other Render modules)
    withDevicePtr,
  )
where

import Foreign.ForeignPtr
  ( ForeignPtr,
    finalizeForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import NovaEngine.Render.Instance (Instance, withInstancePtr)

-- ----------------------------------------------------------------
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a Vulkan logical device with queues and
-- command pool.
newtype Device = Device (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_device_create"
  c_nv_device_create :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "&nv_device_destroy"
  c_nv_device_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_device_wait_idle"
  c_nv_device_wait_idle :: Ptr () -> IO ()

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a logical device with graphics and present queues.
--
-- Enables the swapchain extension and portability subset (macOS).
-- Creates a resettable command pool on the graphics queue family.
--
-- Returns 'Nothing' if device creation fails.
createDevice :: Instance -> IO (Maybe Device)
createDevice inst =
  withInstancePtr inst $ \instPtr -> do
    ptr <- c_nv_device_create instPtr
    if ptr == nullPtr
      then pure Nothing
      else do
        fptr <- newForeignPtr c_nv_device_destroy ptr
        pure (Just (Device fptr))

-- | Destroy the command pool and logical device.
destroyDevice :: Device -> IO ()
destroyDevice (Device fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Operations
-- ----------------------------------------------------------------

-- | Block until all device operations have completed.
--
-- Call before cleanup or swapchain recreation.
deviceWaitIdle :: Device -> IO ()
deviceWaitIdle (Device fptr) =
  withForeignPtr fptr c_nv_device_wait_idle

-- ----------------------------------------------------------------
-- Internal
-- ----------------------------------------------------------------

-- | Run an action with the raw C pointer to the device.
withDevicePtr :: Device -> (Ptr () -> IO a) -> IO a
withDevicePtr (Device fptr) = withForeignPtr fptr
