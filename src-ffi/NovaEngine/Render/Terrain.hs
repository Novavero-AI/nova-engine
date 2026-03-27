-- | GPU terrain rendering pipeline.
--
-- Wraps the C99 @nv_terrain_*@ functions. Heightmap displacement
-- in the vertex shader, 4-layer splatmap blending in the fragment
-- shader.
module NovaEngine.Render.Terrain
  ( -- * Handle
    Terrain,

    -- * Lifecycle
    createTerrain,
    destroyTerrain,

    -- * Queries
    terrainSetLayout,
  )
where

import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
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
-- Handle
-- ----------------------------------------------------------------

-- | Opaque handle to a GPU terrain pipeline.
newtype Terrain = Terrain (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_terrain_create"
  c_nv_terrain_create ::
    Ptr () ->
    Ptr () ->
    CString ->
    CString ->
    Ptr () ->
    Word64 ->
    Ptr () ->
    IO (Ptr ())

foreign import ccall unsafe "&nv_terrain_destroy"
  c_nv_terrain_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_terrain_set_layout"
  c_nv_terrain_set_layout :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a terrain pipeline.
--
-- @renderPassPtr@ is the raw VkRenderPass handle.
-- @setLayouts@ are descriptor set layouts before the terrain set.
-- @shadowSetLayoutPtr@ is the shadow map descriptor set layout.
--
-- Returns 'Nothing' on failure.
createTerrain ::
  Device ->
  Ptr () ->
  FilePath ->
  FilePath ->
  [Word64] ->
  Ptr () ->
  IO (Maybe Terrain)
createTerrain dev renderPassPtr vertPath fragPath setLayouts shadowLayoutPtr =
  withDevicePtr dev $ \devPtr ->
    withCString vertPath $ \vp ->
      withCString fragPath $ \fp ->
        withArrayLen setLayouts $ \len dataPtr -> do
          ptr <-
            c_nv_terrain_create
              devPtr
              renderPassPtr
              vp
              fp
              (castPtr dataPtr)
              (fromIntegral len)
              shadowLayoutPtr
          if ptr == nullPtr
            then pure Nothing
            else do
              fptr <- newForeignPtr c_nv_terrain_destroy ptr
              pure (Just (Terrain fptr))

-- | Destroy the terrain pipeline.
destroyTerrain :: Terrain -> IO ()
destroyTerrain (Terrain fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the terrain descriptor set layout for allocating sets.
terrainSetLayout :: Terrain -> IO (Ptr ())
terrainSetLayout (Terrain fptr) =
  withForeignPtr fptr c_nv_terrain_set_layout
