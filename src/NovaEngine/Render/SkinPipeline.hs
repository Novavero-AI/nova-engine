-- | Vulkan pipeline for GPU-skinned meshes.
--
-- Wraps the C99 @nv_skin_pipeline_*@ functions. 80-byte vertex
-- layout with bone indices and weights. Bone matrices are read
-- from an SSBO.
module NovaEngine.Render.SkinPipeline
  ( -- * Handle
    SkinPipeline,

    -- * Lifecycle
    createSkinPipeline,
    destroySkinPipeline,

    -- * Queries
    skinBoneSetLayout,
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

-- | Opaque handle to a GPU-skinned pipeline.
newtype SkinPipeline = SkinPipeline (ForeignPtr ())

-- ----------------------------------------------------------------
-- FFI imports
-- ----------------------------------------------------------------

foreign import ccall unsafe "nv_skin_pipeline_create"
  c_nv_skin_pipeline_create ::
    Ptr () ->
    Ptr () ->
    CString ->
    CString ->
    Ptr () ->
    Word64 ->
    IO (Ptr ())

foreign import ccall unsafe "&nv_skin_pipeline_destroy"
  c_nv_skin_pipeline_destroy :: FunPtr (Ptr () -> IO ())

foreign import ccall unsafe "nv_skin_pipeline_bone_layout"
  c_nv_skin_pipeline_bone_layout :: Ptr () -> IO (Ptr ())

-- ----------------------------------------------------------------
-- Lifecycle
-- ----------------------------------------------------------------

-- | Create a skinned graphics pipeline.
--
-- @renderPassPtr@ is the raw VkRenderPass handle (e.g. from
-- 'postProcessHdrRenderPass'). @setLayouts@ are the descriptor
-- set layout handles for sets before the bone SSBO (e.g. FrameUBO,
-- textures). The bone SSBO is appended as the next set
-- automatically.
--
-- Returns 'Nothing' on failure.
createSkinPipeline ::
  Device ->
  Ptr () ->
  FilePath ->
  FilePath ->
  [Word64] ->
  IO (Maybe SkinPipeline)
createSkinPipeline dev renderPassPtr vertPath fragPath setLayouts =
  withDevicePtr dev $ \devPtr ->
    withCString vertPath $ \vp ->
      withCString fragPath $ \fp ->
        withArrayLen setLayouts $ \len dataPtr -> do
          ptr <-
            c_nv_skin_pipeline_create
              devPtr
              renderPassPtr
              vp
              fp
              (castPtr dataPtr)
              (fromIntegral len)
          if ptr == nullPtr
            then pure Nothing
            else do
              fptr <- newForeignPtr c_nv_skin_pipeline_destroy ptr
              pure (Just (SkinPipeline fptr))

-- | Destroy the skinned pipeline.
destroySkinPipeline :: SkinPipeline -> IO ()
destroySkinPipeline (SkinPipeline fptr) = finalizeForeignPtr fptr

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Get the bone SSBO descriptor set layout handle for allocating
-- descriptor sets.
skinBoneSetLayout :: SkinPipeline -> IO (Ptr ())
skinBoneSetLayout (SkinPipeline fptr) =
  withForeignPtr fptr c_nv_skin_pipeline_bone_layout
