-- | GPU-ready per-frame uniform buffer matching the PBR shader
-- @FrameUBO@ layout.
--
-- 448 bytes, std140:
--
-- @
-- mat4 view              offset   0
-- mat4 projection        offset  64
-- vec4 cameraPos         offset 128
-- vec4 lightDir          offset 144
-- vec4 lightColor        offset 160
-- mat4 cascadeMatrices[4] offset 176
-- vec4 cascadeSplits     offset 432
-- @
module NovaEngine.Scene.FrameUBO
  ( -- * Types
    FrameUBO (..),

    -- * Construction
    mkFrameUBO,
  )
where

import Foreign.Storable (Storable (..))
import NovaEngine.Math.Types (M44, V3 (..), V4 (..))
import NovaEngine.Scene.Camera
  ( Camera (..),
    projectionMatrix,
    viewMatrix,
  )
import NovaEngine.Scene.Shadow (CascadeData (..))

-- ----------------------------------------------------------------
-- FrameUBO
-- ----------------------------------------------------------------

-- | Per-frame uniform data for the PBR pipeline.
data FrameUBO = FrameUBO
  { uboView :: !M44,
    uboProjection :: !M44,
    uboCameraPos :: !V4,
    uboLightDir :: !V4,
    uboLightColor :: !V4,
    uboCascadeMatrix0 :: !M44,
    uboCascadeMatrix1 :: !M44,
    uboCascadeMatrix2 :: !M44,
    uboCascadeMatrix3 :: !M44,
    uboCascadeSplits :: !V4
  }
  deriving (Show, Eq)

instance Storable FrameUBO where
  sizeOf _ = 448
  alignment _ = 4
  peek p = do
    v <- peekByteOff p 0
    proj <- peekByteOff p 64
    camP <- peekByteOff p 128
    lDir <- peekByteOff p 144
    lCol <- peekByteOff p 160
    cm0 <- peekByteOff p 176
    cm1 <- peekByteOff p 240
    cm2 <- peekByteOff p 304
    cm3 <- peekByteOff p 368
    cs <- peekByteOff p 432
    pure (FrameUBO v proj camP lDir lCol cm0 cm1 cm2 cm3 cs)
  poke p (FrameUBO v proj camP lDir lCol cm0 cm1 cm2 cm3 cs) = do
    pokeByteOff p 0 v
    pokeByteOff p 64 proj
    pokeByteOff p 128 camP
    pokeByteOff p 144 lDir
    pokeByteOff p 160 lCol
    pokeByteOff p 176 cm0
    pokeByteOff p 240 cm1
    pokeByteOff p 304 cm2
    pokeByteOff p 368 cm3
    pokeByteOff p 432 cs

-- ----------------------------------------------------------------
-- Construction
-- ----------------------------------------------------------------

-- | Build a 'FrameUBO' from a camera, light parameters, and
-- cascade data.
--
-- @lightDir@ is the direction /toward/ the light (normalised by
-- the caller).  @lightColor@ carries RGB in xyz and intensity in
-- w, matching @frame.lightColor.rgb * frame.lightColor.a@ in the
-- fragment shader.
mkFrameUBO ::
  Camera ->
  -- | Light direction (toward light)
  V3 ->
  -- | Light color (RGB + intensity in w)
  V4 ->
  -- | Cascade data from 'computeCascades'
  CascadeData ->
  FrameUBO
mkFrameUBO cam (V3 dx dy dz) lightColor cascades =
  let V3 cx cy cz = cameraPosition cam
   in FrameUBO
        { uboView = viewMatrix cam,
          uboProjection = projectionMatrix cam,
          uboCameraPos = V4 cx cy cz 1.0,
          uboLightDir = V4 dx dy dz 0.0,
          uboLightColor = lightColor,
          uboCascadeMatrix0 = cascadeMatrix0 cascades,
          uboCascadeMatrix1 = cascadeMatrix1 cascades,
          uboCascadeMatrix2 = cascadeMatrix2 cascades,
          uboCascadeMatrix3 = cascadeMatrix3 cascades,
          uboCascadeSplits = cascadeSplits cascades
        }
