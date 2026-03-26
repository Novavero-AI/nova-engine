-- | nova-engine: 3D graphics engine.
--
-- C99 Vulkan + SDL2 hot path, Haskell brain.
--
-- @
-- import NovaEngine            -- everything
-- import NovaEngine.Math       -- vectors, matrices, quaternions
-- import NovaEngine.Mesh       -- geometry generation
-- import NovaEngine.Animation  -- skeletal animation
-- import NovaEngine.Terrain    -- heightmaps, noise
-- import NovaEngine.Render     -- Vulkan + SDL2 rendering (future)
-- @
module NovaEngine
  ( -- * Math
    module NovaEngine.Math.Types,
    module NovaEngine.Math.Vector,
    module NovaEngine.Math.Quaternion,
    module NovaEngine.Math.Matrix,
  )
where

import NovaEngine.Math.Matrix
import NovaEngine.Math.Quaternion
import NovaEngine.Math.Types
import NovaEngine.Math.Vector
