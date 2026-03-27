-- | nova-engine: 3D graphics engine.
--
-- C99 Vulkan + SDL3 hot path, Haskell brain.
--
-- @
-- import NovaEngine              -- math re-exports
-- import NovaEngine.Mesh.*       -- procedural geometry
-- import NovaEngine.Animation.*  -- skeleton, pose, IK, skinning
-- import NovaEngine.SDF.*        -- signed distance fields
-- import NovaEngine.Noise        -- procedural noise
-- import NovaEngine.Terrain.*    -- heightmaps, erosion, scatter
-- import NovaEngine.Spatial.*    -- raycast, BVH
-- import NovaEngine.Scene.*      -- transform graph, camera, shadows
-- import NovaEngine.Render.*     -- Vulkan rendering (FFI to C99)
-- import NovaEngine.Input.*      -- SDL3 input, action maps
-- import NovaEngine.Physics.*    -- GJK, EPA, rigid body solver
-- import NovaEngine.Debug        -- line rendering, GPU profiling
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
