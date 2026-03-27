# Contributing to nova-engine

## Architecture

nova-engine is a 3D graphics engine. C99 hot path for Vulkan rendering, SDL3 windowing, and VMA memory management. Haskell brain for mesh generation, animation, terrain, and frame orchestration.

### Module Structure

```
NovaEngine                        -- top-level re-export

-- Math (hand-rolled, Storable, no external deps)
NovaEngine.Math.Types             -- V2, V3, V4, Quaternion, M44, VecSpace, Storable
NovaEngine.Math.Vector            -- dot, cross, normalize, lerp, distance
NovaEngine.Math.Quaternion        -- axisAngle, mulQuat, slerp, rotateV3
NovaEngine.Math.Matrix            -- identity, perspective, ortho, lookAt, mulM44, inverse

-- Mesh (geometry generation + processing)
NovaEngine.Mesh.Types             -- Vertex (64B), Mesh (Monoid), validation, helpers
NovaEngine.Mesh.Primitives        -- sphere, capsule, cylinder, cone, torus, box, plane
NovaEngine.Mesh.Curve             -- Bezier, B-spline, NURBS, arc-length
NovaEngine.Mesh.Surface           -- Bezier patches, B-spline/NURBS surfaces
NovaEngine.Mesh.Loft              -- revolve, loft, extrude, sweep
NovaEngine.Mesh.Icosphere         -- geodesic sphere
NovaEngine.Mesh.Hull              -- convex hull
NovaEngine.Mesh.Subdivision       -- Catmull-Clark, Loop
NovaEngine.Mesh.Smooth            -- Laplacian, Taubin
NovaEngine.Mesh.Simplify          -- quadric error metric decimation
NovaEngine.Mesh.Weld              -- vertex welding
NovaEngine.Mesh.Remesh            -- isotropic remeshing
NovaEngine.Mesh.Boolean           -- CSG operations
NovaEngine.Mesh.Symmetry          -- mirror, radial
NovaEngine.Mesh.UV                -- UV projection
NovaEngine.Mesh.LOD               -- level-of-detail chains
NovaEngine.Mesh.Deform            -- twist, bend, taper, FFD, displacement
NovaEngine.Mesh.Combine           -- translate, rotate, scale, merge, recompute normals/tangents
NovaEngine.Mesh.Buffer            -- GPU-ready buffer packing
NovaEngine.Mesh.Export            -- OBJ, glTF 2.0 export
NovaEngine.Mesh.Import            -- OBJ, glTF 2.0 import

-- SDF (implicit geometry)
NovaEngine.SDF                    -- primitives, CSG, smooth blend, domain ops
NovaEngine.SDF.Isosurface         -- marching cubes
NovaEngine.SDF.DualContour        -- dual contouring with QEF

-- Noise (procedural generation)
NovaEngine.Noise                  -- Perlin, simplex, Worley, FBM, ridged, turbulence

-- Terrain (heightmap generation)
NovaEngine.Terrain                -- heightmap, erosion, terracing
NovaEngine.Terrain.Scatter        -- point distribution

-- Animation (pure CPU-side)
NovaEngine.Animation.Skeleton     -- joint trees, humanoid/quadruped builders
NovaEngine.Animation.Pose         -- forward kinematics, pose interpolation
NovaEngine.Animation.Animate      -- oscillators, keyframes, easing, blending
NovaEngine.Animation.IK           -- CCD, FABRIK solvers
NovaEngine.Animation.Skin         -- linear blend + dual quaternion skinning
NovaEngine.Animation.Morph        -- blend shapes

-- Spatial (queries)
NovaEngine.Spatial.Raycast        -- ray-triangle, BVH

-- Render (C99 hot path via FFI)
NovaEngine.Render.Window          -- SDL3 window, Vulkan surface, events
NovaEngine.Render.Instance        -- VkInstance, validation, GPU selection
NovaEngine.Render.Device          -- logical device, queues, command pool
NovaEngine.Render.Allocator       -- VMA-backed GPU memory allocator
NovaEngine.Render.Swapchain       -- swapchain, image views, depth buffer
NovaEngine.Render.Pipeline        -- render pass, graphics pipeline, framebuffers
NovaEngine.Render.Buffer          -- VMA-backed vertex/index buffer upload
NovaEngine.Render.Frame           -- frame-in-flight sync, command recording
NovaEngine.Render.Descriptor      -- descriptor set layouts, pools, writes
NovaEngine.Render.Texture         -- texture creation, mipmaps, sampling
NovaEngine.Render.Material        -- PBR material params + Storable packing
NovaEngine.Render.Shadow          -- cascaded shadow map resources
NovaEngine.Render.PostProcess     -- HDR framebuffer, bloom, tonemap, FXAA
NovaEngine.Render.Skin            -- skinned vertex type, bone matrix upload
NovaEngine.Render.SkinPipeline    -- skinned mesh graphics pipeline
NovaEngine.Render.Compute         -- Vulkan compute pipeline
NovaEngine.Render.Terrain         -- terrain graphics pipeline

-- Input (C99 via FFI)
NovaEngine.Input                  -- SDL3 input polling, raw state queries
NovaEngine.Input.ActionMap        -- pure Haskell action mapping layer

-- Debug (C99 via FFI)
NovaEngine.Debug                  -- debug line rendering, GPU timestamp profiling

-- Physics (pure Haskell)
NovaEngine.Physics.GJK            -- GJK collision detection
NovaEngine.Physics.EPA            -- EPA penetration depth
NovaEngine.Physics.Shapes         -- collision shape support functions
NovaEngine.Physics.Solver         -- sequential impulse constraint solver

-- Scene (pure Haskell + UBO upload)
NovaEngine.Scene                  -- entity management, transform hierarchy
NovaEngine.Scene.Camera           -- camera types, view/projection matrices
NovaEngine.Scene.FrameUBO         -- per-frame uniform buffer (std140)
NovaEngine.Scene.Shadow           -- cascade split computation, light-space fitting
```

### C99 Layer (`cbits/`)

All C symbols use the `nv_` prefix. Each module has its own header.

**Naming:** `nv_<subsystem>_<function>` (e.g., `nv_buffer_create_vertex`, `nv_frame_begin`).

**Style:**
- C99 strict (`-std=c99 -Wall -Wextra -Wpedantic -Werror`)
- No global mutable state — all state in structs passed by pointer
- Explicit sizes (`uint8_t`, `uint32_t`, etc.) — never `int` for sized data
- No heap allocation on hot path — stack buffers and caller-provided memory
- Bounds checking on every buffer operation
- Return NULL on allocation failure

**FFI Pattern:**
- `foreign import ccall unsafe` with flat scalar arguments
- Opaque `ForeignPtr ()` with C finalizers registered via `newForeignPtr`
- `withXxxPtr :: Xxx -> (Ptr () -> IO a) -> IO a` for safe pointer access
- Descriptor sets passed as `Word64` (Vulkan non-dispatchable handles)

**C++ Exception:** `nv_vma.cpp` is the only C++ file. It compiles VMA and provides C-linkage wrappers. All other engine code is pure C99.

### Haskell Layer (`src/`)

**Style:**
- GHC2021, GHC 9.8.4
- ormolu formatted, hlint clean
- Strict fields (`!`) on data types via `StrictData`
- No partial functions (`head`, `!!`, `fromJust`)
- Total pattern matches
- Pure functions for all geometry/animation/noise — no IO

### Vertex Format

The unified `Vertex` is 64 bytes (16 floats):

```
Position  V3   12 bytes   offset  0
Normal    V3   12 bytes   offset 12
UV        V2    8 bytes   offset 24
Tangent   V4   16 bytes   offset 32   (w = bitangent handedness)
Color     V4   16 bytes   offset 48   (RGBA, default opaque white)
```

Use the `vertex` smart constructor for default white color. Use the `Vertex` constructor directly when you need a specific color.

### Dependencies

**Haskell:** `base`, `containers`, `array`

**System:** `libvulkan`, `libSDL3` (via pkg-config)

**Vendored:** `vk_mem_alloc.h` (VMA 3.1.0, MIT) — compiled as C++ via `nv_vma.cpp`

No `linear`, `vector`, `JuicyPixels`, `vulkan` (Haskell package), or `sdl2`/`sdl3` (Haskell package).
Matrix math is hand-rolled. Buffer uploads use `Foreign.Marshal.Array` from `base`.
Textures take raw `[Word8]` RGBA bytes.

### Memory Management

All GPU memory goes through VMA via `nv_allocator.c`. No direct `vkAllocateMemory` calls.

- **Device-local buffers:** `nv_alloc_buffer_staged` (host-visible staging → device-local transfer)
- **Host-visible buffers:** `nv_alloc_buffer_host` (for UBOs updated per frame)
- **Images:** `nv_alloc_image_create` (device-local, used for depth buffer and textures)

## Build

```bash
cabal build all --ghc-options="-Werror"
cabal test --test-show-details=streaming
```

## Format & Lint

```bash
ormolu --mode inplace src/**/*.hs test/**/*.hs
hlint src/ test/
```
