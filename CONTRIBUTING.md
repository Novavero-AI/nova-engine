# Contributing to nova-engine

## Architecture

nova-engine is a 3D graphics engine. C99 hot path for Vulkan rendering and SDL2 windowing. Haskell brain for mesh generation, animation, terrain, and frame orchestration.

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

-- Render (planned — C99 hot path)
NovaEngine.Render.*               -- Vulkan + SDL2 rendering
NovaEngine.Render.FFI.*           -- foreign imports for C99 layer
```

### C99 Layer (`cbits/`)

All C symbols use the `nv_` prefix. FFI entry points live in `nv_ffi.c/h`.

**Naming:** `nv_<subsystem>_<function>` (e.g., `nv_buffer_create`, `nv_command_draw`).

**Style:**
- C99 strict (`-std=c99 -Wall -Wextra -Wpedantic -Werror`)
- No global mutable state — all state in structs passed by pointer
- Explicit sizes (`uint8_t`, `uint32_t`, etc.) — never `int` for sized data
- No heap allocation on hot path — stack buffers and caller-provided memory
- Bounds checking on every buffer operation

**FFI Pattern:**
- Hot path: `foreign import ccall unsafe` with flat scalar arguments
- Stateful structs: opaque `ForeignPtr ()` with `mallocForeignPtrBytes`
- Multiple outputs: `alloca` + `peek` pattern

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

**Haskell:** `base`, `bytestring`, `text`, `containers`, `array`

**System:** `libvulkan`, `libSDL2` (linked via `extra-libraries` when render subsystem is active)

No `linear`, `vector`, `JuicyPixels`, `vulkan` (Haskell package), or `sdl2` (Haskell package).
Matrix math is hand-rolled. Buffer uploads use `Foreign.Marshal.Array` from `base`.
Textures take raw `[Word8]` RGBA bytes.

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
