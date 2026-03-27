<div align="center">
<h1>nova-engine</h1>
<p><strong>3D Graphics Engine</strong></p>
<p>C99 Vulkan + SDL3 hot path. VMA memory management. Haskell brain.</p>
<p><a href="#architecture">Architecture</a> · <a href="#modules">Modules</a> · <a href="#quick-start">Quick Start</a> · <a href="#building">Building</a> · <a href="#roadmap">Roadmap</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![C99](https://img.shields.io/badge/C99-hot%20path-orange)
![Vulkan](https://img.shields.io/badge/vulkan-1.2-red)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)](LICENSE)

</p>
</div>

---

A general-purpose 3D graphics engine. C99 handles the hot path (Vulkan rendering, SDL3 windowing, VMA memory management, command recording). Haskell handles the brain (mesh generation, skeletal animation, terrain, frame orchestration). Pure math, zero unnecessary deps.

**68 modules. 305 property tests. Cross-platform CI (Linux, macOS, Windows).**

---

## Architecture

```
┌─────────────────────────────────────────────┐
│           User Application                  │
├─────────────────────────────────────────────┤
│  nova-pure (49 modules, src/)               │
│  Math, Mesh, SDF, Noise, Terrain,           │
│  Animation, Spatial, Physics, Scene,        │
│  Render.Types, Input.ActionMap              │
├─────────────────────────────────────────────┤
│  FFI Layer (19 modules, src-ffi/)           │
│  Render.Window .. Render.Terrain            │
│  Input, Debug                               │
├─────────────────────────────────────────────┤
│  C99 Hot Path (cbits/)                      │
│  nv_window     nv_instance   nv_device      │
│  nv_allocator  nv_swapchain  nv_pipeline    │
│  nv_buffer     nv_frame      nv_descriptor  │
│  nv_texture    nv_shadow     nv_postprocess │
│  nv_skin_pipeline  nv_compute  nv_terrain   │
│  nv_input      nv_debug      nv_util        │
├─────────────────────────────────────────────┤
│  Vendored: VMA 3.1.0 (nv_vma.cpp)          │
└─────────────────────────────────────────────┘
```

The codebase is split into two cabal libraries sharing one package:

- **`nova-pure`** — pure Haskell with zero native dependencies. All math, geometry, animation, physics, and scene logic. Tested on all platforms without SDL3/Vulkan installed.
- **`nova-engine`** (public) — re-exports `nova-pure` and adds the C99 FFI layer. Users depend on this.

---

## Modules

### Math

| Module | Description |
|--------|-------------|
| `Math.Types` | `V2`, `V3`, `V4`, `Quaternion`, `M44`, `VecSpace`, `Storable` instances |
| `Math.Vector` | Dot, cross, normalize, lerp, distance, safe operations |
| `Math.Quaternion` | Axis-angle, Hamilton product, SLERP, rotation |
| `Math.Matrix` | Identity, perspective, ortho, lookAt, multiply, transpose, inverse |

### Mesh

| Module | Description |
|--------|-------------|
| `Mesh.Types` | Unified 64-byte `Vertex` (pos, normal, UV, tangent, color), `Mesh` with `Monoid`, validation |
| `Mesh.Primitives` | Sphere, capsule, cylinder, cone, torus, box, plane, tapered cylinder |
| `Mesh.Curve` | Bezier, B-spline, NURBS with arc-length parameterization |
| `Mesh.Surface` | Bezier patches, B-spline and NURBS surface tessellation |
| `Mesh.Loft` | Revolve, loft, extrude, sweep with rotation-minimizing Bishop frames |
| `Mesh.Icosphere` | Geodesic sphere from icosahedron subdivision |
| `Mesh.Hull` | Incremental 3D convex hull |
| `Mesh.Subdivision` | Catmull-Clark (quads, any topology), Loop (triangles) |
| `Mesh.Smooth` | Laplacian smoothing, Taubin volume-preserving smoothing |
| `Mesh.Simplify` | Quadric error metric decimation (Garland-Heckbert) |
| `Mesh.Weld` | Spatial-hash vertex welding, degenerate triangle removal |
| `Mesh.Remesh` | Isotropic remeshing (split, collapse, flip, relax) |
| `Mesh.Boolean` | CSG union, intersection, difference |
| `Mesh.Symmetry` | Mirror (X/Y/Z/arbitrary plane), radial symmetry |
| `Mesh.UV` | Planar, cylindrical, spherical, box UV projection |
| `Mesh.LOD` | Level-of-detail chain generation with screen-size selection |
| `Mesh.Deform` | Twist, bend, taper, free-form deformation lattices, displacement |
| `Mesh.Combine` | Translate, rotate, scale, merge, recompute normals and tangents |
| `Mesh.Buffer` | GPU-ready interleaved and separate attribute packing |
| `Mesh.Export` | Wavefront OBJ and glTF 2.0 export (pure, no deps) |
| `Mesh.Import` | OBJ and glTF 2.0 import with built-in JSON and base64 parsers |

### SDF

| Module | Description |
|--------|-------------|
| `SDF` | Primitives, CSG, smooth blending, domain ops (translate, rotate, scale, repeat, twist, bend, taper) |
| `SDF.Isosurface` | Marching cubes with grid caching and edge-vertex deduplication |
| `SDF.DualContour` | Dual contouring with QEF solving for sharp features |

### Noise

| Module | Description |
|--------|-------------|
| `Noise` | Improved Perlin (2D/3D), simplex (2D/3D/4D), Worley/cellular, FBM, ridged multifractal, turbulence, domain warping |

### Terrain

| Module | Description |
|--------|-------------|
| `Terrain` | Heightmap generation, thermal and hydraulic erosion, terracing, plateau, clamping |
| `Terrain.Scatter` | Uniform, Poisson disk, and weighted point distribution |

### Animation

| Module | Description |
|--------|-------------|
| `Animation.Skeleton` | Joint trees, validation, humanoid and quadruped builders |
| `Animation.Pose` | Forward kinematics (O(n) tree traversal), pose interpolation and composition |
| `Animation.Animate` | Procedural oscillators, keyframes, easing, blending, sequencing, looping |
| `Animation.IK` | CCD and FABRIK solvers with hinge and cone constraints |
| `Animation.Skin` | Linear blend skinning, dual quaternion skinning, auto-weight generation |
| `Animation.Morph` | Full mesh morphing, multi-target blend shapes |

### Spatial

| Module | Description |
|--------|-------------|
| `Spatial.Raycast` | Ray-triangle intersection (Moller-Trumbore), BVH-accelerated ray-mesh queries |

### Scene

| Module | Description |
|--------|-------------|
| `Scene` | IntMap-based entity storage, transform hierarchy, single-pass world matrix computation |
| `Scene.Camera` | Perspective/orthographic camera, view and projection matrix computation |
| `Scene.FrameUBO` | 448-byte GPU-ready per-frame uniform buffer (view, proj, lights, cascades) |
| `Scene.Shadow` | Cascade split computation, frustum extraction, light-space ortho fitting with texel snapping |

### Physics

| Module | Description |
|--------|-------------|
| `Physics.Shapes` | Sphere, box, capsule collision shapes |
| `Physics.GJK` | GJK collision detection with Minkowski difference support mapping |
| `Physics.EPA` | Expanding polytope algorithm for penetration depth and contact normal |
| `Physics.Solver` | Sequential impulse constraint solver, rigid body integration |

### Input

| Module | Description |
|--------|-------------|
| `Input` | SDL3 keyboard/mouse state polling, scancode queries |
| `Input.ActionMap` | Named action bindings (key → action), press/release/hold detection |

### Debug

| Module | Description |
|--------|-------------|
| `Debug` | Immediate-mode debug line rendering, box/sphere wireframes, GPU timestamp profiling |

### Render Types (pure)

| Module | Description |
|--------|-------------|
| `Render.Types` | `Texture` handle, `Material`, `MaterialParams` Storable, `SkinnedVertex` Storable, `calcMipLevels` |

### Render (C99 Vulkan + SDL3)

| C99 Module | Haskell Wrapper | Description |
|------------|-----------------|-------------|
| `nv_window` | `Render.Window` | SDL3 window creation, Vulkan surface, HiDPI, event polling |
| `nv_instance` | `Render.Instance` | VkInstance, validation layers, debug messenger, GPU selection and scoring |
| `nv_device` | `Render.Device` | Logical device, graphics/present queues, command pool |
| `nv_allocator` | `Render.Allocator` | VMA-backed GPU memory allocator, staged upload, host-visible buffers |
| `nv_swapchain` | `Render.Swapchain` | Swapchain creation/recreation, image views, VMA-backed depth buffer |
| `nv_pipeline` | `Render.Pipeline` | Render pass, graphics pipeline, framebuffers, descriptor set layout support |
| `nv_buffer` | `Render.Buffer` | VMA-backed vertex and index buffer upload via staging |
| `nv_frame` | `Render.Frame` | 2 frames in flight, semaphores/fences, command recording, descriptor binding |
| `nv_descriptor` | `Render.Descriptor` | Descriptor set layouts, pools, allocation, buffer/image write helpers |
| `nv_texture` | `Render.Texture` | stb_image loading, VkImage with mipmaps, samplers, default textures |
| — | `Render.Material` | PBR material (5 texture slots), `MaterialParams` Storable, default textures |
| `nv_shadow` | `Render.Shadow` | 4-cascade CSM, depth-only pass, PCF soft shadows |
| `nv_postprocess` | `Render.PostProcess` | HDR framebuffer, bloom, ACES tonemap, FXAA |
| `nv_skin_pipeline` | `Render.SkinPipeline` | GPU skinning pipeline, bone SSBO descriptor layout |
| — | `Render.Skin` | 80-byte `SkinnedVertex`, bone matrix computation, LBS/DQS bridge |
| `nv_compute` | `Render.Compute` | Compute pipeline, morph target dispatch, pipeline barriers |
| `nv_terrain` | `Render.Terrain` | Heightmap displacement, splatmap, terrain render pass |
| `nv_input` | `Input` | SDL3 keyboard/mouse polling, action mapping |
| — | `Input.ActionMap` | Configurable input action bindings |
| `nv_debug` | `Debug` | Debug line rendering, GPU timestamp queries |
| `nv_util` | — | Shared SPIR-V loader, one-shot command buffers |
| `nv_vma` | — | C-linkage wrapper around VMA (only C++ in the project) |

**Shaders:** PBR (vertex + fragment), shadow (depth-only), bloom (downsample + upsample), tonemap, terrain (displacement + splatmap), debug (line rendering), compute (morph targets)

---

## Quick Start

```haskell
import NovaEngine
import NovaEngine.Mesh.Primitives
import NovaEngine.Mesh.Combine
import NovaEngine.Mesh.Export
import NovaEngine.Noise
import NovaEngine.SDF
import NovaEngine.SDF.Isosurface

main :: IO ()
main = do
  -- Generate a sphere
  let Just mesh = sphere 1.0 32 16

  -- Combine meshes
  let scene = merge
        [ mesh
        , translate (V3 3 0 0) mesh
        , translate (V3 (-3) 0 0) mesh
        ]

  -- Export to OBJ
  writeFile "scene.obj" (meshToOBJ scene)

  -- SDF: blend two spheres and extract surface
  let sdf = smoothUnion 0.5
              (sdfSphere 1.0)
              (sdfTranslate (V3 1.5 0 0) (sdfSphere 1.0))
      Just blob = isosurface sdf (-3) 3 (-3) 3 (-3) 3 64 64 64

  -- Noise-driven terrain
  let cfg = mkNoiseConfig 42
      height x z = fbm (perlin2D cfg) 6 2.0 0.5 (x * 0.1) (z * 0.1)

  -- View/projection math
  let proj = perspective (pi / 4) (16 / 9) 0.1 100
      view = lookAt (V3 0 2 5) (V3 0 0 0) (V3 0 1 0)
      mvp  = mulM44 proj view
  print mvp
```

---

## Design

- **C99 hot path.** Vulkan and SDL3 calls run in C99 — zero FFI overhead in the render loop.
- **VMA memory.** All GPU allocations go through Vulkan Memory Allocator — no raw `vkAllocateMemory`, no 4096 allocation limit.
- **Haskell brain.** Mesh generation, animation, terrain, SDF, noise — complex algorithms with type safety.
- **Minimal deps.** Haskell: `base`, `containers`, `array`. System: `libvulkan`, `libSDL3`. Vendored: VMA 3.1.0.
- **Pure math.** All geometry, animation, and noise functions are pure. No IO, no GPU, no mutable state.
- **Total.** No partial functions. Invalid inputs return `Nothing`, not crashes.
- **Hand-rolled.** No `linear`, `vector`, `JuicyPixels`, or Haskell Vulkan/SDL bindings. The math is ours. The C is ours.

---

## Coordinate System

Right-handed, Y-up. Counter-clockwise front faces. All angles in radians. UV: U wraps circumferentially, V runs along height.

```
    Y (up)
    |
    |
    +------ X (right)
   /
  Z (toward camera)
```

---

## Building

**System dependencies:** `libvulkan`, `libSDL3`, `pkg-config`, a C++ compiler (for VMA).

```bash
cabal build all --ghc-options="-Werror"
cabal test --test-show-details=streaming
```

Tests depend only on `nova-pure` (no native libs needed). The full library requires Vulkan and SDL3.

---

## Roadmap

### Done

- [x] Math + Mesh + SDF + Noise + Terrain + Animation + Spatial + Tests
- [x] C99 Vulkan + SDL3 render layer
- [x] VMA integration
- [x] Descriptor set management
- [x] Textures (stb_image, mipmaps, samplers)
- [x] Scene system (IntMap entities, transform hierarchy, camera)
- [x] PBR materials (Cook-Torrance, 5 texture slots)
- [x] Shadows (4-cascade CSM, PCF)
- [x] Post-processing (HDR, bloom, ACES tonemap, FXAA)
- [x] GPU animation (bone SSBO, skinned vertices)
- [x] Compute (morph targets)
- [x] Terrain rendering (heightmap displacement, splatmap)
- [x] Input (SDL3 action mapping)
- [x] Physics (GJK/EPA, sequential impulse solver)
- [x] Debug (line rendering, GPU timestamps)
- [x] CI (HLint, Ormolu, C99 strict, shader validation, cross-platform)
- [x] Internal library split (`nova-pure` + FFI, tests run on all platforms)

---

<p align="center">
  <sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
