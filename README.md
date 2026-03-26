<div align="center">
<h1>nova-engine</h1>
<p><strong>3D Graphics Engine</strong></p>
<p>C99 Vulkan + SDL3 hot path. VMA memory management. Haskell brain.</p>
<p><a href="#architecture">Architecture</a> · <a href="#modules">Modules</a> · <a href="#quick-start">Quick Start</a> · <a href="#building">Building</a> · <a href="#roadmap">Roadmap</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![C99](https://img.shields.io/badge/C99-hot%20path-orange)
![Vulkan](https://img.shields.io/badge/vulkan-1.0-red)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)](LICENSE)

</p>
</div>

---

A general-purpose 3D graphics engine. C99 handles the hot path (Vulkan rendering, SDL3 windowing, VMA memory management, command recording). Haskell handles the brain (mesh generation, skeletal animation, terrain, frame orchestration). Pure math, zero unnecessary deps.

**48 modules. 255 property tests. 18,000+ lines of Haskell + C99.**

---

## Architecture

```
┌─────────────────────────────────────────────┐
│           User Application                  │
├─────────────────────────────────────────────┤
│  Haskell Brain                              │
│  Math, Mesh, SDF, Noise, Terrain,           │
│  Animation, Spatial, Frame orchestration    │
├─────────────────────────────────────────────┤
│  FFI Boundary (unsafe ccall, flat args)     │
│  NovaEngine.Render.*                        │
├─────────────────────────────────────────────┤
│  C99 Hot Path (VMA-backed)                  │
│  nv_window     nv_instance   nv_device      │
│  nv_allocator  nv_swapchain  nv_pipeline    │
│  nv_buffer     nv_frame      nv_descriptor  │
├─────────────────────────────────────────────┤
│  Vendored: VMA 3.1.0 (nv_vma.cpp)          │
└─────────────────────────────────────────────┘
```

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
| `nv_vma` | — | C-linkage wrapper around VMA (only C++ in the project) |

**Shaders:** `default.vert` (MVP push constant), `default.frag` (directional N dot L lighting, SRGB output)

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
- **Minimal deps.** Haskell: `base`, `bytestring`, `text`, `containers`, `array`. System: `libvulkan`, `libSDL3`. Vendored: VMA 3.1.0.
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

---

## Roadmap

### Done

- Math foundation (vectors, matrices, quaternions, Storable instances)
- Mesh generation (20 modules: primitives, curves, surfaces, subdivision, simplification, booleans, import/export)
- SDF (signed distance fields, marching cubes, dual contouring)
- Noise (Perlin, simplex, Worley, FBM, ridged, turbulence, domain warp)
- Terrain (heightmap generation, thermal/hydraulic erosion, scatter)
- Animation (skeleton, pose, FK, IK, skinning, morph targets)
- Spatial (ray-mesh intersection, BVH)
- 255 property tests
- C99 Vulkan + SDL3 render layer (window, instance, device, swapchain, pipeline, buffer, frame)
- VMA integration (all GPU memory sub-allocated, zero raw vkAllocateMemory)
- Descriptor set management (layouts, pools, allocation, buffer/image writes)
- CI: HLint, Ormolu, C99 strict check, cross-platform build (Linux, macOS, Windows)

### Phase 2: Textures

- [ ] Vendor stb_image.h for image loading (PNG, JPG, BMP, TGA)
- [ ] VkImage creation with mipmap generation (vkCmdBlitImage chain)
- [ ] VkSampler (linear, repeat, anisotropy)
- [ ] Texture descriptor binding via Phase 1 descriptor infrastructure
- [ ] Default textures (1x1 white, 1x1 flat normal)
- [ ] Haskell FFI: `Render.Texture`, `Render.ImageLoad`

### Phase 3: Scene System

- [ ] Entity storage (IntMap-based, pure Haskell)
- [ ] Transform hierarchy with single-pass world transform computation
- [ ] Camera (perspective, orthographic)
- [ ] Mesh registry (mesh handle to GPU buffer mapping)
- [ ] Multi-object draw loop

### Phase 4: PBR Materials

- [ ] Cook-Torrance BRDF (GGX distribution, Smith geometry, Fresnel-Schlick)
- [ ] Metallic-roughness workflow with 5 texture slots (albedo, metallic-roughness, normal, AO, emissive)
- [ ] Per-frame UBO (view, projection, camera position, lights)
- [ ] PBR vertex + fragment shaders

### Phase 5: Shadows

- [ ] Cascaded shadow maps (4 cascades, directional light)
- [ ] Depth-only render pass and shadow vertex shader
- [ ] PCF soft shadows in PBR fragment shader

### Phase 6: Post-Processing

- [ ] HDR framebuffer (RGBA16F)
- [ ] Bloom (Jimenez 13-tap downsample + tent upsample)
- [ ] ACES tonemapping
- [ ] FXAA 3.11

### Phase 7: GPU Animation

- [ ] Bone matrix SSBO with vertex shader skinning
- [ ] 80-byte skinned vertex format (base 64B + bone IDs + weights)
- [ ] `computeBoneMatrices` bridge from Haskell animation system

### Phase 8: Compute

- [ ] VkComputePipeline infrastructure
- [ ] Morph target compute shader
- [ ] Compute-to-vertex pipeline barrier

### Phase 9-12: Terrain Rendering, Input/Audio, Physics, Debug

- [ ] GPU terrain (clipmap/CDLOD, heightmap displacement, splatmap)
- [ ] SDL3 input action mapping, spatial audio
- [ ] Pure Haskell physics (GJK/EPA, rigid body, sequential impulse solver)
- [ ] Dear ImGui integration, GPU profiling, debug line rendering

---

<p align="center">
  <sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
