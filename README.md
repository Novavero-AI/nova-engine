<div align="center">
<h1>nova-engine</h1>
<p><strong>3D Graphics Engine</strong></p>
<p>C99 Vulkan + SDL2 hot path. Haskell brain.</p>
<p><a href="#architecture">Architecture</a> · <a href="#modules">Modules</a> · <a href="#quick-start">Quick Start</a> · <a href="#building">Building</a></p>
<p>

[![CI](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml/badge.svg)](https://github.com/Novavero-AI/nova-engine/actions/workflows/ci.yml)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![C99](https://img.shields.io/badge/C99-hot%20path-orange)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)](LICENSE)

</p>
</div>

---

A general-purpose 3D graphics engine. C99 handles the hot path (Vulkan rendering, SDL2 windowing, command recording, buffer management). Haskell handles the brain (mesh generation, skeletal animation, terrain, frame orchestration). Pure math, zero unnecessary deps.

---

## Architecture

```
┌─────────────────────────────────────────────┐
│           User Application                  │
├─────────────────────────────────────────────┤
│  Haskell Brain                              │
│  Math, Mesh, Animation, Terrain,            │
│  Frame orchestration, Resource lifecycle    │
├─────────────────────────────────────────────┤
│  FFI Boundary (unsafe ccall, flat args)     │
│  NovaEngine.Render.FFI.*                    │
├─────────────────────────────────────────────┤
│  C99 Hot Path                               │
│  nv_instance  nv_swapchain  nv_pipeline     │
│  nv_buffer  nv_texture  nv_command          │
│  nv_sync  nv_window  nv_ffi                 │
└─────────────────────────────────────────────┘
```

---

## Modules

### Math

| Module | Description |
|--------|-------------|
| `Math.Types` | `V2`, `V3`, `V4`, `Quaternion`, `M44`, `VecSpace` abstraction |
| `Math.Vector` | Dot, cross, normalize, lerp, distance, safe operations |
| `Math.Quaternion` | Axis-angle, Hamilton product, SLERP, rotation |
| `Math.Matrix` | Identity, perspective, ortho, lookAt, multiply, transpose |

### Mesh (planned)

| Module | Description |
|--------|-------------|
| `Mesh.Primitives` | Sphere, capsule, cylinder, cone, torus, box, plane |
| `Mesh.Curve` | Bezier, B-spline, NURBS with arc-length parameterization |
| `Mesh.Surface` | Bezier patches, B-spline and NURBS surfaces |
| `Mesh.Loft` | Revolve, loft, extrude, sweep with Bishop frames |
| `Mesh.SDF` | Signed distance fields, CSG, smooth blending, domain ops |
| `Mesh.Isosurface` | Marching cubes with vertex caching |
| `Mesh.DualContour` | Dual contouring with QEF for sharp features |
| `Mesh.Subdivision` | Catmull-Clark, Loop subdivision |
| `Mesh.Simplify` | Quadric error metric decimation |
| `Mesh.Boolean` | CSG union, intersection, difference |

### Animation (planned)

| Module | Description |
|--------|-------------|
| `Animation.Skeleton` | Joint trees, humanoid and quadruped builders |
| `Animation.Pose` | Forward kinematics, pose interpolation |
| `Animation.Animate` | Procedural oscillators, keyframes, easing, blending |
| `Animation.IK` | CCD and FABRIK solvers with joint constraints |
| `Animation.Skin` | Linear blend skinning, dual quaternion skinning |
| `Animation.Morph` | Mesh morphing, blend shapes |

### Terrain (planned)

| Module | Description |
|--------|-------------|
| `Terrain.Terrain` | Heightmap generation, erosion simulation |
| `Terrain.Noise` | Perlin, simplex, Worley, FBM, ridged multifractal |

### Render (planned — C99 hot path)

| C99 Module | Description |
|------------|-------------|
| `nv_instance` | Vulkan instance, physical device selection, logical device |
| `nv_swapchain` | Swapchain creation and recreation |
| `nv_pipeline` | Render pass, graphics pipeline, descriptor layouts |
| `nv_buffer` | Vertex, index, uniform, SSBO buffer management |
| `nv_texture` | Image creation, layout transitions, texture upload |
| `nv_command` | Command buffer recording, draw calls |
| `nv_sync` | Fences, semaphores, frame-in-flight synchronization |
| `nv_window` | SDL2 window creation, Vulkan surface |
| `nv_ffi` | FFI entry points (flat scalar arguments) |

---

## Quick Start

```haskell
import NovaEngine

main :: IO ()
main = do
  -- Math foundation is ready now
  let proj = perspective (pi / 4) (16 / 9) 0.1 100
      view = lookAt (V3 0 2 5) (V3 0 0 0) (V3 0 1 0)
      mvp  = mulM44 proj view
  print mvp
```

---

## Design

- **C99 hot path.** Vulkan and SDL2 calls run in C99 — zero FFI overhead in the render loop.
- **Haskell brain.** Mesh generation, animation, terrain — complex algorithms with type safety.
- **Minimal deps.** Haskell: `base`, `bytestring`, `text`, `containers`, `array`. System: `libvulkan`, `libSDL2`.
- **Pure math.** All geometry and animation functions are pure. No IO, no GPU, no mutable state.
- **Total.** No partial functions. Invalid inputs return `Nothing`, not crashes.

---

## Coordinate System

Right-handed, Y-up. Counter-clockwise front faces. All angles in radians.

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

```bash
cabal build all --ghc-options="-Werror"
cabal test
```

---

## Status

**Early development.** Math foundation complete. Mesh, animation, terrain, and render subsystems in progress.

---

<p align="center">
  <sub>BSD-3-Clause · <a href="https://github.com/Novavero-AI">Novavero AI</a></sub>
</p>
