# Changelog

## 0.3.0.0

- **Render: VMA integration.** All GPU memory allocation now goes through Vulkan Memory Allocator (vendored 3.1.0, MIT). Replaced raw `vkAllocateMemory` calls in buffer and swapchain modules with VMA sub-allocation. New `nv_allocator.c` C99 module, `nv_vma.cpp` C-linkage wrapper (only C++ file in the project), and `Render.Allocator` Haskell module.
- **Render: Descriptor set management.** New `nv_descriptor.c` C99 module and `Render.Descriptor` Haskell module. Descriptor set layouts, pools, allocation, and write helpers for buffers and images. Pipeline creation now accepts descriptor set layouts. Frame module supports descriptor set binding during command recording.
- **Render: API changes.** `createVertexBuffer`/`createIndexBuffer` now take `Allocator` instead of `Instance`. `createSwapchain` takes an additional `Allocator` parameter. `createPipeline` accepts a list of descriptor set layout handles.
- **CI:** Added standalone C99 strict compilation check (`-std=c99 -Wall -Wextra -Wpedantic -Werror`). Cross-platform build matrix (Linux, macOS, Windows) with Vulkan SDK and SDL3 installation.

## 0.2.0.0

- **Render: C99 Vulkan + SDL3 layer.** Window creation and event polling (`nv_window.c`), Vulkan instance with validation layers and GPU selection (`nv_instance.c`), logical device and queues (`nv_device.c`), swapchain with image views and depth buffer (`nv_swapchain.c`), render pass and graphics pipeline (`nv_pipeline.c`), staged vertex/index buffer upload (`nv_buffer.c`), 2-frame-in-flight synchronization and command recording (`nv_frame.c`). Haskell FFI wrappers for all modules.
- **Shaders:** Default vertex shader (MVP push constant) and fragment shader (directional N dot L lighting, SRGB output).
- **Math:** Added `Storable` instances for `V2`, `V3`, `V4`, `M44`, `Quaternion`. Added `inverse` and `mkTransformM44` to `Math.Matrix`.
- **Mesh:** 20 modules — primitives, curves, surfaces, loft, icosphere, hull, subdivision, smooth, simplify, weld, remesh, booleans, symmetry, UV, LOD, deform, combine, buffer, export, import. Unified 64-byte `Vertex` with `vColor` field.
- **SDF:** Signed distance fields with primitives, CSG, smooth blending, domain operations. Marching cubes and dual contouring isosurface extraction.
- **Noise:** Improved Perlin (2D/3D), simplex (2D/3D/4D), Worley/cellular, FBM, ridged multifractal, turbulence, domain warping.
- **Terrain:** Heightmap generation, thermal and hydraulic erosion, terracing, point scatter.
- **Animation:** Skeleton (with humanoid/quadruped builders), pose and forward kinematics, procedural animation (oscillators, keyframes, easing), IK (CCD + FABRIK), skinning (LBS + dual quaternion), morph targets.
- **Spatial:** Ray-triangle intersection (Moller-Trumbore), BVH-accelerated raycasting.
- **Tests:** 255 QuickCheck property tests across all modules.

## 0.1.0.0

- Initial release: math foundation (vectors, matrices, quaternions).
