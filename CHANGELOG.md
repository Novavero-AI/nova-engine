# Changelog

## 0.4.0.0

- **Render: Textures.** stb_image vendored for PNG/JPG/BMP/TGA loading. VkImage creation with full mipmap chain generation, VkSampler with anisotropy, texture descriptor binding, and default 1x1 textures. New `nv_texture.c` and `Render.Texture` Haskell module.
- **Render: Scene system.** IntMap-based entity storage, single-pass transform hierarchy, perspective/orthographic camera, mesh registry, and multi-object draw loop.
- **Render: PBR materials.** Cook-Torrance BRDF (GGX/Smith/Fresnel-Schlick) with metallic-roughness workflow and 5 texture slots (albedo, metallic-roughness, normal, AO, emissive). Per-frame UBO for view, projection, camera position, and lights.
- **Render: Cascaded shadows.** 4-cascade directional light shadow maps with depth-only render pass and PCF soft shadow sampling in the PBR fragment shader.
- **Render: Post-processing.** HDR framebuffer (RGBA16F), Jimenez 13-tap bloom with tent upsample, ACES tonemapping, and FXAA 3.11.
- **Render: GPU animation.** Bone matrix SSBO uploaded per frame, vertex shader skinning with 80-byte skinned vertex format. `computeBoneMatrices` bridges the Haskell animation system to the GPU.
- **Render: Compute pipelines.** VkComputePipeline infrastructure with morph target dispatch and compute-to-vertex pipeline barriers.
- **Render: Terrain.** GPU terrain rendering with heightmap displacement and splatmap-based texture blending.
- **Input: SDL3 action mapping.** Configurable input action system built on SDL3 event polling.
- **Physics: GJK/EPA + solver.** Pure Haskell rigid body physics with GJK/EPA collision detection and sequential impulse constraint solver.
- **Debug: line rendering + GPU timestamps.** Debug line rendering overlay and GPU timestamp query support for profiling.
- **Code quality.** Extracted `nv_util.c` shared helpers, fixed VMA version handling, enabled required device features, fixed debug double-buffer teardown, HiDPI surface scaling, postprocess resource cleanup, and corrected several shader issues.
- **CI: shader validation, parallelized pipeline.** Added glslangValidator shader check job. C99 strict check now runs in parallel with lint/format. SDL3 and Vulkan SDK versions extracted to workflow-level env vars.

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
