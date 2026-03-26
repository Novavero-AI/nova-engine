# Changelog

## 0.2.0.0

- Unified gb-mesh into nova-engine with full namespace reorganization.
- **Math:** added `Storable` instances for `V2`, `V3`, `V4`, `M44`, `Quaternion`. Added `inverse` and `mkTransformM44` to `Math.Matrix`.
- **Mesh:** 20 modules — primitives, curves, surfaces, loft, icosphere, hull, subdivision, smooth, simplify, weld, remesh, booleans, symmetry, UV, LOD, deform, combine, buffer, export, import. Unified 64-byte `Vertex` with `vColor` field.
- **SDF:** signed distance fields with primitives, CSG, smooth blending, domain operations. Marching cubes and dual contouring isosurface extraction.
- **Noise:** improved Perlin (2D/3D), simplex (2D/3D/4D), Worley/cellular, FBM, ridged multifractal, turbulence, domain warping.
- **Terrain:** heightmap generation, thermal and hydraulic erosion, terracing, point scatter.
- **Animation:** skeleton (with humanoid/quadruped builders), pose and forward kinematics, procedural animation (oscillators, keyframes, easing), IK (CCD + FABRIK), skinning (LBS + dual quaternion), morph targets.
- **Spatial:** ray-triangle intersection (Moller-Trumbore), BVH-accelerated raycasting.
- **Tests:** 255 QuickCheck property tests across all modules.

## 0.1.0.0

- Initial release: math foundation (vectors, matrices, quaternions).
