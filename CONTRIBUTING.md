# Contributing to nova-engine

## Architecture

nova-engine is a 3D graphics engine using the C99 hot path + Haskell brain pattern (same as nova-net).

### Module Structure

```
NovaEngine                    -- top-level re-export
NovaEngine.Math.Types         -- V2, V3, V4, Quaternion, M44, VecSpace
NovaEngine.Math.Vector        -- dot, cross, normalize, lerp, distance
NovaEngine.Math.Quaternion    -- axisAngle, mulQuat, slerp, rotateV3
NovaEngine.Math.Matrix        -- identity, perspective, ortho, lookAt, mulM44
NovaEngine.Mesh.*             -- geometry generation + processing (from gb-mesh)
NovaEngine.Animation.*        -- skeleton, pose, skin, IK, animate, morph
NovaEngine.Terrain.*          -- heightmaps, noise, erosion
NovaEngine.Render.*           -- Vulkan + SDL2 rendering (C99 hot path)
NovaEngine.Render.FFI.*       -- foreign imports for C99 layer
NovaEngine.UI.*               -- UI system (future)
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

**FFI Pattern (from nova-net):**
- Hot path: `foreign import ccall unsafe` with flat scalar arguments
- Stateful structs: opaque `ForeignPtr ()` with `mallocForeignPtrBytes`
- Multiple outputs: `alloca` + `peek` pattern

### Haskell Layer (`src/`)

**Style:**
- GHC2021, GHC 9.8.4
- ormolu formatted, hlint clean
- Strict fields (`!`) on data types
- No partial functions (`head`, `!!`, `fromJust`)
- Total pattern matches

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
