/*
 * nova-engine FFI entry points.
 *
 * C99 hot path for Vulkan rendering and SDL2 windowing.
 * All symbols prefixed with nv_ to avoid collisions.
 *
 * Flat scalar arguments — no struct marshalling overhead.
 * Opaque handles returned as uint32_t IDs.
 */

#ifndef NV_FFI_H
#define NV_FFI_H

#include <stdint.h>

/* Placeholder — render subsystem FFI will be added here. */
int nv_ffi_version(void);

#endif /* NV_FFI_H */
