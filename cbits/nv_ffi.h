/*
 * nova-engine FFI — version query.
 *
 * C99 hot path for Vulkan rendering and SDL3 windowing.
 * FFI functions are declared in their respective module headers
 * (nv_window.h, nv_instance.h, nv_device.h, etc.).
 */

#ifndef NV_FFI_H
#define NV_FFI_H

#include <stdint.h>

/* Engine FFI version. */
int nv_ffi_version(void);

#endif /* NV_FFI_H */
