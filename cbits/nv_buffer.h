/*
 * nv_buffer — Vulkan vertex and index buffer upload.
 *
 * Creates device-local buffers via VMA-backed staged upload.
 *
 * Haskell sees NvBuffer* as an opaque Ptr ().
 */

#ifndef NV_BUFFER_H
#define NV_BUFFER_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvBuffer {
    VkBuffer        handle;
    NvVmaAllocation allocation;
    NvVmaAllocator  vma;
    VkDeviceSize    size;
} NvBuffer;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Upload vertex data to a device-local buffer via staging.
 * data points to packed floats, byte_size is total bytes.
 * Returns NULL on failure. */
NvBuffer *nv_buffer_create_vertex(NvDevice *dev, NvAllocator *alloc,
                                  const void *data,
                                  uint32_t byte_size);

/* Upload index data to a device-local buffer via staging.
 * data points to packed uint32_t indices, byte_size is total bytes.
 * Returns NULL on failure. */
NvBuffer *nv_buffer_create_index(NvDevice *dev, NvAllocator *alloc,
                                 const void *data,
                                 uint32_t byte_size);

/* Create a host-visible buffer (for UBOs updated per frame).
 * Returns NULL on failure. */
NvBuffer *nv_buffer_create_host(NvAllocator *alloc, uint32_t byte_size);

/* Create a host-visible storage buffer (SSBO, for bone matrices etc).
 * Returns NULL on failure. */
NvBuffer *nv_buffer_create_host_storage(NvAllocator *alloc,
                                         uint32_t byte_size);

/* Destroy the buffer and free its allocation. */
void nv_buffer_destroy(NvBuffer *buf);

/* Map a host-visible buffer.  Returns NULL on failure. */
void *nv_buffer_map(NvBuffer *buf);

/* Unmap a previously mapped buffer. */
void nv_buffer_unmap(NvBuffer *buf);

/* Get the raw VkBuffer handle (for descriptor writes). */
VkBuffer nv_buffer_vk_handle(const NvBuffer *buf);

#endif /* NV_BUFFER_H */
