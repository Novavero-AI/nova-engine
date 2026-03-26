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

/* Destroy the buffer and free its allocation. */
void nv_buffer_destroy(NvBuffer *buf);

#endif /* NV_BUFFER_H */
