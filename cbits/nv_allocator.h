/*
 * nv_allocator — GPU memory allocator backed by VMA.
 *
 * Wraps the thin nv_vma C-linkage layer with higher-level types
 * (NvAllocator, NvAllocBuffer, NvAllocImage) and staged upload.
 *
 * All functions in this module are C99.  VMA internals are hidden
 * behind opaque void* via nv_vma.h.
 *
 * Haskell sees NvAllocator* as an opaque Ptr ().
 */

#ifndef NV_ALLOCATOR_H
#define NV_ALLOCATOR_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"
#include "nv_instance.h"
#include "nv_vma.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvAllocator {
    NvVmaAllocator vma;
    VkDevice       device;
} NvAllocator;

typedef struct NvAllocBuffer {
    VkBuffer        handle;
    NvVmaAllocation allocation;
    NvVmaAllocator  vma;
    VkDeviceSize    size;
} NvAllocBuffer;

typedef struct NvAllocImage {
    VkImage         handle;
    NvVmaAllocation allocation;
    NvVmaAllocator  vma;
    VkFormat        format;
    uint32_t        width;
    uint32_t        height;
    uint32_t        mip_levels;
} NvAllocImage;

/* ----------------------------------------------------------------
 * Allocator lifecycle
 * ---------------------------------------------------------------- */

/* Create a VMA-backed allocator.  Returns NULL on failure. */
NvAllocator *nv_allocator_create(NvInstance *inst, NvDevice *dev);

/* Destroy the allocator.  All buffers/images must be freed first. */
void nv_allocator_destroy(NvAllocator *alloc);

/* ----------------------------------------------------------------
 * Buffer operations
 * ---------------------------------------------------------------- */

/* Create a device-local buffer via staged upload.
 * Allocates a host-visible staging buffer, copies data, transfers
 * to device-local memory via a one-shot command buffer.
 * Returns NULL on failure. */
NvAllocBuffer *nv_alloc_buffer_staged(NvAllocator *alloc,
                                       NvDevice *dev,
                                       VkBufferUsageFlags usage,
                                       const void *data,
                                       uint32_t byte_size);

/* Create a host-visible buffer (for UBOs updated per frame).
 * Returns NULL on failure. */
NvAllocBuffer *nv_alloc_buffer_host(NvAllocator *alloc,
                                     VkDeviceSize size,
                                     VkBufferUsageFlags usage);

/* Destroy a buffer and free its allocation. */
void nv_alloc_buffer_destroy(NvAllocBuffer *buf);

/* Map a host-visible buffer.  Returns NULL on failure. */
void *nv_alloc_buffer_map(NvAllocBuffer *buf);

/* Unmap a previously mapped buffer. */
void nv_alloc_buffer_unmap(NvAllocBuffer *buf);

/* ----------------------------------------------------------------
 * Image operations
 * ---------------------------------------------------------------- */

/* Create a device-local image.  Returns NULL on failure. */
NvAllocImage *nv_alloc_image_create(NvAllocator *alloc,
                                     uint32_t width,
                                     uint32_t height,
                                     uint32_t mip_levels,
                                     VkFormat format,
                                     VkImageUsageFlags usage);

/* Destroy an image and free its allocation. */
void nv_alloc_image_destroy(NvAllocImage *img);

#endif /* NV_ALLOCATOR_H */
