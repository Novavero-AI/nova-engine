/*
 * nv_vma — C99-compatible thin wrapper around VMA.
 *
 * VMA is C++ internally.  This header declares C-linkage functions
 * that nv_allocator.c (and other C99 code) can call without ever
 * including the VMA header.  Opaque pointers hide VmaAllocator and
 * VmaAllocation behind void*.
 */

#ifndef NV_VMA_H
#define NV_VMA_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Opaque handles — void* in C, cast to real VMA types in C++. */
typedef void *NvVmaAllocator;
typedef void *NvVmaAllocation;

/* ----------------------------------------------------------------
 * Allocator lifecycle
 * ---------------------------------------------------------------- */

/* Create a VMA allocator.  Returns NULL on failure. */
NvVmaAllocator nv_vma_create(VkInstance instance,
                              VkPhysicalDevice physical_device,
                              VkDevice device,
                              uint32_t vulkan_api_version);

/* Destroy a VMA allocator. */
void nv_vma_destroy(NvVmaAllocator allocator);

/* ----------------------------------------------------------------
 * Buffer operations
 * ---------------------------------------------------------------- */

/* Create a buffer + allocation in one call.
 * gpu_only: 1 = VMA_MEMORY_USAGE_AUTO with DEVICE_LOCAL preference
 *           0 = VMA_MEMORY_USAGE_AUTO with HOST_SEQUENTIAL_WRITE
 * Returns 1 on success, 0 on failure. */
int nv_vma_create_buffer(NvVmaAllocator allocator,
                          VkDeviceSize size,
                          VkBufferUsageFlags usage,
                          int gpu_only,
                          VkBuffer *out_buffer,
                          NvVmaAllocation *out_allocation);

/* Destroy a buffer + free its allocation. */
void nv_vma_destroy_buffer(NvVmaAllocator allocator,
                            VkBuffer buffer,
                            NvVmaAllocation allocation);

/* ----------------------------------------------------------------
 * Image operations
 * ---------------------------------------------------------------- */

/* Create an image + allocation.  Always device-local.
 * Returns 1 on success, 0 on failure. */
int nv_vma_create_image(NvVmaAllocator allocator,
                         const VkImageCreateInfo *image_info,
                         VkImage *out_image,
                         NvVmaAllocation *out_allocation);

/* Destroy an image + free its allocation. */
void nv_vma_destroy_image(NvVmaAllocator allocator,
                           VkImage image,
                           NvVmaAllocation allocation);

/* ----------------------------------------------------------------
 * Mapping
 * ---------------------------------------------------------------- */

/* Map a host-visible allocation.  Returns NULL on failure. */
void *nv_vma_map(NvVmaAllocator allocator,
                  NvVmaAllocation allocation);

/* Unmap a previously mapped allocation. */
void nv_vma_unmap(NvVmaAllocator allocator,
                   NvVmaAllocation allocation);

#ifdef __cplusplus
}
#endif

#endif /* NV_VMA_H */
