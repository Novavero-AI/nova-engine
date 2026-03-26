/*
 * nv_descriptor — Vulkan descriptor set layout, pool, and writes.
 *
 * Provides creation of descriptor set layouts, descriptor pools,
 * set allocation, and write helpers for buffers and images.
 *
 * Haskell sees NvDescriptorLayout* and NvDescriptorPool* as
 * opaque Ptr ().
 */

#ifndef NV_DESCRIPTOR_H
#define NV_DESCRIPTOR_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"

#define NV_MAX_DESCRIPTOR_BINDINGS 16

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvDescriptorBinding {
    uint32_t           binding;
    VkDescriptorType   type;
    VkShaderStageFlags stages;
    uint32_t           count;
} NvDescriptorBinding;

typedef struct NvDescriptorLayout {
    VkDevice                device;
    VkDescriptorSetLayout   handle;
    uint32_t                binding_count;
    NvDescriptorBinding     bindings[NV_MAX_DESCRIPTOR_BINDINGS];
} NvDescriptorLayout;

typedef struct NvDescriptorPool {
    VkDevice         device;
    VkDescriptorPool handle;
    uint32_t         max_sets;
} NvDescriptorPool;

/* ----------------------------------------------------------------
 * Layout lifecycle
 * ---------------------------------------------------------------- */

/* Create a descriptor set layout from an array of bindings.
 * Returns NULL on failure. */
NvDescriptorLayout *nv_descriptor_layout_create(
    NvDevice *dev,
    const NvDescriptorBinding *bindings,
    uint32_t binding_count);

/* Destroy a descriptor set layout. */
void nv_descriptor_layout_destroy(NvDescriptorLayout *layout);

/* Get the raw VkDescriptorSetLayout handle. */
VkDescriptorSetLayout
nv_descriptor_layout_handle(const NvDescriptorLayout *layout);

/* ----------------------------------------------------------------
 * Pool lifecycle
 * ---------------------------------------------------------------- */

/* Create a descriptor pool that can allocate up to max_sets sets.
 * pool_sizes is an array of VkDescriptorPoolSize entries that
 * specify the total number of descriptors available per type.
 * Returns NULL on failure. */
NvDescriptorPool *nv_descriptor_pool_create(
    NvDevice *dev,
    uint32_t max_sets,
    const VkDescriptorPoolSize *pool_sizes,
    uint32_t pool_size_count);

/* Destroy a descriptor pool (frees all allocated sets). */
void nv_descriptor_pool_destroy(NvDescriptorPool *pool);

/* Reset the pool, freeing all allocated descriptor sets. */
void nv_descriptor_pool_reset(NvDescriptorPool *pool);

/* ----------------------------------------------------------------
 * Set allocation
 * ---------------------------------------------------------------- */

/* Allocate a single descriptor set from the pool.
 * Returns VK_NULL_HANDLE on failure.
 * The returned handle is a uint64_t (Vulkan non-dispatchable). */
uint64_t nv_descriptor_set_allocate(
    NvDescriptorPool *pool,
    NvDescriptorLayout *layout);

/* ----------------------------------------------------------------
 * Write helpers
 * ---------------------------------------------------------------- */

/* Write a buffer descriptor (UBO or SSBO). */
void nv_descriptor_write_buffer(
    VkDevice device,
    uint64_t descriptor_set,
    uint32_t binding,
    VkDescriptorType type,
    VkBuffer buffer,
    uint64_t offset,
    uint64_t range);

/* Write an image descriptor (combined image sampler, etc). */
void nv_descriptor_write_image(
    VkDevice device,
    uint64_t descriptor_set,
    uint32_t binding,
    VkDescriptorType type,
    VkImageView view,
    VkSampler sampler,
    VkImageLayout image_layout);

#endif /* NV_DESCRIPTOR_H */
