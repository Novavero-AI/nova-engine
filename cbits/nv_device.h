/*
 * nv_device — Vulkan logical device, queues, and command pool.
 *
 * Creates a logical device from the physical device selected by
 * NvInstance, retrieves graphics and present queues, and creates
 * a resettable command pool for the graphics family.
 *
 * Haskell sees NvDevice* as an opaque Ptr ().
 */

#ifndef NV_DEVICE_H
#define NV_DEVICE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_instance.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvDevice {
    VkDevice      handle;
    VkQueue       graphics_queue;
    VkQueue       present_queue;
    VkCommandPool command_pool;
    uint32_t      graphics_family;
    uint32_t      present_family;
} NvDevice;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a logical device with graphics and present queues.
 *
 * Enables VK_KHR_swapchain and VK_KHR_portability_subset (if the
 * physical device requires it).  Creates a resettable command pool
 * on the graphics queue family.
 *
 * Returns NULL on failure. */
NvDevice *nv_device_create(NvInstance *inst);

/* Destroy the command pool and logical device. */
void nv_device_destroy(NvDevice *dev);

/* ----------------------------------------------------------------
 * Operations
 * ---------------------------------------------------------------- */

/* Block until all device operations have completed.
 * Call before cleanup or swapchain recreation. */
void nv_device_wait_idle(NvDevice *dev);

#endif /* NV_DEVICE_H */
