/*
 * nv_instance — Vulkan instance, surface, and physical device.
 *
 * Creates a VkInstance with required extensions (SDL3, debug,
 * portability), sets up a debug messenger, creates a Vulkan
 * surface via the NvWindow, and selects the best available GPU.
 *
 * Haskell sees NvInstance* as an opaque Ptr ().
 */

#ifndef NV_INSTANCE_H
#define NV_INSTANCE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_window.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvInstance {
    VkInstance                       handle;
    VkDebugUtilsMessengerEXT        debug_messenger;
    VkSurfaceKHR                    surface;
    VkPhysicalDevice                physical_device;
    VkPhysicalDeviceProperties      device_props;
    VkPhysicalDeviceMemoryProperties mem_props;
    uint32_t                        graphics_family;
    uint32_t                        present_family;
    int                             has_portability_subset;
} NvInstance;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a Vulkan instance, surface, and select a physical device.
 *
 * Gathers SDL3 required extensions, optionally enables validation
 * layers and a debug messenger.  Creates a Vulkan surface from
 * the given window.  Selects the highest-scoring GPU that supports
 * graphics + present + swapchain.
 *
 * Returns NULL on failure. */
NvInstance *nv_instance_create(NvWindow *window, const char *app_name,
                               int enable_validation);

/* Destroy the surface, debug messenger, and Vulkan instance. */
void nv_instance_destroy(NvInstance *inst);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

/* Name of the selected GPU (e.g. "Apple M2 Pro").
 * Pointer is valid for the lifetime of the NvInstance. */
const char *nv_instance_device_name(const NvInstance *inst);

#endif /* NV_INSTANCE_H */
