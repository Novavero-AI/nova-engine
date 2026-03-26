/*
 * nv_swapchain — Vulkan swapchain, image views, and depth buffer.
 *
 * Creates and manages the swapchain with SRGB format preference,
 * mailbox present mode, HiDPI-aware extent, per-image views, and
 * a matching depth buffer.  Supports recreation on window resize.
 *
 * Caches VkDevice / VkPhysicalDevice / VkSurfaceKHR internally so
 * destroy needs only the NvSwapchain pointer (ForeignPtr-friendly).
 *
 * Haskell sees NvSwapchain* as an opaque Ptr ().
 */

#ifndef NV_SWAPCHAIN_H
#define NV_SWAPCHAIN_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"
#include "nv_instance.h"
#include "nv_window.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvSwapchain {
    /* Cached handles for cleanup and recreation */
    VkDevice         device;
    VkPhysicalDevice physical_device;
    VkSurfaceKHR     surface;
    uint32_t         graphics_family;
    uint32_t         present_family;

    /* Swapchain */
    VkSwapchainKHR handle;
    VkFormat       image_format;
    VkExtent2D     extent;
    VkImage       *images;
    VkImageView   *image_views;
    uint32_t       image_count;

    /* Depth (VMA-backed) */
    NvAllocImage  *depth_alloc;
    VkImageView    depth_view;
    VkFormat       depth_format;

    /* Back-reference for depth recreation */
    NvAllocator   *allocator;
} NvSwapchain;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a swapchain with image views and depth buffer.
 * Prefers B8G8R8A8_SRGB format, mailbox present mode, and
 * D32_SFLOAT depth.  Returns NULL on failure. */
NvSwapchain *nv_swapchain_create(NvInstance *inst, NvDevice *dev,
                                 NvAllocator *alloc,
                                 NvWindow *window);

/* Destroy image views, depth resources, swapchain, and free.
 * Uses cached device handle — no extra arguments needed. */
void nv_swapchain_destroy(NvSwapchain *sc);

/* Recreate swapchain after a window resize.
 * Passes the old handle to the driver for resource reuse.
 * Returns 1 on success, 0 on failure. */
int nv_swapchain_recreate(NvSwapchain *sc, NvWindow *window);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

uint32_t nv_swapchain_image_count(const NvSwapchain *sc);
uint32_t nv_swapchain_width(const NvSwapchain *sc);
uint32_t nv_swapchain_height(const NvSwapchain *sc);

#endif /* NV_SWAPCHAIN_H */
