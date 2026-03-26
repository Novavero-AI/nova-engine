/*
 * nv_swapchain — Vulkan swapchain, image views, and depth buffer.
 */

#include "nv_swapchain.h"

#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * Helpers
 * ---------------------------------------------------------------- */

static uint32_t clamp_u32(uint32_t val, uint32_t lo, uint32_t hi) {
    if (val < lo) { return lo; }
    if (val > hi) { return hi; }
    return val;
}

static uint32_t find_memory_type(VkPhysicalDevice phys_device,
                                 uint32_t type_filter,
                                 VkMemoryPropertyFlags properties) {
    VkPhysicalDeviceMemoryProperties mem_props;
    vkGetPhysicalDeviceMemoryProperties(phys_device, &mem_props);

    for (uint32_t i = 0; i < mem_props.memoryTypeCount; i++) {
        if ((type_filter & (1u << i))
            && (mem_props.memoryTypes[i].propertyFlags & properties)
                   == properties) {
            return i;
        }
    }
    return UINT32_MAX;
}

/* ----------------------------------------------------------------
 * Format / present mode / extent selection
 * ---------------------------------------------------------------- */

static VkSurfaceFormatKHR
choose_surface_format(VkPhysicalDevice device, VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, NULL);

    VkSurfaceFormatKHR *formats = malloc(count * sizeof(*formats));
    if (!formats) {
        VkSurfaceFormatKHR fallback;
        memset(&fallback, 0, sizeof(fallback));
        return fallback;
    }
    vkGetPhysicalDeviceSurfaceFormatsKHR(
        device, surface, &count, formats);

    VkSurfaceFormatKHR chosen = formats[0];
    for (uint32_t i = 0; i < count; i++) {
        if (formats[i].format == VK_FORMAT_B8G8R8A8_SRGB
            && formats[i].colorSpace
                   == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
            chosen = formats[i];
            break;
        }
    }
    free(formats);
    return chosen;
}

static VkPresentModeKHR
choose_present_mode(VkPhysicalDevice device, VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceSurfacePresentModesKHR(
        device, surface, &count, NULL);

    VkPresentModeKHR *modes = malloc(count * sizeof(*modes));
    if (!modes) {
        return VK_PRESENT_MODE_FIFO_KHR;
    }
    vkGetPhysicalDeviceSurfacePresentModesKHR(
        device, surface, &count, modes);

    VkPresentModeKHR chosen = VK_PRESENT_MODE_FIFO_KHR;
    for (uint32_t i = 0; i < count; i++) {
        if (modes[i] == VK_PRESENT_MODE_MAILBOX_KHR) {
            chosen = VK_PRESENT_MODE_MAILBOX_KHR;
            break;
        }
    }
    free(modes);
    return chosen;
}

static VkExtent2D
choose_extent(const VkSurfaceCapabilitiesKHR *caps, NvWindow *window) {
    if (caps->currentExtent.width != UINT32_MAX) {
        return caps->currentExtent;
    }
    uint32_t w = 0;
    uint32_t h = 0;
    nv_window_drawable_size(window, &w, &h);

    VkExtent2D extent;
    extent.width  = clamp_u32(w, caps->minImageExtent.width,
                              caps->maxImageExtent.width);
    extent.height = clamp_u32(h, caps->minImageExtent.height,
                              caps->maxImageExtent.height);
    return extent;
}

/* ----------------------------------------------------------------
 * Depth format
 * ---------------------------------------------------------------- */

static VkFormat find_depth_format(VkPhysicalDevice device) {
    VkFormat candidates[] = {
        VK_FORMAT_D32_SFLOAT,
        VK_FORMAT_D32_SFLOAT_S8_UINT,
        VK_FORMAT_D24_UNORM_S8_UINT,
    };
    uint32_t n = sizeof(candidates) / sizeof(candidates[0]);

    for (uint32_t i = 0; i < n; i++) {
        VkFormatProperties props;
        vkGetPhysicalDeviceFormatProperties(
            device, candidates[i], &props);
        if (props.optimalTilingFeatures
            & VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT) {
            return candidates[i];
        }
    }
    return VK_FORMAT_UNDEFINED;
}

/* ----------------------------------------------------------------
 * Depth resource creation / destruction
 * ---------------------------------------------------------------- */

static void destroy_depth(NvSwapchain *sc) {
    if (sc->depth_view != VK_NULL_HANDLE) {
        vkDestroyImageView(sc->device, sc->depth_view, NULL);
        sc->depth_view = VK_NULL_HANDLE;
    }
    if (sc->depth_image != VK_NULL_HANDLE) {
        vkDestroyImage(sc->device, sc->depth_image, NULL);
        sc->depth_image = VK_NULL_HANDLE;
    }
    if (sc->depth_memory != VK_NULL_HANDLE) {
        vkFreeMemory(sc->device, sc->depth_memory, NULL);
        sc->depth_memory = VK_NULL_HANDLE;
    }
}

static int create_depth(NvSwapchain *sc) {
    sc->depth_format = find_depth_format(sc->physical_device);
    if (sc->depth_format == VK_FORMAT_UNDEFINED) {
        return 0;
    }

    /* Image */
    VkImageCreateInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    img_info.imageType     = VK_IMAGE_TYPE_2D;
    img_info.format        = sc->depth_format;
    img_info.extent.width  = sc->extent.width;
    img_info.extent.height = sc->extent.height;
    img_info.extent.depth  = 1;
    img_info.mipLevels     = 1;
    img_info.arrayLayers   = 1;
    img_info.samples       = VK_SAMPLE_COUNT_1_BIT;
    img_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
    img_info.usage         = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
    img_info.sharingMode   = VK_SHARING_MODE_EXCLUSIVE;
    img_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    if (vkCreateImage(sc->device, &img_info, NULL, &sc->depth_image)
        != VK_SUCCESS) {
        return 0;
    }

    /* Memory */
    VkMemoryRequirements reqs;
    vkGetImageMemoryRequirements(sc->device, sc->depth_image, &reqs);

    uint32_t mem_type = find_memory_type(
        sc->physical_device, reqs.memoryTypeBits,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
    if (mem_type == UINT32_MAX) {
        vkDestroyImage(sc->device, sc->depth_image, NULL);
        sc->depth_image = VK_NULL_HANDLE;
        return 0;
    }

    VkMemoryAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    alloc_info.allocationSize  = reqs.size;
    alloc_info.memoryTypeIndex = mem_type;

    if (vkAllocateMemory(sc->device, &alloc_info, NULL,
                         &sc->depth_memory)
        != VK_SUCCESS) {
        vkDestroyImage(sc->device, sc->depth_image, NULL);
        sc->depth_image = VK_NULL_HANDLE;
        return 0;
    }

    vkBindImageMemory(sc->device, sc->depth_image, sc->depth_memory, 0);

    /* View */
    VkImageViewCreateInfo view_info;
    memset(&view_info, 0, sizeof(view_info));
    view_info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    view_info.image    = sc->depth_image;
    view_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
    view_info.format   = sc->depth_format;
    view_info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT;
    view_info.subresourceRange.levelCount = 1;
    view_info.subresourceRange.layerCount = 1;

    if (vkCreateImageView(sc->device, &view_info, NULL,
                          &sc->depth_view)
        != VK_SUCCESS) {
        destroy_depth(sc);
        return 0;
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Image view creation / destruction
 * ---------------------------------------------------------------- */

static void destroy_views(NvSwapchain *sc) {
    if (sc->image_views) {
        for (uint32_t i = 0; i < sc->image_count; i++) {
            if (sc->image_views[i] != VK_NULL_HANDLE) {
                vkDestroyImageView(
                    sc->device, sc->image_views[i], NULL);
            }
        }
        free(sc->image_views);
        sc->image_views = NULL;
    }
    free(sc->images);
    sc->images      = NULL;
    sc->image_count = 0;
}

static int create_views(NvSwapchain *sc) {
    vkGetSwapchainImagesKHR(
        sc->device, sc->handle, &sc->image_count, NULL);

    sc->images = malloc(sc->image_count * sizeof(VkImage));
    if (!sc->images) {
        return 0;
    }
    vkGetSwapchainImagesKHR(
        sc->device, sc->handle, &sc->image_count, sc->images);

    sc->image_views = calloc(sc->image_count, sizeof(VkImageView));
    if (!sc->image_views) {
        return 0;
    }

    for (uint32_t i = 0; i < sc->image_count; i++) {
        VkImageViewCreateInfo info;
        memset(&info, 0, sizeof(info));
        info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        info.image    = sc->images[i];
        info.viewType = VK_IMAGE_VIEW_TYPE_2D;
        info.format   = sc->image_format;
        info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        info.subresourceRange.levelCount = 1;
        info.subresourceRange.layerCount = 1;

        if (vkCreateImageView(sc->device, &info, NULL,
                              &sc->image_views[i])
            != VK_SUCCESS) {
            return 0;
        }
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Core swapchain build (used by create and recreate)
 * ---------------------------------------------------------------- */

static int build_swapchain(NvSwapchain *sc, NvWindow *window) {
    VkSurfaceCapabilitiesKHR caps;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(
        sc->physical_device, sc->surface, &caps);

    VkSurfaceFormatKHR format =
        choose_surface_format(sc->physical_device, sc->surface);
    VkPresentModeKHR present_mode =
        choose_present_mode(sc->physical_device, sc->surface);
    VkExtent2D extent = choose_extent(&caps, window);

    uint32_t img_count = caps.minImageCount + 1;
    if (caps.maxImageCount > 0 && img_count > caps.maxImageCount) {
        img_count = caps.maxImageCount;
    }

    VkSwapchainKHR old_handle = sc->handle;

    VkSwapchainCreateInfoKHR ci;
    memset(&ci, 0, sizeof(ci));
    ci.sType            = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
    ci.surface          = sc->surface;
    ci.minImageCount    = img_count;
    ci.imageFormat      = format.format;
    ci.imageColorSpace  = format.colorSpace;
    ci.imageExtent      = extent;
    ci.imageArrayLayers = 1;
    ci.imageUsage       = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
    ci.preTransform     = caps.currentTransform;
    ci.compositeAlpha   = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
    ci.presentMode      = present_mode;
    ci.clipped          = VK_TRUE;
    ci.oldSwapchain     = old_handle;

    uint32_t families[2];
    if (sc->graphics_family != sc->present_family) {
        families[0]              = sc->graphics_family;
        families[1]              = sc->present_family;
        ci.imageSharingMode      = VK_SHARING_MODE_CONCURRENT;
        ci.queueFamilyIndexCount = 2;
        ci.pQueueFamilyIndices   = families;
    } else {
        ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    VkSwapchainKHR new_handle;
    if (vkCreateSwapchainKHR(sc->device, &ci, NULL, &new_handle)
        != VK_SUCCESS) {
        return 0;
    }

    /* Retire old handle after successful creation. */
    if (old_handle != VK_NULL_HANDLE) {
        vkDestroySwapchainKHR(sc->device, old_handle, NULL);
    }
    sc->handle       = new_handle;
    sc->image_format = format.format;
    sc->extent       = extent;

    if (!create_views(sc)) {
        return 0;
    }
    if (!create_depth(sc)) {
        return 0;
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvSwapchain *nv_swapchain_create(NvInstance *inst, NvDevice *dev,
                                 NvWindow *window) {
    if (!inst || !dev || !window) {
        return NULL;
    }

    NvSwapchain *sc = calloc(1, sizeof(NvSwapchain));
    if (!sc) {
        return NULL;
    }

    sc->device          = dev->handle;
    sc->physical_device = inst->physical_device;
    sc->surface         = inst->surface;
    sc->graphics_family = dev->graphics_family;
    sc->present_family  = dev->present_family;

    if (!build_swapchain(sc, window)) {
        nv_swapchain_destroy(sc);
        return NULL;
    }

    return sc;
}

void nv_swapchain_destroy(NvSwapchain *sc) {
    if (!sc) {
        return;
    }
    destroy_depth(sc);
    destroy_views(sc);
    if (sc->handle != VK_NULL_HANDLE) {
        vkDestroySwapchainKHR(sc->device, sc->handle, NULL);
    }
    free(sc);
}

int nv_swapchain_recreate(NvSwapchain *sc, NvWindow *window) {
    if (!sc || !window) {
        return 0;
    }
    destroy_depth(sc);
    destroy_views(sc);
    /* sc->handle is passed as oldSwapchain inside build_swapchain */
    return build_swapchain(sc, window);
}

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

uint32_t nv_swapchain_image_count(const NvSwapchain *sc) {
    return sc ? sc->image_count : 0;
}

uint32_t nv_swapchain_width(const NvSwapchain *sc) {
    return sc ? sc->extent.width : 0;
}

uint32_t nv_swapchain_height(const NvSwapchain *sc) {
    return sc ? sc->extent.height : 0;
}
