/*
 * nv_device — Vulkan logical device, queues, and command pool.
 */

#include "nv_device.h"

#include <stdlib.h>
#include <string.h>

#define NV_MAX_DEVICE_EXTENSIONS 8

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvDevice *nv_device_create(NvInstance *inst) {
    if (!inst) {
        return NULL;
    }

    NvDevice *dev = calloc(1, sizeof(NvDevice));
    if (!dev) {
        return NULL;
    }

    /* ---- Queue create infos ---- */
    float priority = 1.0f;
    VkDeviceQueueCreateInfo queue_infos[2];
    uint32_t queue_count = 0;

    memset(&queue_infos[0], 0, sizeof(queue_infos[0]));
    queue_infos[0].sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
    queue_infos[0].queueFamilyIndex = inst->graphics_family;
    queue_infos[0].queueCount       = 1;
    queue_infos[0].pQueuePriorities = &priority;
    queue_count = 1;

    if (inst->present_family != inst->graphics_family) {
        memset(&queue_infos[1], 0, sizeof(queue_infos[1]));
        queue_infos[1].sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queue_infos[1].queueFamilyIndex = inst->present_family;
        queue_infos[1].queueCount       = 1;
        queue_infos[1].pQueuePriorities = &priority;
        queue_count = 2;
    }

    /* ---- Device extensions ---- */
    const char *extensions[NV_MAX_DEVICE_EXTENSIONS];
    uint32_t ext_count = 0;

    extensions[ext_count++] = VK_KHR_SWAPCHAIN_EXTENSION_NAME;
    if (inst->has_portability_subset) {
        extensions[ext_count++] = "VK_KHR_portability_subset";
    }

    /* ---- Required device features ---- */
    VkPhysicalDeviceFeatures features;
    memset(&features, 0, sizeof(features));
    features.depthClamp = VK_TRUE;

    /* ---- Logical device ---- */
    VkDeviceCreateInfo create_info;
    memset(&create_info, 0, sizeof(create_info));
    create_info.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    create_info.queueCreateInfoCount    = queue_count;
    create_info.pQueueCreateInfos       = queue_infos;
    create_info.enabledExtensionCount   = ext_count;
    create_info.ppEnabledExtensionNames = extensions;
    create_info.pEnabledFeatures        = &features;

    if (vkCreateDevice(inst->physical_device, &create_info, NULL,
                       &dev->handle)
        != VK_SUCCESS) {
        goto fail_alloc;
    }

    /* ---- Queues ---- */
    vkGetDeviceQueue(
        dev->handle, inst->graphics_family, 0, &dev->graphics_queue);
    vkGetDeviceQueue(
        dev->handle, inst->present_family, 0, &dev->present_queue);

    dev->graphics_family = inst->graphics_family;
    dev->present_family  = inst->present_family;

    /* ---- Command pool ---- */
    VkCommandPoolCreateInfo pool_info;
    memset(&pool_info, 0, sizeof(pool_info));
    pool_info.sType            = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    pool_info.flags            = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
    pool_info.queueFamilyIndex = inst->graphics_family;

    if (vkCreateCommandPool(dev->handle, &pool_info, NULL,
                            &dev->command_pool)
        != VK_SUCCESS) {
        goto fail_device;
    }

    return dev;

fail_device:
    vkDestroyDevice(dev->handle, NULL);
fail_alloc:
    free(dev);
    return NULL;
}

void nv_device_destroy(NvDevice *dev) {
    if (!dev) {
        return;
    }
    if (dev->command_pool != VK_NULL_HANDLE) {
        vkDestroyCommandPool(dev->handle, dev->command_pool, NULL);
    }
    if (dev->handle != VK_NULL_HANDLE) {
        vkDestroyDevice(dev->handle, NULL);
    }
    free(dev);
}

void nv_device_wait_idle(NvDevice *dev) {
    if (dev && dev->handle != VK_NULL_HANDLE) {
        vkDeviceWaitIdle(dev->handle);
    }
}
