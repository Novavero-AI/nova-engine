/*
 * nv_buffer — Vulkan vertex and index buffer upload.
 */

#include "nv_buffer.h"

#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * Memory type selection
 * ---------------------------------------------------------------- */

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
 * One-shot command buffer
 * ---------------------------------------------------------------- */

static VkCommandBuffer begin_single_command(VkDevice device,
                                            VkCommandPool pool) {
    VkCommandBufferAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType       = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    alloc_info.commandPool = pool;
    alloc_info.level       = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    alloc_info.commandBufferCount = 1;

    VkCommandBuffer cmd = VK_NULL_HANDLE;
    vkAllocateCommandBuffers(device, &alloc_info, &cmd);

    VkCommandBufferBeginInfo begin;
    memset(&begin, 0, sizeof(begin));
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    vkBeginCommandBuffer(cmd, &begin);

    return cmd;
}

static void end_single_command(VkDevice device, VkCommandPool pool,
                               VkQueue queue, VkCommandBuffer cmd) {
    vkEndCommandBuffer(cmd);

    VkSubmitInfo submit;
    memset(&submit, 0, sizeof(submit));
    submit.sType              = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers    = &cmd;

    vkQueueSubmit(queue, 1, &submit, VK_NULL_HANDLE);
    vkQueueWaitIdle(queue);
    vkFreeCommandBuffers(device, pool, 1, &cmd);
}

/* ----------------------------------------------------------------
 * Staged buffer creation
 * ---------------------------------------------------------------- */

static NvBuffer *create_buffer_staged(NvDevice *dev, NvInstance *inst,
                                      VkBufferUsageFlags usage,
                                      const void *data,
                                      uint32_t byte_size) {
    if (!dev || !inst || !data || byte_size == 0) {
        return NULL;
    }

    VkDevice         device      = dev->handle;
    VkPhysicalDevice phys_device = inst->physical_device;

    /* ---- Staging buffer (host-visible) ---- */
    VkBuffer       staging_buf = VK_NULL_HANDLE;
    VkDeviceMemory staging_mem = VK_NULL_HANDLE;

    VkBufferCreateInfo staging_info;
    memset(&staging_info, 0, sizeof(staging_info));
    staging_info.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    staging_info.size        = byte_size;
    staging_info.usage       = VK_BUFFER_USAGE_TRANSFER_SRC_BIT;
    staging_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

    if (vkCreateBuffer(device, &staging_info, NULL, &staging_buf)
        != VK_SUCCESS) {
        return NULL;
    }

    VkMemoryRequirements staging_reqs;
    vkGetBufferMemoryRequirements(device, staging_buf, &staging_reqs);

    uint32_t staging_type = find_memory_type(
        phys_device, staging_reqs.memoryTypeBits,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
            | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
    if (staging_type == UINT32_MAX) {
        vkDestroyBuffer(device, staging_buf, NULL);
        return NULL;
    }

    VkMemoryAllocateInfo staging_alloc;
    memset(&staging_alloc, 0, sizeof(staging_alloc));
    staging_alloc.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    staging_alloc.allocationSize  = staging_reqs.size;
    staging_alloc.memoryTypeIndex = staging_type;

    if (vkAllocateMemory(device, &staging_alloc, NULL, &staging_mem)
        != VK_SUCCESS) {
        vkDestroyBuffer(device, staging_buf, NULL);
        return NULL;
    }
    vkBindBufferMemory(device, staging_buf, staging_mem, 0);

    /* Map, copy, unmap */
    void *mapped = NULL;
    vkMapMemory(device, staging_mem, 0, byte_size, 0, &mapped);
    memcpy(mapped, data, byte_size);
    vkUnmapMemory(device, staging_mem);

    /* ---- Device-local buffer ---- */
    VkBuffer       gpu_buf = VK_NULL_HANDLE;
    VkDeviceMemory gpu_mem = VK_NULL_HANDLE;

    VkBufferCreateInfo gpu_info;
    memset(&gpu_info, 0, sizeof(gpu_info));
    gpu_info.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    gpu_info.size        = byte_size;
    gpu_info.usage       = usage | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
    gpu_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

    if (vkCreateBuffer(device, &gpu_info, NULL, &gpu_buf)
        != VK_SUCCESS) {
        goto fail_staging;
    }

    VkMemoryRequirements gpu_reqs;
    vkGetBufferMemoryRequirements(device, gpu_buf, &gpu_reqs);

    uint32_t gpu_type = find_memory_type(
        phys_device, gpu_reqs.memoryTypeBits,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
    if (gpu_type == UINT32_MAX) {
        goto fail_gpu;
    }

    VkMemoryAllocateInfo gpu_alloc;
    memset(&gpu_alloc, 0, sizeof(gpu_alloc));
    gpu_alloc.sType           = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    gpu_alloc.allocationSize  = gpu_reqs.size;
    gpu_alloc.memoryTypeIndex = gpu_type;

    if (vkAllocateMemory(device, &gpu_alloc, NULL, &gpu_mem)
        != VK_SUCCESS) {
        goto fail_gpu;
    }
    vkBindBufferMemory(device, gpu_buf, gpu_mem, 0);

    /* ---- Transfer staging → device-local ---- */
    {
        VkCommandBuffer cmd = begin_single_command(
            device, dev->command_pool);

        VkBufferCopy region;
        memset(&region, 0, sizeof(region));
        region.size = byte_size;
        vkCmdCopyBuffer(cmd, staging_buf, gpu_buf, 1, &region);

        end_single_command(
            device, dev->command_pool, dev->graphics_queue, cmd);
    }

    /* Clean up staging */
    vkDestroyBuffer(device, staging_buf, NULL);
    vkFreeMemory(device, staging_mem, NULL);

    /* Build result */
    NvBuffer *buf = calloc(1, sizeof(NvBuffer));
    if (!buf) {
        vkDestroyBuffer(device, gpu_buf, NULL);
        vkFreeMemory(device, gpu_mem, NULL);
        return NULL;
    }
    buf->device = device;
    buf->handle = gpu_buf;
    buf->memory = gpu_mem;
    buf->size   = byte_size;
    return buf;

fail_gpu:
    vkDestroyBuffer(device, gpu_buf, NULL);
fail_staging:
    vkDestroyBuffer(device, staging_buf, NULL);
    vkFreeMemory(device, staging_mem, NULL);
    return NULL;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvBuffer *nv_buffer_create_vertex(NvDevice *dev, NvInstance *inst,
                                  const void *data,
                                  uint32_t byte_size) {
    return create_buffer_staged(
        dev, inst, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, data,
        byte_size);
}

NvBuffer *nv_buffer_create_index(NvDevice *dev, NvInstance *inst,
                                 const void *data,
                                 uint32_t byte_size) {
    return create_buffer_staged(
        dev, inst, VK_BUFFER_USAGE_INDEX_BUFFER_BIT, data,
        byte_size);
}

void nv_buffer_destroy(NvBuffer *buf) {
    if (!buf) {
        return;
    }
    if (buf->handle != VK_NULL_HANDLE) {
        vkDestroyBuffer(buf->device, buf->handle, NULL);
    }
    if (buf->memory != VK_NULL_HANDLE) {
        vkFreeMemory(buf->device, buf->memory, NULL);
    }
    free(buf);
}
