/*
 * nv_descriptor — Vulkan descriptor set layout, pool, and writes.
 */

#include "nv_descriptor.h"

#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * Layout lifecycle
 * ---------------------------------------------------------------- */

NvDescriptorLayout *nv_descriptor_layout_create(
    NvDevice *dev,
    const NvDescriptorBinding *bindings,
    uint32_t binding_count) {
    if (!dev || !bindings || binding_count == 0
        || binding_count > NV_MAX_DESCRIPTOR_BINDINGS) {
        return NULL;
    }

    VkDescriptorSetLayoutBinding vk_bindings[NV_MAX_DESCRIPTOR_BINDINGS];
    memset(vk_bindings, 0, sizeof(vk_bindings));

    for (uint32_t i = 0; i < binding_count; i++) {
        vk_bindings[i].binding         = bindings[i].binding;
        vk_bindings[i].descriptorType  = bindings[i].type;
        vk_bindings[i].descriptorCount = bindings[i].count > 0
                                             ? bindings[i].count
                                             : 1;
        vk_bindings[i].stageFlags      = bindings[i].stages;
    }

    VkDescriptorSetLayoutCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    info.bindingCount = binding_count;
    info.pBindings    = vk_bindings;

    VkDescriptorSetLayout handle = VK_NULL_HANDLE;
    if (vkCreateDescriptorSetLayout(dev->handle, &info, NULL,
                                     &handle)
        != VK_SUCCESS) {
        return NULL;
    }

    NvDescriptorLayout *layout = calloc(1, sizeof(NvDescriptorLayout));
    if (!layout) {
        vkDestroyDescriptorSetLayout(dev->handle, handle, NULL);
        return NULL;
    }
    layout->device        = dev->handle;
    layout->handle        = handle;
    layout->binding_count = binding_count;
    memcpy(layout->bindings, bindings,
           binding_count * sizeof(NvDescriptorBinding));
    return layout;
}

void nv_descriptor_layout_destroy(NvDescriptorLayout *layout) {
    if (!layout) {
        return;
    }
    if (layout->handle != VK_NULL_HANDLE) {
        vkDestroyDescriptorSetLayout(
            layout->device, layout->handle, NULL);
    }
    free(layout);
}

VkDescriptorSetLayout
nv_descriptor_layout_handle(const NvDescriptorLayout *layout) {
    return layout ? layout->handle : VK_NULL_HANDLE;
}

/* ----------------------------------------------------------------
 * Pool lifecycle
 * ---------------------------------------------------------------- */

NvDescriptorPool *nv_descriptor_pool_create(
    NvDevice *dev,
    uint32_t max_sets,
    const VkDescriptorPoolSize *pool_sizes,
    uint32_t pool_size_count) {
    if (!dev || max_sets == 0 || !pool_sizes
        || pool_size_count == 0) {
        return NULL;
    }

    VkDescriptorPoolCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType         = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
    info.maxSets       = max_sets;
    info.poolSizeCount = pool_size_count;
    info.pPoolSizes    = pool_sizes;

    VkDescriptorPool handle = VK_NULL_HANDLE;
    if (vkCreateDescriptorPool(dev->handle, &info, NULL, &handle)
        != VK_SUCCESS) {
        return NULL;
    }

    NvDescriptorPool *pool = calloc(1, sizeof(NvDescriptorPool));
    if (!pool) {
        vkDestroyDescriptorPool(dev->handle, handle, NULL);
        return NULL;
    }
    pool->device   = dev->handle;
    pool->handle   = handle;
    pool->max_sets = max_sets;
    return pool;
}

void nv_descriptor_pool_destroy(NvDescriptorPool *pool) {
    if (!pool) {
        return;
    }
    if (pool->handle != VK_NULL_HANDLE) {
        vkDestroyDescriptorPool(pool->device, pool->handle, NULL);
    }
    free(pool);
}

void nv_descriptor_pool_reset(NvDescriptorPool *pool) {
    if (pool && pool->handle != VK_NULL_HANDLE) {
        vkResetDescriptorPool(pool->device, pool->handle, 0);
    }
}

/* ----------------------------------------------------------------
 * Set allocation
 * ---------------------------------------------------------------- */

uint64_t nv_descriptor_set_allocate(
    NvDescriptorPool *pool,
    NvDescriptorLayout *layout) {
    if (!pool || !layout) {
        return 0;
    }

    VkDescriptorSetAllocateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType              = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    info.descriptorPool     = pool->handle;
    info.descriptorSetCount = 1;
    info.pSetLayouts        = &layout->handle;

    VkDescriptorSet set = VK_NULL_HANDLE;
    if (vkAllocateDescriptorSets(pool->device, &info, &set)
        != VK_SUCCESS) {
        return 0;
    }
    return (uint64_t)(uintptr_t)set;
}

/* ----------------------------------------------------------------
 * Write helpers
 * ---------------------------------------------------------------- */

void nv_descriptor_write_buffer(
    VkDevice device,
    uint64_t descriptor_set,
    uint32_t binding,
    VkDescriptorType type,
    VkBuffer buffer,
    uint64_t offset,
    uint64_t range) {
    VkDescriptorBufferInfo buf_info;
    memset(&buf_info, 0, sizeof(buf_info));
    buf_info.buffer = buffer;
    buf_info.offset = (VkDeviceSize)offset;
    buf_info.range  = (VkDeviceSize)range;

    VkWriteDescriptorSet write;
    memset(&write, 0, sizeof(write));
    write.sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    write.dstSet          = (VkDescriptorSet)(uintptr_t)descriptor_set;
    write.dstBinding      = binding;
    write.descriptorCount = 1;
    write.descriptorType  = type;
    write.pBufferInfo     = &buf_info;

    vkUpdateDescriptorSets(device, 1, &write, 0, NULL);
}

void nv_descriptor_write_image(
    VkDevice device,
    uint64_t descriptor_set,
    uint32_t binding,
    VkDescriptorType type,
    VkImageView view,
    VkSampler sampler,
    VkImageLayout image_layout) {
    VkDescriptorImageInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.imageView   = view;
    img_info.sampler     = sampler;
    img_info.imageLayout = image_layout;

    VkWriteDescriptorSet write;
    memset(&write, 0, sizeof(write));
    write.sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
    write.dstSet          = (VkDescriptorSet)(uintptr_t)descriptor_set;
    write.dstBinding      = binding;
    write.descriptorCount = 1;
    write.descriptorType  = type;
    write.pImageInfo      = &img_info;

    vkUpdateDescriptorSets(device, 1, &write, 0, NULL);
}
