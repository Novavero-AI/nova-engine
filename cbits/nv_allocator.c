/*
 * nv_allocator — GPU memory allocator backed by VMA.
 */

#include "nv_allocator.h"

#include <stdlib.h>
#include <string.h>

#include "nv_util.h"

/* ----------------------------------------------------------------
 * Allocator lifecycle
 * ---------------------------------------------------------------- */

NvAllocator *nv_allocator_create(NvInstance *inst, NvDevice *dev) {
    if (!inst || !dev) {
        return NULL;
    }

    NvAllocator *alloc = calloc(1, sizeof(NvAllocator));
    if (!alloc) {
        return NULL;
    }

    alloc->device = dev->handle;
    alloc->vma = nv_vma_create(inst->handle, inst->physical_device,
                               dev->handle,
                               VK_API_VERSION_1_2);
    if (!alloc->vma) {
        free(alloc);
        return NULL;
    }

    return alloc;
}

void nv_allocator_destroy(NvAllocator *alloc) {
    if (!alloc) {
        return;
    }
    nv_vma_destroy(alloc->vma);
    free(alloc);
}

/* ----------------------------------------------------------------
 * Buffer operations
 * ---------------------------------------------------------------- */

NvAllocBuffer *nv_alloc_buffer_staged(NvAllocator *alloc,
                                       NvDevice *dev,
                                       VkBufferUsageFlags usage,
                                       const void *data,
                                       uint32_t byte_size) {
    if (!alloc || !dev || !data || byte_size == 0) {
        return NULL;
    }

    /* Staging buffer (host-visible) */
    VkBuffer        staging_buf = VK_NULL_HANDLE;
    NvVmaAllocation staging_alloc = NULL;

    if (!nv_vma_create_buffer(alloc->vma, byte_size,
                               VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                               0, &staging_buf, &staging_alloc)) {
        return NULL;
    }

    /* Map, copy, unmap */
    void *mapped = nv_vma_map(alloc->vma, staging_alloc);
    if (!mapped) {
        nv_vma_destroy_buffer(alloc->vma, staging_buf, staging_alloc);
        return NULL;
    }
    memcpy(mapped, data, byte_size);
    nv_vma_unmap(alloc->vma, staging_alloc);

    /* Device-local buffer */
    VkBuffer        gpu_buf = VK_NULL_HANDLE;
    NvVmaAllocation gpu_alloc = NULL;

    if (!nv_vma_create_buffer(
            alloc->vma, byte_size,
            usage | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
            1, &gpu_buf, &gpu_alloc)) {
        nv_vma_destroy_buffer(alloc->vma, staging_buf, staging_alloc);
        return NULL;
    }

    /* Transfer staging -> device-local */
    {
        VkCommandBuffer cmd = nv_begin_single_command(
            alloc->device, dev->command_pool);

        VkBufferCopy region;
        memset(&region, 0, sizeof(region));
        region.size = byte_size;
        vkCmdCopyBuffer(cmd, staging_buf, gpu_buf, 1, &region);

        nv_end_single_command(
            alloc->device, dev->command_pool,
            dev->graphics_queue, cmd);
    }

    /* Clean up staging */
    nv_vma_destroy_buffer(alloc->vma, staging_buf, staging_alloc);

    /* Build result */
    NvAllocBuffer *buf = calloc(1, sizeof(NvAllocBuffer));
    if (!buf) {
        nv_vma_destroy_buffer(alloc->vma, gpu_buf, gpu_alloc);
        return NULL;
    }
    buf->handle     = gpu_buf;
    buf->allocation = gpu_alloc;
    buf->vma        = alloc->vma;
    buf->size       = byte_size;
    return buf;
}

NvAllocBuffer *nv_alloc_buffer_host(NvAllocator *alloc,
                                     VkDeviceSize size,
                                     VkBufferUsageFlags usage) {
    if (!alloc || size == 0) {
        return NULL;
    }

    VkBuffer        handle = VK_NULL_HANDLE;
    NvVmaAllocation allocation = NULL;

    if (!nv_vma_create_buffer(alloc->vma, size, usage, 0,
                               &handle, &allocation)) {
        return NULL;
    }

    NvAllocBuffer *buf = calloc(1, sizeof(NvAllocBuffer));
    if (!buf) {
        nv_vma_destroy_buffer(alloc->vma, handle, allocation);
        return NULL;
    }
    buf->handle     = handle;
    buf->allocation = allocation;
    buf->vma        = alloc->vma;
    buf->size       = size;
    return buf;
}

void nv_alloc_buffer_destroy(NvAllocBuffer *buf) {
    if (!buf) {
        return;
    }
    nv_vma_destroy_buffer(buf->vma, buf->handle, buf->allocation);
    free(buf);
}

void *nv_alloc_buffer_map(NvAllocBuffer *buf) {
    if (!buf) {
        return NULL;
    }
    return nv_vma_map(buf->vma, buf->allocation);
}

void nv_alloc_buffer_unmap(NvAllocBuffer *buf) {
    if (buf) {
        nv_vma_unmap(buf->vma, buf->allocation);
    }
}

/* ----------------------------------------------------------------
 * Image operations
 * ---------------------------------------------------------------- */

NvAllocImage *nv_alloc_image_create(NvAllocator *alloc,
                                     uint32_t width,
                                     uint32_t height,
                                     uint32_t mip_levels,
                                     VkFormat format,
                                     VkImageUsageFlags usage) {
    if (!alloc || width == 0 || height == 0) {
        return NULL;
    }

    VkImageCreateInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    img_info.imageType     = VK_IMAGE_TYPE_2D;
    img_info.format        = format;
    img_info.extent.width  = width;
    img_info.extent.height = height;
    img_info.extent.depth  = 1;
    img_info.mipLevels     = mip_levels > 0 ? mip_levels : 1;
    img_info.arrayLayers   = 1;
    img_info.samples       = VK_SAMPLE_COUNT_1_BIT;
    img_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
    img_info.usage         = usage;
    img_info.sharingMode   = VK_SHARING_MODE_EXCLUSIVE;
    img_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    VkImage         handle = VK_NULL_HANDLE;
    NvVmaAllocation allocation = NULL;

    if (!nv_vma_create_image(alloc->vma, &img_info, &handle,
                              &allocation)) {
        return NULL;
    }

    NvAllocImage *img = calloc(1, sizeof(NvAllocImage));
    if (!img) {
        nv_vma_destroy_image(alloc->vma, handle, allocation);
        return NULL;
    }
    img->handle     = handle;
    img->allocation = allocation;
    img->vma        = alloc->vma;
    img->format     = format;
    img->width      = width;
    img->height     = height;
    img->mip_levels = img_info.mipLevels;
    return img;
}

void nv_alloc_image_destroy(NvAllocImage *img) {
    if (!img) {
        return;
    }
    nv_vma_destroy_image(img->vma, img->handle, img->allocation);
    free(img);
}
