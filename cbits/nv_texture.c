/*
 * nv_texture — Vulkan texture creation, mipmaps, and sampling.
 */

#include "nv_texture.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stb_image.h"

/* ----------------------------------------------------------------
 * One-shot command buffer (same pattern as nv_allocator.c)
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
 * Image layout transition
 * ---------------------------------------------------------------- */

static void transition_image_layout(VkCommandBuffer cmd,
                                     VkImage image,
                                     VkImageLayout old_layout,
                                     VkImageLayout new_layout,
                                     uint32_t mip_levels) {
    VkImageMemoryBarrier barrier;
    memset(&barrier, 0, sizeof(barrier));
    barrier.sType               = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    barrier.oldLayout           = old_layout;
    barrier.newLayout           = new_layout;
    barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.image               = image;
    barrier.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
    barrier.subresourceRange.baseMipLevel   = 0;
    barrier.subresourceRange.levelCount     = mip_levels;
    barrier.subresourceRange.baseArrayLayer = 0;
    barrier.subresourceRange.layerCount     = 1;

    VkPipelineStageFlags src_stage = 0;
    VkPipelineStageFlags dst_stage = 0;

    if (old_layout == VK_IMAGE_LAYOUT_UNDEFINED
        && new_layout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        barrier.srcAccessMask = 0;
        barrier.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        src_stage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        dst_stage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    } else if (old_layout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
               && new_layout
                      == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
        src_stage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        dst_stage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    } else {
        /* Generic fallback */
        barrier.srcAccessMask = VK_ACCESS_MEMORY_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_MEMORY_READ_BIT;
        src_stage = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;
        dst_stage = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;
    }

    vkCmdPipelineBarrier(cmd, src_stage, dst_stage, 0,
                          0, NULL, 0, NULL, 1, &barrier);
}

/* ----------------------------------------------------------------
 * Mipmap generation via blit chain
 * ---------------------------------------------------------------- */

static void gen_mipmaps(VkCommandBuffer cmd, VkImage image,
                         uint32_t width, uint32_t height,
                         uint32_t mip_levels) {
    VkImageMemoryBarrier barrier;
    memset(&barrier, 0, sizeof(barrier));
    barrier.sType               = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    barrier.image               = image;
    barrier.srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
    barrier.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
    barrier.subresourceRange.baseArrayLayer = 0;
    barrier.subresourceRange.layerCount     = 1;
    barrier.subresourceRange.levelCount     = 1;

    int32_t mip_w = (int32_t)width;
    int32_t mip_h = (int32_t)height;

    for (uint32_t i = 1; i < mip_levels; i++) {
        /* Transition mip i-1 from TRANSFER_DST to TRANSFER_SRC */
        barrier.subresourceRange.baseMipLevel = i - 1;
        barrier.oldLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        barrier.newLayout     = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;

        vkCmdPipelineBarrier(cmd,
                              VK_PIPELINE_STAGE_TRANSFER_BIT,
                              VK_PIPELINE_STAGE_TRANSFER_BIT,
                              0, 0, NULL, 0, NULL, 1, &barrier);

        /* Blit from mip i-1 to mip i */
        int32_t next_w = mip_w > 1 ? mip_w / 2 : 1;
        int32_t next_h = mip_h > 1 ? mip_h / 2 : 1;

        VkImageBlit blit;
        memset(&blit, 0, sizeof(blit));
        blit.srcOffsets[1].x                = mip_w;
        blit.srcOffsets[1].y                = mip_h;
        blit.srcOffsets[1].z                = 1;
        blit.srcSubresource.aspectMask      = VK_IMAGE_ASPECT_COLOR_BIT;
        blit.srcSubresource.mipLevel        = i - 1;
        blit.srcSubresource.baseArrayLayer  = 0;
        blit.srcSubresource.layerCount      = 1;
        blit.dstOffsets[1].x                = next_w;
        blit.dstOffsets[1].y                = next_h;
        blit.dstOffsets[1].z                = 1;
        blit.dstSubresource.aspectMask      = VK_IMAGE_ASPECT_COLOR_BIT;
        blit.dstSubresource.mipLevel        = i;
        blit.dstSubresource.baseArrayLayer  = 0;
        blit.dstSubresource.layerCount      = 1;

        vkCmdBlitImage(cmd,
                        image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                        image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                        1, &blit, VK_FILTER_LINEAR);

        /* Transition mip i-1 to SHADER_READ_ONLY */
        barrier.oldLayout     = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        barrier.newLayout     = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;

        vkCmdPipelineBarrier(cmd,
                              VK_PIPELINE_STAGE_TRANSFER_BIT,
                              VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                              0, 0, NULL, 0, NULL, 1, &barrier);

        mip_w = next_w;
        mip_h = next_h;
    }

    /* Transition last mip level to SHADER_READ_ONLY */
    barrier.subresourceRange.baseMipLevel = mip_levels - 1;
    barrier.oldLayout     = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    barrier.newLayout     = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    barrier.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
    barrier.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;

    vkCmdPipelineBarrier(cmd,
                          VK_PIPELINE_STAGE_TRANSFER_BIT,
                          VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT,
                          0, 0, NULL, 0, NULL, 1, &barrier);
}

/* ----------------------------------------------------------------
 * Image view / sampler helpers
 * ---------------------------------------------------------------- */

static VkImageView create_view(VkDevice device, VkImage image,
                                VkFormat format, uint32_t mip_levels) {
    VkImageViewCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    info.image    = image;
    info.viewType = VK_IMAGE_VIEW_TYPE_2D;
    info.format   = format;
    info.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
    info.subresourceRange.baseMipLevel   = 0;
    info.subresourceRange.levelCount     = mip_levels;
    info.subresourceRange.baseArrayLayer = 0;
    info.subresourceRange.layerCount     = 1;

    VkImageView view = VK_NULL_HANDLE;
    if (vkCreateImageView(device, &info, NULL, &view) != VK_SUCCESS) {
        return VK_NULL_HANDLE;
    }
    return view;
}

static VkSampler create_tex_sampler(VkDevice device,
                                     uint32_t mip_levels,
                                     int filter,
                                     int address_mode) {
    VkFilter vk_filter = filter == NV_FILTER_NEAREST
                             ? VK_FILTER_NEAREST
                             : VK_FILTER_LINEAR;

    VkSamplerMipmapMode mipmap_mode = filter == NV_FILTER_NEAREST
                                          ? VK_SAMPLER_MIPMAP_MODE_NEAREST
                                          : VK_SAMPLER_MIPMAP_MODE_LINEAR;

    VkSamplerAddressMode vk_address;
    switch (address_mode) {
        case NV_ADDRESS_CLAMP_EDGE:
            vk_address = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
            break;
        case NV_ADDRESS_MIRROR:
            vk_address = VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
            break;
        default:
            vk_address = VK_SAMPLER_ADDRESS_MODE_REPEAT;
            break;
    }

    VkSamplerCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType        = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
    info.magFilter    = vk_filter;
    info.minFilter    = vk_filter;
    info.mipmapMode   = mipmap_mode;
    info.addressModeU = vk_address;
    info.addressModeV = vk_address;
    info.addressModeW = vk_address;
    info.maxLod       = (float)mip_levels;
    info.borderColor  = VK_BORDER_COLOR_INT_OPAQUE_BLACK;

    VkSampler sampler = VK_NULL_HANDLE;
    if (vkCreateSampler(device, &info, NULL, &sampler) != VK_SUCCESS) {
        return VK_NULL_HANDLE;
    }
    return sampler;
}

/* ----------------------------------------------------------------
 * Utilities
 * ---------------------------------------------------------------- */

uint32_t nv_texture_calc_mip_levels(uint32_t width, uint32_t height) {
    uint32_t max_dim = width > height ? width : height;
    if (max_dim == 0) {
        return 1;
    }
    uint32_t levels = 1;
    while (max_dim > 1) {
        max_dim >>= 1;
        levels++;
    }
    return levels;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvTexture *nv_texture_create(NvDevice *dev, NvAllocator *alloc,
                              uint32_t width, uint32_t height,
                              const uint8_t *rgba_pixels,
                              int generate_mipmaps,
                              int filter,
                              int address_mode) {
    if (!dev || !alloc || !rgba_pixels || width == 0 || height == 0) {
        return NULL;
    }

    uint32_t mip_levels = generate_mipmaps
                              ? nv_texture_calc_mip_levels(width, height)
                              : 1;

    VkImageUsageFlags usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT
                            | VK_IMAGE_USAGE_SAMPLED_BIT;
    if (generate_mipmaps) {
        usage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
    }

    /* 1. Create device-local image via VMA */
    NvAllocImage *image = nv_alloc_image_create(
        alloc, width, height, mip_levels,
        VK_FORMAT_R8G8B8A8_SRGB, usage);
    if (!image) {
        return NULL;
    }

    /* 2. Create host-visible staging buffer */
    VkDeviceSize pixel_bytes = (VkDeviceSize)width * height * 4;
    VkBuffer        staging_buf   = VK_NULL_HANDLE;
    NvVmaAllocation staging_alloc = NULL;

    if (!nv_vma_create_buffer(alloc->vma, pixel_bytes,
                               VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                               0, &staging_buf, &staging_alloc)) {
        nv_alloc_image_destroy(image);
        return NULL;
    }

    /* 3. Map, copy pixels, unmap */
    void *mapped = nv_vma_map(alloc->vma, staging_alloc);
    if (!mapped) {
        nv_vma_destroy_buffer(alloc->vma, staging_buf, staging_alloc);
        nv_alloc_image_destroy(image);
        return NULL;
    }
    memcpy(mapped, rgba_pixels, (size_t)pixel_bytes);
    nv_vma_unmap(alloc->vma, staging_alloc);

    /* 4. Record upload + mipmap commands */
    {
        VkCommandBuffer cmd = begin_single_command(
            alloc->device, dev->command_pool);

        /* Transition entire image to TRANSFER_DST */
        transition_image_layout(cmd, image->handle,
                                 VK_IMAGE_LAYOUT_UNDEFINED,
                                 VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                 mip_levels);

        /* Copy staging buffer to mip level 0 */
        VkBufferImageCopy region;
        memset(&region, 0, sizeof(region));
        region.imageSubresource.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
        region.imageSubresource.mipLevel       = 0;
        region.imageSubresource.baseArrayLayer = 0;
        region.imageSubresource.layerCount     = 1;
        region.imageExtent.width               = width;
        region.imageExtent.height              = height;
        region.imageExtent.depth               = 1;

        vkCmdCopyBufferToImage(cmd, staging_buf, image->handle,
                                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                                1, &region);

        if (generate_mipmaps && mip_levels > 1) {
            gen_mipmaps(cmd, image->handle,
                         width, height, mip_levels);
        } else {
            transition_image_layout(
                cmd, image->handle,
                VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                mip_levels);
        }

        end_single_command(alloc->device, dev->command_pool,
                            dev->graphics_queue, cmd);
    }

    /* 5. Clean up staging */
    nv_vma_destroy_buffer(alloc->vma, staging_buf, staging_alloc);

    /* 6. Create image view */
    VkImageView view = create_view(
        alloc->device, image->handle,
        VK_FORMAT_R8G8B8A8_SRGB, mip_levels);
    if (view == VK_NULL_HANDLE) {
        nv_alloc_image_destroy(image);
        return NULL;
    }

    /* 7. Create sampler */
    VkSampler sampler = create_tex_sampler(
        alloc->device, mip_levels, filter, address_mode);
    if (sampler == VK_NULL_HANDLE) {
        vkDestroyImageView(alloc->device, view, NULL);
        nv_alloc_image_destroy(image);
        return NULL;
    }

    /* 8. Build result */
    NvTexture *tex = calloc(1, sizeof(NvTexture));
    if (!tex) {
        vkDestroySampler(alloc->device, sampler, NULL);
        vkDestroyImageView(alloc->device, view, NULL);
        nv_alloc_image_destroy(image);
        return NULL;
    }
    tex->image      = image;
    tex->view       = view;
    tex->sampler    = sampler;
    tex->device     = alloc->device;
    tex->width      = width;
    tex->height     = height;
    tex->mip_levels = mip_levels;

    return tex;
}

NvTexture *nv_texture_create_from_file(NvDevice *dev,
                                        NvAllocator *alloc,
                                        const char *path,
                                        int generate_mipmaps,
                                        int filter,
                                        int address_mode) {
    if (!dev || !alloc || !path) {
        return NULL;
    }

    int w = 0;
    int h = 0;
    int channels = 0;
    stbi_uc *pixels = stbi_load(path, &w, &h, &channels, 4);
    if (!pixels) {
        fprintf(stderr, "[nova] failed to load texture: %s\n", path);
        return NULL;
    }

    NvTexture *tex = nv_texture_create(
        dev, alloc, (uint32_t)w, (uint32_t)h, pixels,
        generate_mipmaps, filter, address_mode);

    stbi_image_free(pixels);
    return tex;
}

void nv_texture_destroy(NvTexture *tex) {
    if (!tex) {
        return;
    }
    if (tex->sampler != VK_NULL_HANDLE) {
        vkDestroySampler(tex->device, tex->sampler, NULL);
    }
    if (tex->view != VK_NULL_HANDLE) {
        vkDestroyImageView(tex->device, tex->view, NULL);
    }
    nv_alloc_image_destroy(tex->image);
    free(tex);
}

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

VkImageView nv_texture_view(const NvTexture *tex) {
    return tex ? tex->view : VK_NULL_HANDLE;
}

VkSampler nv_texture_sampler(const NvTexture *tex) {
    return tex ? tex->sampler : VK_NULL_HANDLE;
}

uint32_t nv_texture_width(const NvTexture *tex) {
    return tex ? tex->width : 0;
}

uint32_t nv_texture_height(const NvTexture *tex) {
    return tex ? tex->height : 0;
}

uint32_t nv_texture_mip_levels(const NvTexture *tex) {
    return tex ? tex->mip_levels : 0;
}
