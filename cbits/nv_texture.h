/*
 * nv_texture — Vulkan texture creation, mipmaps, and sampling.
 *
 * Loads RGBA pixel data (from memory or file via stb_image),
 * creates a VMA-backed VkImage, optionally generates a full mip
 * chain via vkCmdBlitImage, and pairs the image with a VkImageView
 * and VkSampler ready for descriptor binding.
 *
 * Haskell sees NvTexture* as an opaque Ptr ().
 */

#ifndef NV_TEXTURE_H
#define NV_TEXTURE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvTexture {
    NvAllocImage *image;
    VkImageView   view;
    VkSampler     sampler;
    VkDevice      device;
    uint32_t      width;
    uint32_t      height;
    uint32_t      mip_levels;
} NvTexture;

/* Filter modes */
#define NV_FILTER_NEAREST 0
#define NV_FILTER_LINEAR  1

/* Address modes */
#define NV_ADDRESS_REPEAT     0
#define NV_ADDRESS_CLAMP_EDGE 1
#define NV_ADDRESS_MIRROR     2

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a texture from raw RGBA8 pixel data.
 * rgba_pixels must contain width * height * 4 bytes.
 * generate_mipmaps: 1 to auto-generate a full mip chain.
 * filter: NV_FILTER_NEAREST or NV_FILTER_LINEAR.
 * address_mode: NV_ADDRESS_REPEAT, _CLAMP_EDGE, or _MIRROR.
 * Returns NULL on failure. */
NvTexture *nv_texture_create(NvDevice *dev, NvAllocator *alloc,
                              uint32_t width, uint32_t height,
                              const uint8_t *rgba_pixels,
                              int generate_mipmaps,
                              int filter,
                              int address_mode);

/* Create a texture by loading an image file via stb_image.
 * Supports PNG, JPG, BMP, TGA, HDR.
 * Returns NULL on failure (file not found, decode error, etc). */
NvTexture *nv_texture_create_from_file(NvDevice *dev,
                                        NvAllocator *alloc,
                                        const char *path,
                                        int generate_mipmaps,
                                        int filter,
                                        int address_mode);

/* Destroy texture resources (sampler, view, image). */
void nv_texture_destroy(NvTexture *tex);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

/* Get the image view (for descriptor writes). */
VkImageView nv_texture_view(const NvTexture *tex);

/* Get the sampler (for descriptor writes). */
VkSampler nv_texture_sampler(const NvTexture *tex);

/* Get texture dimensions and mip count. */
uint32_t nv_texture_width(const NvTexture *tex);
uint32_t nv_texture_height(const NvTexture *tex);
uint32_t nv_texture_mip_levels(const NvTexture *tex);

/* ----------------------------------------------------------------
 * Utilities
 * ---------------------------------------------------------------- */

/* Compute the number of mip levels for given dimensions.
 * floor(log2(max(w,h))) + 1. */
uint32_t nv_texture_calc_mip_levels(uint32_t width, uint32_t height);

#endif /* NV_TEXTURE_H */
