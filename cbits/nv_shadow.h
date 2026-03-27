/*
 * nv_shadow — Cascaded shadow map resources.
 *
 * Depth-only array texture (4 layers), depth-only render pass,
 * shadow pipeline with front-face culling and depth bias, and
 * per-cascade framebuffers.
 *
 * Haskell sees NvShadow* as an opaque Ptr ().
 */

#ifndef NV_SHADOW_H
#define NV_SHADOW_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"
#include "nv_frame.h"

#define NV_SHADOW_CASCADE_COUNT 4

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvShadow {
    VkDevice         device;

    /* Depth array image (4 layers) via VMA */
    VkImage          image;
    NvVmaAllocation  allocation;
    NvVmaAllocator   vma;
    VkImageView      array_view;
    VkImageView      layer_views[NV_SHADOW_CASCADE_COUNT];
    VkSampler        sampler;

    /* Depth-only render pass */
    VkRenderPass     render_pass;

    /* Per-cascade framebuffers */
    VkFramebuffer    framebuffers[NV_SHADOW_CASCADE_COUNT];

    /* Shadow pipeline (depth-only, no fragment shader) */
    VkPipelineLayout pipeline_layout;
    VkPipeline       pipeline;

    /* Dimensions */
    uint32_t         resolution;
    VkFormat         depth_format;
} NvShadow;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create shadow map resources.
 * depth_format: typically sc->depth_format from the swapchain.
 * vert_path: path to compiled shadow.vert.spv.
 * Returns NULL on failure. */
NvShadow *nv_shadow_create(NvDevice *dev, NvAllocator *alloc,
                            VkFormat depth_format, uint32_t resolution,
                            const char *vert_path);

/* Destroy all shadow resources. */
void nv_shadow_destroy(NvShadow *shadow);

/* ----------------------------------------------------------------
 * Per-cascade pass management
 * ---------------------------------------------------------------- */

/* Begin a shadow render pass for the given cascade (0..3).
 * Call between nv_frame_acquire and nv_frame_begin_render_pass. */
void nv_shadow_begin_pass(NvShadow *shadow, NvFrame *fr,
                          uint32_t cascade_index);

/* End the shadow render pass for the current cascade. */
void nv_shadow_end_pass(NvFrame *fr);

/* ----------------------------------------------------------------
 * Queries (for descriptor binding)
 * ---------------------------------------------------------------- */

/* Array image view (sampler2DArrayShadow in the shader). */
VkImageView nv_shadow_array_view(const NvShadow *shadow);

/* Comparison sampler for PCF. */
VkSampler nv_shadow_sampler(const NvShadow *shadow);

#endif /* NV_SHADOW_H */
