/*
 * nv_postprocess — HDR framebuffer, bloom, ACES tonemap, FXAA.
 *
 * Manages the full post-processing chain: an RGBA16F HDR color
 * target, a 5-level bloom mip chain (downsample + upsample), and
 * a final tonemap+FXAA pass to the swapchain.
 *
 * Haskell sees NvPostProcess* as an opaque Ptr ().
 */

#ifndef NV_POSTPROCESS_H
#define NV_POSTPROCESS_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"
#include "nv_frame.h"
#include "nv_pipeline.h"
#include "nv_swapchain.h"
#include "nv_vma.h"

#define NV_BLOOM_MIP_COUNT 5

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvPostProcess {
    VkDevice device;

    /* HDR color target (RGBA16F, full resolution) */
    VkImage          hdr_image;
    NvVmaAllocation  hdr_allocation;
    NvVmaAllocator   vma;
    VkImageView      hdr_view;
    VkSampler        hdr_sampler;

    /* HDR render pass + framebuffer (PBR scene draws here) */
    VkRenderPass     hdr_render_pass;
    VkFramebuffer    hdr_framebuffer;

    /* Bloom mip chain (half-res base, 5 mip levels) */
    VkImage          bloom_image;
    NvVmaAllocation  bloom_allocation;
    VkImageView      bloom_mip_views[NV_BLOOM_MIP_COUNT];
    VkSampler        bloom_sampler;

    /* Bloom render passes */
    VkRenderPass     bloom_down_pass;   /* loadOp DONT_CARE */
    VkRenderPass     bloom_up_pass;     /* loadOp LOAD (additive) */

    /* Bloom framebuffers (one per mip, shared for down/up) */
    VkFramebuffer    bloom_fbs[NV_BLOOM_MIP_COUNT];

    /* Bloom pipelines */
    VkDescriptorSetLayout bloom_set_layout;
    VkPipelineLayout      bloom_pipe_layout;
    VkPipeline            bloom_down_pipeline;
    VkPipeline            bloom_up_pipeline;

    /* Bloom descriptors */
    VkDescriptorPool bloom_desc_pool;
    VkDescriptorSet  bloom_down_sets[NV_BLOOM_MIP_COUNT];
    VkDescriptorSet  bloom_up_sets[NV_BLOOM_MIP_COUNT - 1];

    /* Tonemap + FXAA pipeline (renders to swapchain) */
    VkDescriptorSetLayout tonemap_set_layout;
    VkPipelineLayout      tonemap_pipe_layout;
    VkPipeline            tonemap_pipeline;
    VkRenderPass          tonemap_render_pass;
    VkFramebuffer        *tonemap_framebuffers;
    uint32_t              tonemap_fb_count;
    VkDescriptorPool      tonemap_desc_pool;
    VkDescriptorSet       tonemap_desc_set;

    /* Cached dimensions */
    uint32_t         width;
    uint32_t         height;
    VkFormat         hdr_format;
    VkFormat         depth_format;
    VkFormat         swapchain_format;
} NvPostProcess;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create post-processing resources.
 * Shader paths: fullscreen.vert.spv, bloom_downsample.frag.spv,
 *               bloom_upsample.frag.spv, tonemap.frag.spv.
 * Returns NULL on failure. */
NvPostProcess *nv_postprocess_create(NvDevice *dev,
                                      NvAllocator *alloc,
                                      NvSwapchain *sc,
                                      const char *fullscreen_vert,
                                      const char *bloom_down_frag,
                                      const char *bloom_up_frag,
                                      const char *tonemap_frag);

/* Destroy all post-processing resources. */
void nv_postprocess_destroy(NvPostProcess *pp);

/* Recreate size-dependent resources after swapchain resize.
 * Returns 1 on success, 0 on failure. */
int nv_postprocess_recreate(NvPostProcess *pp, NvSwapchain *sc);

/* ----------------------------------------------------------------
 * Pass management
 * ---------------------------------------------------------------- */

/* Begin the HDR render pass (PBR scene draws into this).
 * Binds the given PBR pipeline and sets viewport/scissor. */
void nv_postprocess_begin_hdr_pass(NvPostProcess *pp, NvFrame *fr,
                                    NvSwapchain *sc, NvPipeline *pip);

/* End the HDR render pass. */
void nv_postprocess_end_hdr_pass(NvFrame *fr);

/* Record the full post-processing chain:
 * bloom downsample (5) -> bloom upsample (4) -> tonemap+FXAA.
 * Call after ending the HDR pass, before nv_frame_submit. */
void nv_postprocess_record(NvPostProcess *pp, NvFrame *fr,
                            NvSwapchain *sc);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

/* HDR render pass (for PBR pipeline creation). */
VkRenderPass nv_postprocess_hdr_render_pass(const NvPostProcess *pp);

#endif /* NV_POSTPROCESS_H */
