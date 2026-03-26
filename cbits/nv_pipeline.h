/*
 * nv_pipeline — Vulkan render pass, graphics pipeline, framebuffers.
 *
 * Creates a render pass with color + depth attachments, a graphics
 * pipeline matching the 64-byte Vertex layout (position, normal,
 * UV, tangent, color), dynamic viewport/scissor, push-constant
 * MVP matrix, and per-swapchain-image framebuffers.
 *
 * Haskell sees NvPipeline* as an opaque Ptr ().
 */

#ifndef NV_PIPELINE_H
#define NV_PIPELINE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"
#include "nv_swapchain.h"

/* Push constant size: one mat4 (MVP). */
#define NV_PUSH_CONSTANT_SIZE 64

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvPipeline {
    VkDevice         device;
    VkRenderPass     render_pass;
    VkPipelineLayout layout;
    VkPipeline       handle;
    VkFramebuffer   *framebuffers;
    uint32_t         framebuffer_count;
} NvPipeline;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create render pass, pipeline, and framebuffers.
 * vert_path / frag_path are paths to compiled SPIR-V files.
 * Returns NULL on failure. */
NvPipeline *nv_pipeline_create(NvDevice *dev, NvSwapchain *sc,
                               const char *vert_path,
                               const char *frag_path);

/* Destroy framebuffers, pipeline, layout, render pass, and free. */
void nv_pipeline_destroy(NvPipeline *pip);

/* Recreate framebuffers after a swapchain resize.
 * The render pass and pipeline are unchanged (dynamic viewport).
 * Returns 1 on success, 0 on failure. */
int nv_pipeline_recreate_framebuffers(NvPipeline *pip,
                                      NvSwapchain *sc);

#endif /* NV_PIPELINE_H */
