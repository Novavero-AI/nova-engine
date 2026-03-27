/*
 * nv_skin_pipeline — Graphics pipeline for GPU-skinned meshes.
 *
 * 80-byte vertex layout (no color, adds bone indices + weights).
 * Bone matrices are read from an SSBO at set 1, binding 0.
 * Reuses the render pass from the base (unskinned) pipeline.
 *
 * Haskell sees NvSkinPipeline* as an opaque Ptr ().
 */

#ifndef NV_SKIN_PIPELINE_H
#define NV_SKIN_PIPELINE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"
#include "nv_pipeline.h"

#define NV_MAX_BONES     128
#define NV_BONE_SSBO_SIZE (NV_MAX_BONES * 64)

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvSkinPipeline {
    VkDevice              device;
    VkPipelineLayout      layout;
    VkPipeline            pipeline;
    VkDescriptorSetLayout bone_set_layout;
} NvSkinPipeline;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a skinned graphics pipeline.
 * render_pass: the render pass to use (from base PBR pipeline or
 *              post-process HDR pass).
 * set_layouts / set_layout_count: descriptor set layouts for sets
 *   BEFORE the bone set (e.g. set 0 = FrameUBO + textures).
 *   The bone SSBO is appended as the next set automatically.
 * Returns NULL on failure. */
NvSkinPipeline *nv_skin_pipeline_create(NvDevice *dev,
                                         VkRenderPass render_pass,
                                         const char *vert_path,
                                         const char *frag_path,
                                         const VkDescriptorSetLayout *set_layouts,
                                         uint32_t set_layout_count);

/* Destroy the skinned pipeline. */
void nv_skin_pipeline_destroy(NvSkinPipeline *pip);

/* Get the VkDescriptorSetLayout for the bone SSBO set
 * (for allocating descriptor sets). */
VkDescriptorSetLayout nv_skin_pipeline_bone_layout(
    const NvSkinPipeline *pip);

#endif /* NV_SKIN_PIPELINE_H */
