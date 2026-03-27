/*
 * nv_terrain — GPU terrain pipeline with heightmap displacement
 * and splatmap material blending.
 *
 * Uses the standard 64-byte vertex layout (a flat grid patch).
 * The vertex shader displaces Y by sampling the heightmap.
 * The fragment shader blends 4 material layers via splatmap.
 *
 * Haskell sees NvTerrain* as an opaque Ptr ().
 */

#ifndef NV_TERRAIN_H
#define NV_TERRAIN_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"
#include "nv_pipeline.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvTerrain {
    VkDevice              device;
    VkPipelineLayout      layout;
    VkPipeline            pipeline;
    VkDescriptorSetLayout terrain_set_layout;
} NvTerrain;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a terrain pipeline.
 * render_pass: from the base PBR pipeline or post-process HDR pass.
 * set_layouts / set_layout_count: descriptor sets before the
 *   terrain set (e.g. set 0 = FrameUBO).  The terrain set
 *   (heightmap + splatmap + 4 layers) is appended as the next set.
 * shadow_set_layout: descriptor set layout for the shadow map
 *   (appended after the terrain set).
 * Returns NULL on failure. */
NvTerrain *nv_terrain_create(NvDevice *dev,
                              VkRenderPass render_pass,
                              const char *vert_path,
                              const char *frag_path,
                              const VkDescriptorSetLayout *set_layouts,
                              uint32_t set_layout_count,
                              VkDescriptorSetLayout shadow_set_layout);

/* Destroy the terrain pipeline. */
void nv_terrain_destroy(NvTerrain *terr);

/* Get the terrain descriptor set layout (for allocating sets). */
VkDescriptorSetLayout nv_terrain_set_layout(const NvTerrain *terr);

#endif /* NV_TERRAIN_H */
