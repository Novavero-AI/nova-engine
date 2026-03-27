/*
 * nv_compute — Vulkan compute pipeline.
 *
 * Manages a VkComputePipeline with a configurable descriptor set
 * layout and push constant range.  Used for morph target blending,
 * particle simulation, and any general-purpose GPU compute work.
 *
 * Haskell sees NvCompute* as an opaque Ptr ().
 */

#ifndef NV_COMPUTE_H
#define NV_COMPUTE_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_device.h"
#include "nv_frame.h"

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvCompute {
    VkDevice              device;
    VkPipelineLayout      layout;
    VkPipeline            pipeline;
    VkDescriptorSetLayout set_layout;
} NvCompute;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create a compute pipeline.
 * bindings: packed array of (binding, descriptorType, stageFlags,
 *           count) quads (4 uint32_t per binding).
 * binding_count: number of bindings.
 * push_size: push constant size in bytes (0 for none).
 * shader_path: path to compiled .comp.spv.
 * Returns NULL on failure. */
NvCompute *nv_compute_create(NvDevice *dev,
                              const uint32_t *bindings,
                              uint32_t binding_count,
                              uint32_t push_size,
                              const char *shader_path);

/* Destroy the compute pipeline. */
void nv_compute_destroy(NvCompute *comp);

/* ----------------------------------------------------------------
 * Dispatch
 * ---------------------------------------------------------------- */

/* Bind the compute pipeline. Call before dispatch. */
void nv_compute_bind(NvCompute *comp, NvFrame *fr);

/* Dispatch compute work groups. */
void nv_compute_dispatch(NvFrame *fr,
                          uint32_t group_x,
                          uint32_t group_y,
                          uint32_t group_z);

/* Push constants for the compute pipeline. */
void nv_compute_push_constants(NvCompute *comp, NvFrame *fr,
                                const void *data, uint32_t size);

/* Bind a descriptor set for the compute pipeline. */
void nv_compute_bind_descriptor_set(NvCompute *comp, NvFrame *fr,
                                     uint64_t descriptor_set);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

/* Get the descriptor set layout handle. */
VkDescriptorSetLayout nv_compute_set_layout(const NvCompute *comp);

#endif /* NV_COMPUTE_H */
