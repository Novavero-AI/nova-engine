/*
 * nv_util — Shared Vulkan utility functions.
 *
 * SPIR-V shader loading, one-shot command buffers.
 * Used by all pipeline and resource modules.
 */

#ifndef NV_UTIL_H
#define NV_UTIL_H

#include <vulkan/vulkan.h>

/* ----------------------------------------------------------------
 * Shader loading
 * ---------------------------------------------------------------- */

/* Load a SPIR-V binary from disk and create a VkShaderModule.
 * Validates that the file size is > 0 and a multiple of 4.
 * Returns VK_NULL_HANDLE on failure. */
VkShaderModule nv_load_shader(VkDevice device, const char *path);

/* ----------------------------------------------------------------
 * One-shot command buffers
 * ---------------------------------------------------------------- */

/* Allocate and begin a one-time-submit command buffer. */
VkCommandBuffer nv_begin_single_command(VkDevice device,
                                        VkCommandPool pool);

/* End, submit, wait, and free a one-shot command buffer. */
void nv_end_single_command(VkDevice device, VkCommandPool pool,
                           VkQueue queue, VkCommandBuffer cmd);

#endif /* NV_UTIL_H */
