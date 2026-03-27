/*
 * nv_frame — Per-frame synchronisation, command recording, present.
 *
 * Manages 2 frames in flight with semaphores and fences.
 * Provides begin/end bracketing with draw commands in between.
 *
 *   nv_frame_begin   →  acquire image, begin render pass
 *   nv_frame_*       →  push constants, bind buffers, draw
 *   nv_frame_end     →  end render pass, submit, present
 *
 * Returns 1 on success, 0 when the swapchain needs recreation,
 * -1 on fatal error.
 *
 * Haskell sees NvFrame* as an opaque Ptr ().
 */

#ifndef NV_FRAME_H
#define NV_FRAME_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_buffer.h"
#include "nv_device.h"
#include "nv_pipeline.h"
#include "nv_swapchain.h"

#define NV_MAX_FRAMES_IN_FLIGHT 2

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvFrame {
    VkDevice        device;
    VkQueue         graphics_queue;
    VkQueue         present_queue;
    VkCommandBuffer cmd[NV_MAX_FRAMES_IN_FLIGHT];
    VkSemaphore     image_available[NV_MAX_FRAMES_IN_FLIGHT];
    VkSemaphore     render_finished[NV_MAX_FRAMES_IN_FLIGHT];
    VkFence         in_flight[NV_MAX_FRAMES_IN_FLIGHT];
    uint32_t        current_frame;
    uint32_t        image_index;
} NvFrame;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Allocate command buffers, semaphores, and fences.
 * Returns NULL on failure. */
NvFrame *nv_frame_create(NvDevice *dev);

/* Destroy sync objects, free command buffers, free struct. */
void nv_frame_destroy(NvFrame *fr);

/* ----------------------------------------------------------------
 * Frame bracket
 * ---------------------------------------------------------------- */

/* Begin a frame: wait fence, acquire image, begin render pass.
 * Returns 1 OK, 0 swapchain out-of-date, -1 error. */
int nv_frame_begin(NvFrame *fr, NvSwapchain *sc, NvPipeline *pip);

/* Acquire next image and begin command recording (no render pass).
 * Use when recording shadow passes before the main render pass.
 * Returns 1 OK, 0 swapchain out-of-date, -1 error. */
int nv_frame_acquire(NvFrame *fr, NvSwapchain *sc);

/* Begin the main render pass after shadow pass recording.
 * Call after nv_frame_acquire + shadow recording. */
void nv_frame_begin_render_pass(NvFrame *fr, NvSwapchain *sc,
                                NvPipeline *pip);

/* End a frame: end render pass, submit, present.
 * Returns 1 OK, 0 swapchain out-of-date, -1 error. */
int nv_frame_end(NvFrame *fr, NvSwapchain *sc);

/* Submit and present without ending any render pass.
 * Use when the last render pass was already ended (e.g. by
 * nv_postprocess_record).
 * Returns 1 OK, 0 swapchain out-of-date, -1 error. */
int nv_frame_submit(NvFrame *fr, NvSwapchain *sc);

/* ----------------------------------------------------------------
 * Draw commands (call between begin and end)
 * ---------------------------------------------------------------- */

/* Push constant data at the given byte offset. */
void nv_frame_push_constants(NvFrame *fr, NvPipeline *pip,
                             const void *data, uint32_t offset,
                             uint32_t size);

/* Bind a vertex buffer at binding 0. */
void nv_frame_bind_vertex_buffer(NvFrame *fr, NvBuffer *buf);

/* Bind an index buffer (uint32 indices). */
void nv_frame_bind_index_buffer(NvFrame *fr, NvBuffer *buf);

/* Issue an indexed draw call. */
void nv_frame_draw_indexed(NvFrame *fr, uint32_t index_count);

/* Issue a non-indexed draw call. */
void nv_frame_draw(NvFrame *fr, uint32_t vertex_count);

/* Bind a descriptor set at the given set index. */
void nv_frame_bind_descriptor_set(NvFrame *fr, NvPipeline *pip,
                                   uint32_t set_index,
                                   uint64_t descriptor_set);

#endif /* NV_FRAME_H */
