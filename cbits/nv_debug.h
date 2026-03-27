/*
 * nv_debug — Debug line rendering and GPU timestamp profiling.
 *
 * Immediate-mode debug line API: accumulate line segments each
 * frame, flush to a dynamic vertex buffer, draw in a single call.
 * Also provides GPU timestamp query support for frame profiling.
 *
 * Haskell sees NvDebug* as an opaque Ptr ().
 */

#ifndef NV_DEBUG_H
#define NV_DEBUG_H

#include <stdint.h>

#include <vulkan/vulkan.h>

#include "nv_allocator.h"
#include "nv_device.h"
#include "nv_frame.h"
#include "nv_pipeline.h"

#define NV_DEBUG_MAX_LINES     8192
#define NV_DEBUG_VERTS_PER_LINE 2
#define NV_DEBUG_BYTES_PER_VERT 28  /* V3 pos (12) + V4 color (16) */
#define NV_DEBUG_BUFFER_SIZE \
    (NV_DEBUG_MAX_LINES * NV_DEBUG_VERTS_PER_LINE * NV_DEBUG_BYTES_PER_VERT)

#define NV_DEBUG_MAX_TIMESTAMPS 16

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvDebugVertex {
    float px, py, pz;
    float cr, cg, cb, ca;
} NvDebugVertex;

typedef struct NvDebug {
    VkDevice device;

    /* Line rendering */
    VkPipelineLayout line_layout;
    VkPipeline       line_pipeline;
    VkBuffer         line_buffer;
    NvVmaAllocation  line_allocation;
    NvVmaAllocator   vma;
    NvDebugVertex    line_verts[NV_DEBUG_MAX_LINES * NV_DEBUG_VERTS_PER_LINE];
    uint32_t         line_count;

    /* GPU timestamps */
    VkQueryPool      timestamp_pool;
    float            timestamp_period;  /* nanoseconds per tick */
    uint32_t         timestamp_count;
} NvDebug;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create debug resources.
 * render_pass: the render pass to draw lines into (typically the
 *   main or HDR pass).
 * Returns NULL on failure. */
NvDebug *nv_debug_create(NvDevice *dev, NvAllocator *alloc,
                          VkRenderPass render_pass,
                          const char *vert_path,
                          const char *frag_path);

/* Destroy all debug resources. */
void nv_debug_destroy(NvDebug *dbg);

/* ----------------------------------------------------------------
 * Line drawing (call between begin/end of a render pass)
 * ---------------------------------------------------------------- */

/* Add a line segment.  Color is RGBA [0,1]. */
void nv_debug_line(NvDebug *dbg,
                    float x0, float y0, float z0,
                    float x1, float y1, float z1,
                    float r, float g, float b, float a);

/* Add an axis-aligned box wireframe. */
void nv_debug_box(NvDebug *dbg,
                   float cx, float cy, float cz,
                   float hx, float hy, float hz,
                   float r, float g, float b, float a);

/* Add a sphere wireframe (3 circles). */
void nv_debug_sphere(NvDebug *dbg,
                      float cx, float cy, float cz, float radius,
                      int segments,
                      float r, float g, float b, float a);

/* Flush all accumulated lines: upload to GPU and draw.
 * Must be called inside an active render pass.
 * view_projection: the VP matrix (push constant). */
void nv_debug_flush(NvDebug *dbg, NvFrame *fr,
                     const float *view_projection);

/* ----------------------------------------------------------------
 * GPU timestamps
 * ---------------------------------------------------------------- */

/* Write a GPU timestamp at the current command position.
 * Returns the timestamp index (0..NV_DEBUG_MAX_TIMESTAMPS-1),
 * or -1 if full. */
int nv_debug_timestamp_write(NvDebug *dbg, NvFrame *fr);

/* Reset timestamps for a new frame.
 * Call at the start of each frame before writing timestamps. */
void nv_debug_timestamp_reset(NvDebug *dbg, NvFrame *fr);

/* Read back timestamp results in nanoseconds.
 * results must point to at least NV_DEBUG_MAX_TIMESTAMPS floats.
 * Returns the number of valid timestamps. */
int nv_debug_timestamp_read(NvDebug *dbg, float *results);

#endif /* NV_DEBUG_H */
