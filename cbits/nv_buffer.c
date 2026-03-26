/*
 * nv_buffer — Vulkan vertex and index buffer upload.
 *
 * Now backed by VMA via nv_allocator.  The staged upload pattern
 * delegates to nv_alloc_buffer_staged.
 */

#include "nv_buffer.h"

#include <stdlib.h>

/* ----------------------------------------------------------------
 * Internal
 * ---------------------------------------------------------------- */

static NvBuffer *create_from_alloc_buffer(NvAllocBuffer *ab) {
    if (!ab) {
        return NULL;
    }

    NvBuffer *buf = calloc(1, sizeof(NvBuffer));
    if (!buf) {
        nv_alloc_buffer_destroy(ab);
        return NULL;
    }
    buf->handle     = ab->handle;
    buf->allocation = ab->allocation;
    buf->vma        = ab->vma;
    buf->size       = ab->size;

    /* Free the NvAllocBuffer wrapper but NOT the Vulkan resources.
     * We transferred ownership to NvBuffer. */
    free(ab);
    return buf;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvBuffer *nv_buffer_create_vertex(NvDevice *dev, NvAllocator *alloc,
                                  const void *data,
                                  uint32_t byte_size) {
    NvAllocBuffer *ab = nv_alloc_buffer_staged(
        alloc, dev, VK_BUFFER_USAGE_VERTEX_BUFFER_BIT, data,
        byte_size);
    return create_from_alloc_buffer(ab);
}

NvBuffer *nv_buffer_create_index(NvDevice *dev, NvAllocator *alloc,
                                 const void *data,
                                 uint32_t byte_size) {
    NvAllocBuffer *ab = nv_alloc_buffer_staged(
        alloc, dev, VK_BUFFER_USAGE_INDEX_BUFFER_BIT, data,
        byte_size);
    return create_from_alloc_buffer(ab);
}

void nv_buffer_destroy(NvBuffer *buf) {
    if (!buf) {
        return;
    }
    nv_vma_destroy_buffer(buf->vma, buf->handle, buf->allocation);
    free(buf);
}
