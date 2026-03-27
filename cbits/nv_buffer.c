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

NvBuffer *nv_buffer_create_host(NvAllocator *alloc,
                                uint32_t byte_size) {
    if (!alloc || byte_size == 0) {
        return NULL;
    }

    NvAllocBuffer *ab = nv_alloc_buffer_host(
        alloc, byte_size, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT);
    return create_from_alloc_buffer(ab);
}

void nv_buffer_destroy(NvBuffer *buf) {
    if (!buf) {
        return;
    }
    nv_vma_destroy_buffer(buf->vma, buf->handle, buf->allocation);
    free(buf);
}

void *nv_buffer_map(NvBuffer *buf) {
    if (!buf) {
        return NULL;
    }
    return nv_vma_map(buf->vma, buf->allocation);
}

void nv_buffer_unmap(NvBuffer *buf) {
    if (buf) {
        nv_vma_unmap(buf->vma, buf->allocation);
    }
}

VkBuffer nv_buffer_vk_handle(const NvBuffer *buf) {
    return buf ? buf->handle : VK_NULL_HANDLE;
}
