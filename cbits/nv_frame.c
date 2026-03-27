/*
 * nv_frame — Per-frame synchronisation, command recording, present.
 */

#include "nv_frame.h"

#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

NvFrame *nv_frame_create(NvDevice *dev) {
    if (!dev) {
        return NULL;
    }

    NvFrame *fr = calloc(1, sizeof(NvFrame));
    if (!fr) {
        return NULL;
    }

    fr->device         = dev->handle;
    fr->graphics_queue = dev->graphics_queue;
    fr->present_queue  = dev->present_queue;

    /* Command buffers */
    VkCommandBufferAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType       = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    alloc_info.commandPool = dev->command_pool;
    alloc_info.level       = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    alloc_info.commandBufferCount = NV_MAX_FRAMES_IN_FLIGHT;

    if (vkAllocateCommandBuffers(fr->device, &alloc_info, fr->cmd)
        != VK_SUCCESS) {
        goto fail;
    }

    /* Sync objects */
    VkSemaphoreCreateInfo sem_info;
    memset(&sem_info, 0, sizeof(sem_info));
    sem_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;

    VkFenceCreateInfo fence_info;
    memset(&fence_info, 0, sizeof(fence_info));
    fence_info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fence_info.flags = VK_FENCE_CREATE_SIGNALED_BIT; /* pre-signaled */

    for (uint32_t i = 0; i < NV_MAX_FRAMES_IN_FLIGHT; i++) {
        if (vkCreateSemaphore(fr->device, &sem_info, NULL,
                              &fr->image_available[i])
                != VK_SUCCESS
            || vkCreateSemaphore(fr->device, &sem_info, NULL,
                                 &fr->render_finished[i])
                   != VK_SUCCESS
            || vkCreateFence(fr->device, &fence_info, NULL,
                             &fr->in_flight[i])
                   != VK_SUCCESS) {
            goto fail;
        }
    }

    return fr;

fail:
    nv_frame_destroy(fr);
    return NULL;
}

void nv_frame_destroy(NvFrame *fr) {
    if (!fr) {
        return;
    }
    for (uint32_t i = 0; i < NV_MAX_FRAMES_IN_FLIGHT; i++) {
        if (fr->in_flight[i] != VK_NULL_HANDLE) {
            vkDestroyFence(fr->device, fr->in_flight[i], NULL);
        }
        if (fr->render_finished[i] != VK_NULL_HANDLE) {
            vkDestroySemaphore(
                fr->device, fr->render_finished[i], NULL);
        }
        if (fr->image_available[i] != VK_NULL_HANDLE) {
            vkDestroySemaphore(
                fr->device, fr->image_available[i], NULL);
        }
    }
    /* Command buffers freed when command pool is destroyed. */
    free(fr);
}

/* ----------------------------------------------------------------
 * Frame bracket
 * ---------------------------------------------------------------- */

int nv_frame_acquire(NvFrame *fr, NvSwapchain *sc) {
    if (!fr || !sc) {
        return -1;
    }

    uint32_t f = fr->current_frame;

    /* Wait for this frame's previous submission to complete. */
    vkWaitForFences(fr->device, 1, &fr->in_flight[f], VK_TRUE,
                    UINT64_MAX);

    /* Acquire the next swapchain image. */
    VkResult acq = vkAcquireNextImageKHR(
        fr->device, sc->handle, UINT64_MAX,
        fr->image_available[f], VK_NULL_HANDLE, &fr->image_index);

    if (acq == VK_ERROR_OUT_OF_DATE_KHR) {
        return 0;
    }
    if (acq != VK_SUCCESS && acq != VK_SUBOPTIMAL_KHR) {
        return -1;
    }

    vkResetFences(fr->device, 1, &fr->in_flight[f]);

    /* Begin command recording */
    VkCommandBuffer cmd = fr->cmd[f];
    vkResetCommandBuffer(cmd, 0);

    VkCommandBufferBeginInfo begin;
    memset(&begin, 0, sizeof(begin));
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    vkBeginCommandBuffer(cmd, &begin);

    return 1;
}

void nv_frame_begin_render_pass(NvFrame *fr, NvSwapchain *sc,
                                NvPipeline *pip) {
    VkCommandBuffer cmd = fr->cmd[fr->current_frame];

    VkClearValue clears[2];
    memset(clears, 0, sizeof(clears));
    clears[0].color.float32[0] = 0.01f;
    clears[0].color.float32[1] = 0.01f;
    clears[0].color.float32[2] = 0.02f;
    clears[0].color.float32[3] = 1.0f;
    clears[1].depthStencil.depth   = 1.0f;
    clears[1].depthStencil.stencil = 0;

    VkRenderPassBeginInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType             = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    rp.renderPass        = pip->render_pass;
    rp.framebuffer       = pip->framebuffers[fr->image_index];
    rp.renderArea.extent = sc->extent;
    rp.clearValueCount   = 2;
    rp.pClearValues      = clears;

    vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);

    /* Bind pipeline + dynamic state */
    vkCmdBindPipeline(
        cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, pip->handle);

    VkViewport vp;
    memset(&vp, 0, sizeof(vp));
    vp.width    = (float)sc->extent.width;
    vp.height   = (float)sc->extent.height;
    vp.maxDepth = 1.0f;
    vkCmdSetViewport(cmd, 0, 1, &vp);

    VkRect2D scissor;
    memset(&scissor, 0, sizeof(scissor));
    scissor.extent = sc->extent;
    vkCmdSetScissor(cmd, 0, 1, &scissor);
}

int nv_frame_begin(NvFrame *fr, NvSwapchain *sc, NvPipeline *pip) {
    if (!fr || !sc || !pip) {
        return -1;
    }

    int result = nv_frame_acquire(fr, sc);
    if (result != 1) {
        return result;
    }

    nv_frame_begin_render_pass(fr, sc, pip);
    return 1;
}

int nv_frame_end(NvFrame *fr, NvSwapchain *sc) {
    if (!fr || !sc) {
        return -1;
    }

    uint32_t        f   = fr->current_frame;
    VkCommandBuffer cmd = fr->cmd[f];

    vkCmdEndRenderPass(cmd);
    if (vkEndCommandBuffer(cmd) != VK_SUCCESS) {
        return -1;
    }

    /* Submit */
    VkSemaphore wait_sems[]   = {fr->image_available[f]};
    VkSemaphore signal_sems[] = {fr->render_finished[f]};
    VkPipelineStageFlags wait_stages[] = {
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};

    VkSubmitInfo submit;
    memset(&submit, 0, sizeof(submit));
    submit.sType                = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.waitSemaphoreCount   = 1;
    submit.pWaitSemaphores      = wait_sems;
    submit.pWaitDstStageMask    = wait_stages;
    submit.commandBufferCount   = 1;
    submit.pCommandBuffers      = &cmd;
    submit.signalSemaphoreCount = 1;
    submit.pSignalSemaphores    = signal_sems;

    if (vkQueueSubmit(fr->graphics_queue, 1, &submit,
                      fr->in_flight[f])
        != VK_SUCCESS) {
        return -1;
    }

    /* Present */
    VkPresentInfoKHR present;
    memset(&present, 0, sizeof(present));
    present.sType              = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    present.waitSemaphoreCount = 1;
    present.pWaitSemaphores    = signal_sems;
    present.swapchainCount     = 1;
    present.pSwapchains        = &sc->handle;
    present.pImageIndices      = &fr->image_index;

    VkResult res = vkQueuePresentKHR(fr->present_queue, &present);

    fr->current_frame =
        (fr->current_frame + 1) % NV_MAX_FRAMES_IN_FLIGHT;

    if (res == VK_ERROR_OUT_OF_DATE_KHR
        || res == VK_SUBOPTIMAL_KHR) {
        return 0;
    }
    if (res != VK_SUCCESS) {
        return -1;
    }
    return 1;
}

int nv_frame_submit(NvFrame *fr, NvSwapchain *sc) {
    if (!fr || !sc) {
        return -1;
    }

    uint32_t        f   = fr->current_frame;
    VkCommandBuffer cmd = fr->cmd[f];

    if (vkEndCommandBuffer(cmd) != VK_SUCCESS) {
        return -1;
    }

    VkSemaphore wait_sems[]   = {fr->image_available[f]};
    VkSemaphore signal_sems[] = {fr->render_finished[f]};
    VkPipelineStageFlags wait_stages[] = {
        VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};

    VkSubmitInfo submit;
    memset(&submit, 0, sizeof(submit));
    submit.sType                = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.waitSemaphoreCount   = 1;
    submit.pWaitSemaphores      = wait_sems;
    submit.pWaitDstStageMask    = wait_stages;
    submit.commandBufferCount   = 1;
    submit.pCommandBuffers      = &cmd;
    submit.signalSemaphoreCount = 1;
    submit.pSignalSemaphores    = signal_sems;

    if (vkQueueSubmit(fr->graphics_queue, 1, &submit,
                      fr->in_flight[f])
        != VK_SUCCESS) {
        return -1;
    }

    VkPresentInfoKHR present;
    memset(&present, 0, sizeof(present));
    present.sType              = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    present.waitSemaphoreCount = 1;
    present.pWaitSemaphores    = signal_sems;
    present.swapchainCount     = 1;
    present.pSwapchains        = &sc->handle;
    present.pImageIndices      = &fr->image_index;

    VkResult res = vkQueuePresentKHR(fr->present_queue, &present);

    fr->current_frame =
        (fr->current_frame + 1) % NV_MAX_FRAMES_IN_FLIGHT;

    if (res == VK_ERROR_OUT_OF_DATE_KHR
        || res == VK_SUBOPTIMAL_KHR) {
        return 0;
    }
    if (res != VK_SUCCESS) {
        return -1;
    }
    return 1;
}

/* ----------------------------------------------------------------
 * Draw commands
 * ---------------------------------------------------------------- */

void nv_frame_push_constants(NvFrame *fr, NvPipeline *pip,
                             const void *data, uint32_t offset,
                             uint32_t size) {
    vkCmdPushConstants(fr->cmd[fr->current_frame], pip->layout,
                       VK_SHADER_STAGE_VERTEX_BIT
                           | VK_SHADER_STAGE_FRAGMENT_BIT,
                       offset, size, data);
}

void nv_frame_bind_vertex_buffer(NvFrame *fr, NvBuffer *buf) {
    VkBuffer     buffers[] = {buf->handle};
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(
        fr->cmd[fr->current_frame], 0, 1, buffers, offsets);
}

void nv_frame_bind_index_buffer(NvFrame *fr, NvBuffer *buf) {
    vkCmdBindIndexBuffer(fr->cmd[fr->current_frame], buf->handle,
                         0, VK_INDEX_TYPE_UINT32);
}

void nv_frame_draw_indexed(NvFrame *fr, uint32_t index_count) {
    vkCmdDrawIndexed(
        fr->cmd[fr->current_frame], index_count, 1, 0, 0, 0);
}

void nv_frame_draw(NvFrame *fr, uint32_t vertex_count) {
    vkCmdDraw(fr->cmd[fr->current_frame], vertex_count, 1, 0, 0);
}

void nv_frame_bind_descriptor_set(NvFrame *fr, NvPipeline *pip,
                                   uint32_t set_index,
                                   uint64_t descriptor_set) {
    VkDescriptorSet set = (VkDescriptorSet)(uintptr_t)descriptor_set;
    vkCmdBindDescriptorSets(
        fr->cmd[fr->current_frame],
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        pip->layout, set_index, 1, &set, 0, NULL);
}
