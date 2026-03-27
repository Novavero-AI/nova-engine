/*
 * nv_util — Shared Vulkan utility functions.
 */

#include "nv_util.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * Shader loading
 * ---------------------------------------------------------------- */

VkShaderModule nv_load_shader(VkDevice device, const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "[nova] failed to open shader: %s\n", path);
        return VK_NULL_HANDLE;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    /* SPIR-V requires codeSize > 0 and a multiple of 4. */
    if (size <= 0 || size % 4 != 0) {
        fprintf(stderr, "[nova] invalid SPIR-V size (%ld): %s\n",
                size, path);
        fclose(f);
        return VK_NULL_HANDLE;
    }

    /* SPIR-V requires uint32_t alignment — malloc guarantees it. */
    uint32_t *code = malloc((size_t)size);
    if (!code) {
        fclose(f);
        return VK_NULL_HANDLE;
    }
    if (fread(code, 1, (size_t)size, f) != (size_t)size) {
        free(code);
        fclose(f);
        return VK_NULL_HANDLE;
    }
    fclose(f);

    VkShaderModuleCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    info.codeSize = (size_t)size;
    info.pCode    = code;

    VkShaderModule mod = VK_NULL_HANDLE;
    vkCreateShaderModule(device, &info, NULL, &mod);
    free(code);
    return mod;
}

/* ----------------------------------------------------------------
 * One-shot command buffers
 * ---------------------------------------------------------------- */

VkCommandBuffer nv_begin_single_command(VkDevice device,
                                        VkCommandPool pool) {
    VkCommandBufferAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType       = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    alloc_info.commandPool = pool;
    alloc_info.level       = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    alloc_info.commandBufferCount = 1;

    VkCommandBuffer cmd = VK_NULL_HANDLE;
    vkAllocateCommandBuffers(device, &alloc_info, &cmd);

    VkCommandBufferBeginInfo begin;
    memset(&begin, 0, sizeof(begin));
    begin.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
    begin.flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
    vkBeginCommandBuffer(cmd, &begin);

    return cmd;
}

void nv_end_single_command(VkDevice device, VkCommandPool pool,
                           VkQueue queue, VkCommandBuffer cmd) {
    vkEndCommandBuffer(cmd);

    VkSubmitInfo submit;
    memset(&submit, 0, sizeof(submit));
    submit.sType              = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit.commandBufferCount = 1;
    submit.pCommandBuffers    = &cmd;

    vkQueueSubmit(queue, 1, &submit, VK_NULL_HANDLE);
    vkQueueWaitIdle(queue);
    vkFreeCommandBuffers(device, pool, 1, &cmd);
}
