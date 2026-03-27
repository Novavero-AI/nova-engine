/*
 * nv_compute — Vulkan compute pipeline.
 */

#include "nv_compute.h"

#include <stdlib.h>
#include <string.h>

#include "nv_frame.h"
#include "nv_util.h"

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvCompute *nv_compute_create(NvDevice *dev,
                              const uint32_t *bindings,
                              uint32_t binding_count,
                              uint32_t push_size,
                              const char *shader_path) {
    if (!dev || !shader_path) return NULL;

    NvCompute *comp = calloc(1, sizeof(NvCompute));
    if (!comp) return NULL;
    comp->device = dev->handle;

    /* ---- Descriptor set layout ---- */
    VkDescriptorSetLayoutBinding *dsl_bindings = NULL;
    if (binding_count > 0 && bindings) {
        dsl_bindings = calloc(binding_count,
                               sizeof(VkDescriptorSetLayoutBinding));
        if (!dsl_bindings) goto fail;
        for (uint32_t i = 0; i < binding_count; i++) {
            dsl_bindings[i].binding         = bindings[i * 4 + 0];
            dsl_bindings[i].descriptorType  = bindings[i * 4 + 1];
            dsl_bindings[i].stageFlags      = bindings[i * 4 + 2];
            dsl_bindings[i].descriptorCount = bindings[i * 4 + 3];
        }
    }

    VkDescriptorSetLayoutCreateInfo dsl_info;
    memset(&dsl_info, 0, sizeof(dsl_info));
    dsl_info.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    dsl_info.bindingCount = binding_count;
    dsl_info.pBindings    = dsl_bindings;

    if (vkCreateDescriptorSetLayout(comp->device, &dsl_info, NULL,
                                     &comp->set_layout) != VK_SUCCESS) {
        free(dsl_bindings);
        goto fail;
    }
    free(dsl_bindings);

    /* ---- Pipeline layout ---- */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_COMPUTE_BIT;
    push.size       = push_size;

    VkPipelineLayoutCreateInfo pli;
    memset(&pli, 0, sizeof(pli));
    pli.sType          = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pli.setLayoutCount = 1;
    pli.pSetLayouts    = &comp->set_layout;
    if (push_size > 0) {
        pli.pushConstantRangeCount = 1;
        pli.pPushConstantRanges    = &push;
    }

    if (vkCreatePipelineLayout(comp->device, &pli, NULL,
                                &comp->layout) != VK_SUCCESS) {
        goto fail;
    }

    /* ---- Compute pipeline ---- */
    VkShaderModule shader = nv_load_shader(comp->device, shader_path);
    if (shader == VK_NULL_HANDLE) goto fail;

    VkPipelineShaderStageCreateInfo stage;
    memset(&stage, 0, sizeof(stage));
    stage.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stage.stage  = VK_SHADER_STAGE_COMPUTE_BIT;
    stage.module = shader;
    stage.pName  = "main";

    VkComputePipelineCreateInfo ci;
    memset(&ci, 0, sizeof(ci));
    ci.sType  = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
    ci.stage  = stage;
    ci.layout = comp->layout;

    VkResult result = vkCreateComputePipelines(
        comp->device, VK_NULL_HANDLE, 1, &ci, NULL, &comp->pipeline);

    vkDestroyShaderModule(comp->device, shader, NULL);
    if (result != VK_SUCCESS) goto fail;

    return comp;

fail:
    nv_compute_destroy(comp);
    return NULL;
}

void nv_compute_destroy(NvCompute *comp) {
    if (!comp) return;
    if (comp->pipeline != VK_NULL_HANDLE)
        vkDestroyPipeline(comp->device, comp->pipeline, NULL);
    if (comp->layout != VK_NULL_HANDLE)
        vkDestroyPipelineLayout(comp->device, comp->layout, NULL);
    if (comp->set_layout != VK_NULL_HANDLE)
        vkDestroyDescriptorSetLayout(comp->device,
                                      comp->set_layout, NULL);
    free(comp);
}

/* ----------------------------------------------------------------
 * Dispatch
 * ---------------------------------------------------------------- */

void nv_compute_bind(NvCompute *comp, NvFrame *fr) {
    vkCmdBindPipeline(fr->cmd[fr->current_frame],
                       VK_PIPELINE_BIND_POINT_COMPUTE,
                       comp->pipeline);
}

void nv_compute_dispatch(NvFrame *fr,
                          uint32_t group_x,
                          uint32_t group_y,
                          uint32_t group_z) {
    vkCmdDispatch(fr->cmd[fr->current_frame],
                   group_x, group_y, group_z);
}

void nv_compute_push_constants(NvCompute *comp, NvFrame *fr,
                                const void *data, uint32_t size) {
    vkCmdPushConstants(fr->cmd[fr->current_frame], comp->layout,
                       VK_SHADER_STAGE_COMPUTE_BIT, 0, size, data);
}

void nv_compute_bind_descriptor_set(NvCompute *comp, NvFrame *fr,
                                     uint64_t descriptor_set) {
    VkDescriptorSet set = (VkDescriptorSet)(uintptr_t)descriptor_set;
    vkCmdBindDescriptorSets(
        fr->cmd[fr->current_frame],
        VK_PIPELINE_BIND_POINT_COMPUTE,
        comp->layout, 0, 1, &set, 0, NULL);
}

VkDescriptorSetLayout nv_compute_set_layout(const NvCompute *comp) {
    return comp ? comp->set_layout : VK_NULL_HANDLE;
}
