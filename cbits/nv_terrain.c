/*
 * nv_terrain — GPU terrain pipeline.
 */

#include "nv_terrain.h"

#include <stdlib.h>
#include <string.h>

#include "nv_util.h"

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvTerrain *nv_terrain_create(NvDevice *dev,
                              VkRenderPass render_pass,
                              const char *vert_path,
                              const char *frag_path,
                              const VkDescriptorSetLayout *set_layouts,
                              uint32_t set_layout_count,
                              VkDescriptorSetLayout shadow_set_layout) {
    if (!dev || !vert_path || !frag_path
        || render_pass == VK_NULL_HANDLE) {
        return NULL;
    }

    NvTerrain *terr = calloc(1, sizeof(NvTerrain));
    if (!terr) return NULL;
    terr->device = dev->handle;

    /* ---- Terrain descriptor set layout ----
     * binding 0: heightmap (sampler2D, vertex)
     * binding 1: splatmap (sampler2D, fragment)
     * binding 2-5: material layers 0-3 (sampler2D, fragment) */
    VkDescriptorSetLayoutBinding bindings[6];
    memset(bindings, 0, sizeof(bindings));
    bindings[0].binding         = 0;
    bindings[0].descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    bindings[0].descriptorCount = 1;
    bindings[0].stageFlags      = VK_SHADER_STAGE_VERTEX_BIT;
    for (uint32_t i = 1; i <= 5; i++) {
        bindings[i].binding         = i;
        bindings[i].descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        bindings[i].descriptorCount = 1;
        bindings[i].stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;
    }

    VkDescriptorSetLayoutCreateInfo dsl;
    memset(&dsl, 0, sizeof(dsl));
    dsl.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    dsl.bindingCount = 6;
    dsl.pBindings    = bindings;

    if (vkCreateDescriptorSetLayout(terr->device, &dsl, NULL,
                                     &terr->terrain_set_layout)
        != VK_SUCCESS) {
        goto fail;
    }

    /* ---- Pipeline layout: existing + terrain + shadow ---- */
    uint32_t total_sets = set_layout_count + 2;
    VkDescriptorSetLayout *all_layouts = calloc(total_sets,
                                                 sizeof(VkDescriptorSetLayout));
    if (!all_layouts) goto fail;
    for (uint32_t i = 0; i < set_layout_count; i++) {
        all_layouts[i] = set_layouts[i];
    }
    all_layouts[set_layout_count]     = terr->terrain_set_layout;
    all_layouts[set_layout_count + 1] = shadow_set_layout;

    /* Push constants: mat4 model (64) + vec4 terrainParams (16) = 80 */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_VERTEX_BIT
                    | VK_SHADER_STAGE_FRAGMENT_BIT;
    push.offset     = 0;
    push.size       = 80;

    VkPipelineLayoutCreateInfo pli;
    memset(&pli, 0, sizeof(pli));
    pli.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pli.setLayoutCount         = total_sets;
    pli.pSetLayouts            = all_layouts;
    pli.pushConstantRangeCount = 1;
    pli.pPushConstantRanges    = &push;

    VkResult layout_result = vkCreatePipelineLayout(
        terr->device, &pli, NULL, &terr->layout);
    free(all_layouts);
    if (layout_result != VK_SUCCESS) goto fail;

    /* ---- Shaders ---- */
    VkShaderModule vert = nv_load_shader(terr->device, vert_path);
    VkShaderModule frag = nv_load_shader(terr->device, frag_path);
    if (vert == VK_NULL_HANDLE || frag == VK_NULL_HANDLE) {
        if (vert) vkDestroyShaderModule(terr->device, vert, NULL);
        if (frag) vkDestroyShaderModule(terr->device, frag, NULL);
        goto fail;
    }

    VkPipelineShaderStageCreateInfo stages[2];
    memset(stages, 0, sizeof(stages));
    stages[0].sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stages[0].stage  = VK_SHADER_STAGE_VERTEX_BIT;
    stages[0].module = vert;
    stages[0].pName  = "main";
    stages[1].sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stages[1].stage  = VK_SHADER_STAGE_FRAGMENT_BIT;
    stages[1].module = frag;
    stages[1].pName  = "main";

    /* ---- Standard 64-byte vertex input (flat grid patch) ---- */
    VkVertexInputBindingDescription binding;
    memset(&binding, 0, sizeof(binding));
    binding.stride    = 64;
    binding.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

    VkVertexInputAttributeDescription attrs[5];
    memset(attrs, 0, sizeof(attrs));
    attrs[0].location = 0; attrs[0].format = VK_FORMAT_R32G32B32_SFLOAT; attrs[0].offset = 0;
    attrs[1].location = 1; attrs[1].format = VK_FORMAT_R32G32B32_SFLOAT; attrs[1].offset = 12;
    attrs[2].location = 2; attrs[2].format = VK_FORMAT_R32G32_SFLOAT;    attrs[2].offset = 24;
    attrs[3].location = 3; attrs[3].format = VK_FORMAT_R32G32B32A32_SFLOAT; attrs[3].offset = 32;
    attrs[4].location = 4; attrs[4].format = VK_FORMAT_R32G32B32A32_SFLOAT; attrs[4].offset = 48;

    VkPipelineVertexInputStateCreateInfo vi;
    memset(&vi, 0, sizeof(vi));
    vi.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vi.vertexBindingDescriptionCount   = 1;
    vi.pVertexBindingDescriptions      = &binding;
    vi.vertexAttributeDescriptionCount = 5;
    vi.pVertexAttributeDescriptions    = attrs;

    VkPipelineInputAssemblyStateCreateInfo assembly;
    memset(&assembly, 0, sizeof(assembly));
    assembly.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

    VkDynamicState dyn_states[] = {
        VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR};
    VkPipelineDynamicStateCreateInfo dynamic;
    memset(&dynamic, 0, sizeof(dynamic));
    dynamic.sType             = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
    dynamic.dynamicStateCount = 2;
    dynamic.pDynamicStates    = dyn_states;

    VkPipelineViewportStateCreateInfo viewport;
    memset(&viewport, 0, sizeof(viewport));
    viewport.sType         = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
    viewport.viewportCount = 1;
    viewport.scissorCount  = 1;

    VkPipelineRasterizationStateCreateInfo raster;
    memset(&raster, 0, sizeof(raster));
    raster.sType       = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    raster.polygonMode = VK_POLYGON_MODE_FILL;
    raster.cullMode    = VK_CULL_MODE_BACK_BIT;
    raster.frontFace   = VK_FRONT_FACE_COUNTER_CLOCKWISE;
    raster.lineWidth   = 1.0f;

    VkPipelineMultisampleStateCreateInfo msaa;
    memset(&msaa, 0, sizeof(msaa));
    msaa.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    msaa.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    VkPipelineDepthStencilStateCreateInfo depth;
    memset(&depth, 0, sizeof(depth));
    depth.sType            = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
    depth.depthTestEnable  = VK_TRUE;
    depth.depthWriteEnable = VK_TRUE;
    depth.depthCompareOp   = VK_COMPARE_OP_LESS;

    VkPipelineColorBlendAttachmentState blend_attach;
    memset(&blend_attach, 0, sizeof(blend_attach));
    blend_attach.colorWriteMask = VK_COLOR_COMPONENT_R_BIT
                                | VK_COLOR_COMPONENT_G_BIT
                                | VK_COLOR_COMPONENT_B_BIT
                                | VK_COLOR_COMPONENT_A_BIT;

    VkPipelineColorBlendStateCreateInfo blend;
    memset(&blend, 0, sizeof(blend));
    blend.sType           = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
    blend.attachmentCount = 1;
    blend.pAttachments    = &blend_attach;

    VkGraphicsPipelineCreateInfo ci;
    memset(&ci, 0, sizeof(ci));
    ci.sType               = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    ci.stageCount          = 2;
    ci.pStages             = stages;
    ci.pVertexInputState   = &vi;
    ci.pInputAssemblyState = &assembly;
    ci.pViewportState      = &viewport;
    ci.pRasterizationState = &raster;
    ci.pMultisampleState   = &msaa;
    ci.pDepthStencilState  = &depth;
    ci.pColorBlendState    = &blend;
    ci.pDynamicState       = &dynamic;
    ci.layout              = terr->layout;
    ci.renderPass          = render_pass;
    ci.subpass             = 0;

    VkResult result = vkCreateGraphicsPipelines(
        terr->device, VK_NULL_HANDLE, 1, &ci, NULL, &terr->pipeline);

    vkDestroyShaderModule(terr->device, vert, NULL);
    vkDestroyShaderModule(terr->device, frag, NULL);

    if (result != VK_SUCCESS) goto fail;
    return terr;

fail:
    nv_terrain_destroy(terr);
    return NULL;
}

void nv_terrain_destroy(NvTerrain *terr) {
    if (!terr) return;
    if (terr->pipeline != VK_NULL_HANDLE)
        vkDestroyPipeline(terr->device, terr->pipeline, NULL);
    if (terr->layout != VK_NULL_HANDLE)
        vkDestroyPipelineLayout(terr->device, terr->layout, NULL);
    if (terr->terrain_set_layout != VK_NULL_HANDLE)
        vkDestroyDescriptorSetLayout(terr->device,
                                      terr->terrain_set_layout, NULL);
    free(terr);
}

VkDescriptorSetLayout nv_terrain_set_layout(const NvTerrain *terr) {
    return terr ? terr->terrain_set_layout : VK_NULL_HANDLE;
}
