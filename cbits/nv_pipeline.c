/*
 * nv_pipeline — Vulkan render pass, graphics pipeline, framebuffers.
 */

#include "nv_pipeline.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ----------------------------------------------------------------
 * SPIR-V shader loading
 * ---------------------------------------------------------------- */

static VkShaderModule load_shader(VkDevice device, const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) {
        fprintf(stderr, "[nova] failed to open shader: %s\n", path);
        return VK_NULL_HANDLE;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

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
 * Render pass
 * ---------------------------------------------------------------- */

static int create_render_pass(NvPipeline *pip, VkFormat color_format,
                              VkFormat depth_format) {
    VkAttachmentDescription attachments[2];
    memset(attachments, 0, sizeof(attachments));

    /* Color attachment */
    attachments[0].format         = color_format;
    attachments[0].samples        = VK_SAMPLE_COUNT_1_BIT;
    attachments[0].loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachments[0].storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    attachments[0].stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachments[0].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[0].initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attachments[0].finalLayout    = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

    /* Depth attachment */
    attachments[1].format         = depth_format;
    attachments[1].samples        = VK_SAMPLE_COUNT_1_BIT;
    attachments[1].loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachments[1].storeOp        = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[1].stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachments[1].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[1].initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attachments[1].finalLayout =
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

    VkAttachmentReference color_ref;
    memset(&color_ref, 0, sizeof(color_ref));
    color_ref.attachment = 0;
    color_ref.layout     = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

    VkAttachmentReference depth_ref;
    memset(&depth_ref, 0, sizeof(depth_ref));
    depth_ref.attachment = 1;
    depth_ref.layout =
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

    VkSubpassDescription subpass;
    memset(&subpass, 0, sizeof(subpass));
    subpass.pipelineBindPoint       = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount    = 1;
    subpass.pColorAttachments       = &color_ref;
    subpass.pDepthStencilAttachment = &depth_ref;

    VkSubpassDependency dep;
    memset(&dep, 0, sizeof(dep));
    dep.srcSubpass    = VK_SUBPASS_EXTERNAL;
    dep.dstSubpass    = 0;
    dep.srcStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                      | VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    dep.dstStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                      | VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    dep.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                      | VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;

    VkRenderPassCreateInfo rp_info;
    memset(&rp_info, 0, sizeof(rp_info));
    rp_info.sType           = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    rp_info.attachmentCount = 2;
    rp_info.pAttachments    = attachments;
    rp_info.subpassCount    = 1;
    rp_info.pSubpasses      = &subpass;
    rp_info.dependencyCount = 1;
    rp_info.pDependencies   = &dep;

    return vkCreateRenderPass(pip->device, &rp_info, NULL,
                              &pip->render_pass)
           == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Pipeline layout
 * ---------------------------------------------------------------- */

static int create_layout(NvPipeline *pip,
                         const VkDescriptorSetLayout *set_layouts,
                         uint32_t set_layout_count) {
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    push.offset     = 0;
    push.size       = NV_PUSH_CONSTANT_SIZE;

    VkPipelineLayoutCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    info.pushConstantRangeCount = 1;
    info.pPushConstantRanges    = &push;
    info.setLayoutCount         = set_layout_count;
    info.pSetLayouts            = set_layouts;

    return vkCreatePipelineLayout(pip->device, &info, NULL,
                                  &pip->layout)
           == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Graphics pipeline
 * ---------------------------------------------------------------- */

static int create_pipeline(NvPipeline *pip, NvSwapchain *sc,
                           const char *vert_path,
                           const char *frag_path) {
    VkShaderModule vert = load_shader(pip->device, vert_path);
    VkShaderModule frag = load_shader(pip->device, frag_path);
    if (vert == VK_NULL_HANDLE || frag == VK_NULL_HANDLE) {
        if (vert != VK_NULL_HANDLE) {
            vkDestroyShaderModule(pip->device, vert, NULL);
        }
        if (frag != VK_NULL_HANDLE) {
            vkDestroyShaderModule(pip->device, frag, NULL);
        }
        return 0;
    }

    /* Shader stages */
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

    /* Vertex input — matches 64-byte Vertex from Mesh.Buffer */
    VkVertexInputBindingDescription binding;
    memset(&binding, 0, sizeof(binding));
    binding.binding   = 0;
    binding.stride    = 64;
    binding.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

    VkVertexInputAttributeDescription attrs[5];
    memset(attrs, 0, sizeof(attrs));
    /* position V3 */
    attrs[0].location = 0;
    attrs[0].format   = VK_FORMAT_R32G32B32_SFLOAT;
    attrs[0].offset   = 0;
    /* normal V3 */
    attrs[1].location = 1;
    attrs[1].format   = VK_FORMAT_R32G32B32_SFLOAT;
    attrs[1].offset   = 12;
    /* UV V2 */
    attrs[2].location = 2;
    attrs[2].format   = VK_FORMAT_R32G32_SFLOAT;
    attrs[2].offset   = 24;
    /* tangent V4 */
    attrs[3].location = 3;
    attrs[3].format   = VK_FORMAT_R32G32B32A32_SFLOAT;
    attrs[3].offset   = 32;
    /* color V4 */
    attrs[4].location = 4;
    attrs[4].format   = VK_FORMAT_R32G32B32A32_SFLOAT;
    attrs[4].offset   = 48;

    VkPipelineVertexInputStateCreateInfo vertex_input;
    memset(&vertex_input, 0, sizeof(vertex_input));
    vertex_input.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vertex_input.vertexBindingDescriptionCount   = 1;
    vertex_input.pVertexBindingDescriptions      = &binding;
    vertex_input.vertexAttributeDescriptionCount = 5;
    vertex_input.pVertexAttributeDescriptions    = attrs;

    /* Input assembly */
    VkPipelineInputAssemblyStateCreateInfo assembly;
    memset(&assembly, 0, sizeof(assembly));
    assembly.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

    /* Dynamic viewport + scissor */
    VkDynamicState dyn_states[] = {
        VK_DYNAMIC_STATE_VIEWPORT,
        VK_DYNAMIC_STATE_SCISSOR,
    };

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

    /* Rasterization */
    VkPipelineRasterizationStateCreateInfo raster;
    memset(&raster, 0, sizeof(raster));
    raster.sType       = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    raster.polygonMode = VK_POLYGON_MODE_FILL;
    raster.cullMode    = VK_CULL_MODE_BACK_BIT;
    raster.frontFace   = VK_FRONT_FACE_COUNTER_CLOCKWISE;
    raster.lineWidth   = 1.0f;

    /* Multisample (no MSAA) */
    VkPipelineMultisampleStateCreateInfo msaa;
    memset(&msaa, 0, sizeof(msaa));
    msaa.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    msaa.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    /* Depth stencil */
    VkPipelineDepthStencilStateCreateInfo depth;
    memset(&depth, 0, sizeof(depth));
    depth.sType            = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
    depth.depthTestEnable  = VK_TRUE;
    depth.depthWriteEnable = VK_TRUE;
    depth.depthCompareOp   = VK_COMPARE_OP_LESS;

    /* Color blend (opaque) */
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

    /* Create pipeline */
    VkGraphicsPipelineCreateInfo ci;
    memset(&ci, 0, sizeof(ci));
    ci.sType               = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    ci.stageCount          = 2;
    ci.pStages             = stages;
    ci.pVertexInputState   = &vertex_input;
    ci.pInputAssemblyState = &assembly;
    ci.pViewportState      = &viewport;
    ci.pRasterizationState = &raster;
    ci.pMultisampleState   = &msaa;
    ci.pDepthStencilState  = &depth;
    ci.pColorBlendState    = &blend;
    ci.pDynamicState       = &dynamic;
    ci.layout              = pip->layout;
    ci.renderPass          = pip->render_pass;
    ci.subpass             = 0;

    VkResult result = vkCreateGraphicsPipelines(
        pip->device, VK_NULL_HANDLE, 1, &ci, NULL, &pip->handle);

    vkDestroyShaderModule(pip->device, vert, NULL);
    vkDestroyShaderModule(pip->device, frag, NULL);

    (void)sc;
    return result == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Framebuffers
 * ---------------------------------------------------------------- */

static void destroy_framebuffers(NvPipeline *pip) {
    if (pip->framebuffers) {
        for (uint32_t i = 0; i < pip->framebuffer_count; i++) {
            if (pip->framebuffers[i] != VK_NULL_HANDLE) {
                vkDestroyFramebuffer(
                    pip->device, pip->framebuffers[i], NULL);
            }
        }
        free(pip->framebuffers);
        pip->framebuffers      = NULL;
        pip->framebuffer_count = 0;
    }
}

static int create_framebuffers(NvPipeline *pip, NvSwapchain *sc) {
    pip->framebuffer_count = sc->image_count;
    pip->framebuffers =
        calloc(sc->image_count, sizeof(VkFramebuffer));
    if (!pip->framebuffers) {
        return 0;
    }

    for (uint32_t i = 0; i < sc->image_count; i++) {
        VkImageView views[2];
        views[0] = sc->image_views[i];
        views[1] = sc->depth_view;

        VkFramebufferCreateInfo info;
        memset(&info, 0, sizeof(info));
        info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        info.renderPass      = pip->render_pass;
        info.attachmentCount = 2;
        info.pAttachments    = views;
        info.width           = sc->extent.width;
        info.height          = sc->extent.height;
        info.layers          = 1;

        if (vkCreateFramebuffer(pip->device, &info, NULL,
                                &pip->framebuffers[i])
            != VK_SUCCESS) {
            return 0;
        }
    }
    return 1;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvPipeline *nv_pipeline_create(NvDevice *dev, NvSwapchain *sc,
                               const char *vert_path,
                               const char *frag_path,
                               const VkDescriptorSetLayout *set_layouts,
                               uint32_t set_layout_count) {
    if (!dev || !sc || !vert_path || !frag_path) {
        return NULL;
    }

    NvPipeline *pip = calloc(1, sizeof(NvPipeline));
    if (!pip) {
        return NULL;
    }
    pip->device = dev->handle;

    if (!create_render_pass(pip, sc->image_format, sc->depth_format)) {
        goto fail;
    }
    if (!create_layout(pip, set_layouts, set_layout_count)) {
        goto fail;
    }
    if (!create_pipeline(pip, sc, vert_path, frag_path)) {
        goto fail;
    }
    if (!create_framebuffers(pip, sc)) {
        goto fail;
    }

    return pip;

fail:
    nv_pipeline_destroy(pip);
    return NULL;
}

void nv_pipeline_destroy(NvPipeline *pip) {
    if (!pip) {
        return;
    }
    destroy_framebuffers(pip);
    if (pip->handle != VK_NULL_HANDLE) {
        vkDestroyPipeline(pip->device, pip->handle, NULL);
    }
    if (pip->layout != VK_NULL_HANDLE) {
        vkDestroyPipelineLayout(pip->device, pip->layout, NULL);
    }
    if (pip->render_pass != VK_NULL_HANDLE) {
        vkDestroyRenderPass(pip->device, pip->render_pass, NULL);
    }
    free(pip);
}

int nv_pipeline_recreate_framebuffers(NvPipeline *pip,
                                      NvSwapchain *sc) {
    if (!pip || !sc) {
        return 0;
    }
    destroy_framebuffers(pip);
    return create_framebuffers(pip, sc);
}
