/*
 * nv_shadow — Cascaded shadow map resources.
 */

#include "nv_shadow.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nv_vma.h"

/* ----------------------------------------------------------------
 * SPIR-V shader loading (same pattern as nv_pipeline.c)
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
 * Depth array image
 * ---------------------------------------------------------------- */

static int create_depth_image(NvShadow *shadow, NvAllocator *alloc) {
    uint32_t res = shadow->resolution;

    VkImageCreateInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    img_info.imageType     = VK_IMAGE_TYPE_2D;
    img_info.format        = shadow->depth_format;
    img_info.extent.width  = res;
    img_info.extent.height = res;
    img_info.extent.depth  = 1;
    img_info.mipLevels     = 1;
    img_info.arrayLayers   = NV_SHADOW_CASCADE_COUNT;
    img_info.samples       = VK_SAMPLE_COUNT_1_BIT;
    img_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
    img_info.usage         = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                           | VK_IMAGE_USAGE_SAMPLED_BIT;
    img_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    if (!nv_vma_create_image(alloc->vma, &img_info,
                              &shadow->image, &shadow->allocation)) {
        return 0;
    }
    shadow->vma = alloc->vma;
    return 1;
}

/* ----------------------------------------------------------------
 * Image views
 * ---------------------------------------------------------------- */

static int create_image_views(NvShadow *shadow) {
    /* Array view for shader sampling (all 4 layers) */
    VkImageViewCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    info.image    = shadow->image;
    info.viewType = VK_IMAGE_VIEW_TYPE_2D_ARRAY;
    info.format   = shadow->depth_format;
    info.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_DEPTH_BIT;
    info.subresourceRange.baseMipLevel   = 0;
    info.subresourceRange.levelCount     = 1;
    info.subresourceRange.baseArrayLayer = 0;
    info.subresourceRange.layerCount     = NV_SHADOW_CASCADE_COUNT;

    if (vkCreateImageView(shadow->device, &info, NULL,
                           &shadow->array_view) != VK_SUCCESS) {
        return 0;
    }

    /* Per-layer views for framebuffer attachments */
    for (uint32_t i = 0; i < NV_SHADOW_CASCADE_COUNT; i++) {
        VkImageViewCreateInfo layer_info;
        memset(&layer_info, 0, sizeof(layer_info));
        layer_info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        layer_info.image    = shadow->image;
        layer_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
        layer_info.format   = shadow->depth_format;
        layer_info.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_DEPTH_BIT;
        layer_info.subresourceRange.baseMipLevel   = 0;
        layer_info.subresourceRange.levelCount     = 1;
        layer_info.subresourceRange.baseArrayLayer = i;
        layer_info.subresourceRange.layerCount     = 1;

        if (vkCreateImageView(shadow->device, &layer_info, NULL,
                               &shadow->layer_views[i]) != VK_SUCCESS) {
            return 0;
        }
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Comparison sampler (for hardware PCF)
 * ---------------------------------------------------------------- */

static int create_sampler(NvShadow *shadow) {
    VkSamplerCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType        = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
    info.magFilter    = VK_FILTER_LINEAR;
    info.minFilter    = VK_FILTER_LINEAR;
    info.mipmapMode   = VK_SAMPLER_MIPMAP_MODE_NEAREST;
    info.addressModeU = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
    info.addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
    info.addressModeW = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
    info.borderColor  = VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE;
    info.compareEnable = VK_TRUE;
    info.compareOp     = VK_COMPARE_OP_LESS;
    info.maxLod        = 1.0f;

    return vkCreateSampler(shadow->device, &info, NULL,
                            &shadow->sampler) == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Depth-only render pass
 * ---------------------------------------------------------------- */

static int create_render_pass(NvShadow *shadow) {
    VkAttachmentDescription attach;
    memset(&attach, 0, sizeof(attach));
    attach.format         = shadow->depth_format;
    attach.samples        = VK_SAMPLE_COUNT_1_BIT;
    attach.loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attach.storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    attach.stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attach.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attach.initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attach.finalLayout    =
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;

    VkAttachmentReference depth_ref;
    memset(&depth_ref, 0, sizeof(depth_ref));
    depth_ref.attachment = 0;
    depth_ref.layout     =
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

    VkSubpassDescription subpass;
    memset(&subpass, 0, sizeof(subpass));
    subpass.pipelineBindPoint       = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount    = 0;
    subpass.pDepthStencilAttachment = &depth_ref;

    VkSubpassDependency deps[2];
    memset(deps, 0, sizeof(deps));

    /* Dependency: external -> subpass 0 (depth write) */
    deps[0].srcSubpass    = VK_SUBPASS_EXTERNAL;
    deps[0].dstSubpass    = 0;
    deps[0].srcStageMask  = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    deps[0].dstStageMask  = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    deps[0].srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
    deps[0].dstAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
    deps[0].dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;

    /* Dependency: subpass 0 -> external (shader read) */
    deps[1].srcSubpass    = 0;
    deps[1].dstSubpass    = VK_SUBPASS_EXTERNAL;
    deps[1].srcStageMask  = VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT;
    deps[1].dstStageMask  = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    deps[1].srcAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
    deps[1].dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
    deps[1].dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;

    VkRenderPassCreateInfo rp_info;
    memset(&rp_info, 0, sizeof(rp_info));
    rp_info.sType           = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    rp_info.attachmentCount = 1;
    rp_info.pAttachments    = &attach;
    rp_info.subpassCount    = 1;
    rp_info.pSubpasses      = &subpass;
    rp_info.dependencyCount = 2;
    rp_info.pDependencies   = deps;

    return vkCreateRenderPass(shadow->device, &rp_info, NULL,
                               &shadow->render_pass) == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Framebuffers (one per cascade layer)
 * ---------------------------------------------------------------- */

static int create_framebuffers(NvShadow *shadow) {
    for (uint32_t i = 0; i < NV_SHADOW_CASCADE_COUNT; i++) {
        VkFramebufferCreateInfo info;
        memset(&info, 0, sizeof(info));
        info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        info.renderPass      = shadow->render_pass;
        info.attachmentCount = 1;
        info.pAttachments    = &shadow->layer_views[i];
        info.width           = shadow->resolution;
        info.height          = shadow->resolution;
        info.layers          = 1;

        if (vkCreateFramebuffer(shadow->device, &info, NULL,
                                 &shadow->framebuffers[i])
            != VK_SUCCESS) {
            return 0;
        }
    }
    return 1;
}

/* ----------------------------------------------------------------
 * Shadow pipeline (depth-only, no fragment shader)
 * ---------------------------------------------------------------- */

static int create_pipeline(NvShadow *shadow, const char *vert_path) {
    VkShaderModule vert = load_shader(shadow->device, vert_path);
    if (vert == VK_NULL_HANDLE) {
        return 0;
    }

    /* Push constant range: mat4 lightVP (64) + mat4 model (64) */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    push.offset     = 0;
    push.size       = 128;

    VkPipelineLayoutCreateInfo layout_info;
    memset(&layout_info, 0, sizeof(layout_info));
    layout_info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    layout_info.pushConstantRangeCount = 1;
    layout_info.pPushConstantRanges    = &push;

    if (vkCreatePipelineLayout(shadow->device, &layout_info, NULL,
                                &shadow->pipeline_layout)
        != VK_SUCCESS) {
        vkDestroyShaderModule(shadow->device, vert, NULL);
        return 0;
    }

    /* Single vertex shader stage */
    VkPipelineShaderStageCreateInfo stage;
    memset(&stage, 0, sizeof(stage));
    stage.sType  = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    stage.stage  = VK_SHADER_STAGE_VERTEX_BIT;
    stage.module = vert;
    stage.pName  = "main";

    /* Vertex input — same 64-byte layout as PBR pipeline */
    VkVertexInputBindingDescription binding;
    memset(&binding, 0, sizeof(binding));
    binding.binding   = 0;
    binding.stride    = 64;
    binding.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

    VkVertexInputAttributeDescription attrs[5];
    memset(attrs, 0, sizeof(attrs));
    attrs[0].location = 0;
    attrs[0].format   = VK_FORMAT_R32G32B32_SFLOAT;
    attrs[0].offset   = 0;
    attrs[1].location = 1;
    attrs[1].format   = VK_FORMAT_R32G32B32_SFLOAT;
    attrs[1].offset   = 12;
    attrs[2].location = 2;
    attrs[2].format   = VK_FORMAT_R32G32_SFLOAT;
    attrs[2].offset   = 24;
    attrs[3].location = 3;
    attrs[3].format   = VK_FORMAT_R32G32B32A32_SFLOAT;
    attrs[3].offset   = 32;
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

    VkPipelineInputAssemblyStateCreateInfo assembly;
    memset(&assembly, 0, sizeof(assembly));
    assembly.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

    /* Dynamic viewport, scissor, depth bias */
    VkDynamicState dyn_states[] = {
        VK_DYNAMIC_STATE_VIEWPORT,
        VK_DYNAMIC_STATE_SCISSOR,
        VK_DYNAMIC_STATE_DEPTH_BIAS,
    };

    VkPipelineDynamicStateCreateInfo dynamic;
    memset(&dynamic, 0, sizeof(dynamic));
    dynamic.sType             = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
    dynamic.dynamicStateCount = 3;
    dynamic.pDynamicStates    = dyn_states;

    VkPipelineViewportStateCreateInfo viewport;
    memset(&viewport, 0, sizeof(viewport));
    viewport.sType         = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
    viewport.viewportCount = 1;
    viewport.scissorCount  = 1;

    /* Front-face culling + depth bias for shadow acne */
    VkPipelineRasterizationStateCreateInfo raster;
    memset(&raster, 0, sizeof(raster));
    raster.sType            = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    raster.polygonMode      = VK_POLYGON_MODE_FILL;
    raster.cullMode         = VK_CULL_MODE_FRONT_BIT;
    raster.frontFace        = VK_FRONT_FACE_COUNTER_CLOCKWISE;
    raster.lineWidth        = 1.0f;
    raster.depthBiasEnable  = VK_TRUE;
    raster.depthClampEnable = VK_TRUE;

    VkPipelineMultisampleStateCreateInfo msaa;
    memset(&msaa, 0, sizeof(msaa));
    msaa.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    msaa.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    VkPipelineDepthStencilStateCreateInfo depth;
    memset(&depth, 0, sizeof(depth));
    depth.sType            = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
    depth.depthTestEnable  = VK_TRUE;
    depth.depthWriteEnable = VK_TRUE;
    depth.depthCompareOp   = VK_COMPARE_OP_LESS_OR_EQUAL;

    /* No color blend (depth-only) */
    VkPipelineColorBlendStateCreateInfo blend;
    memset(&blend, 0, sizeof(blend));
    blend.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;

    VkGraphicsPipelineCreateInfo ci;
    memset(&ci, 0, sizeof(ci));
    ci.sType               = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    ci.stageCount          = 1;
    ci.pStages             = &stage;
    ci.pVertexInputState   = &vertex_input;
    ci.pInputAssemblyState = &assembly;
    ci.pViewportState      = &viewport;
    ci.pRasterizationState = &raster;
    ci.pMultisampleState   = &msaa;
    ci.pDepthStencilState  = &depth;
    ci.pColorBlendState    = &blend;
    ci.pDynamicState       = &dynamic;
    ci.layout              = shadow->pipeline_layout;
    ci.renderPass          = shadow->render_pass;
    ci.subpass             = 0;

    VkResult result = vkCreateGraphicsPipelines(
        shadow->device, VK_NULL_HANDLE, 1, &ci, NULL,
        &shadow->pipeline);

    vkDestroyShaderModule(shadow->device, vert, NULL);
    return result == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvShadow *nv_shadow_create(NvDevice *dev, NvAllocator *alloc,
                            VkFormat depth_format, uint32_t resolution,
                            const char *vert_path) {
    if (!dev || !alloc || !vert_path || resolution == 0) {
        return NULL;
    }

    NvShadow *shadow = calloc(1, sizeof(NvShadow));
    if (!shadow) {
        return NULL;
    }
    shadow->device       = dev->handle;
    shadow->resolution   = resolution;
    shadow->depth_format = depth_format;

    if (!create_depth_image(shadow, alloc)) {
        goto fail;
    }
    if (!create_image_views(shadow)) {
        goto fail;
    }
    if (!create_sampler(shadow)) {
        goto fail;
    }
    if (!create_render_pass(shadow)) {
        goto fail;
    }
    if (!create_framebuffers(shadow)) {
        goto fail;
    }
    if (!create_pipeline(shadow, vert_path)) {
        goto fail;
    }

    return shadow;

fail:
    nv_shadow_destroy(shadow);
    return NULL;
}

void nv_shadow_destroy(NvShadow *shadow) {
    if (!shadow) {
        return;
    }
    if (shadow->pipeline != VK_NULL_HANDLE) {
        vkDestroyPipeline(shadow->device, shadow->pipeline, NULL);
    }
    if (shadow->pipeline_layout != VK_NULL_HANDLE) {
        vkDestroyPipelineLayout(shadow->device,
                                 shadow->pipeline_layout, NULL);
    }
    for (uint32_t i = 0; i < NV_SHADOW_CASCADE_COUNT; i++) {
        if (shadow->framebuffers[i] != VK_NULL_HANDLE) {
            vkDestroyFramebuffer(shadow->device,
                                  shadow->framebuffers[i], NULL);
        }
        if (shadow->layer_views[i] != VK_NULL_HANDLE) {
            vkDestroyImageView(shadow->device,
                                shadow->layer_views[i], NULL);
        }
    }
    if (shadow->render_pass != VK_NULL_HANDLE) {
        vkDestroyRenderPass(shadow->device, shadow->render_pass,
                             NULL);
    }
    if (shadow->sampler != VK_NULL_HANDLE) {
        vkDestroySampler(shadow->device, shadow->sampler, NULL);
    }
    if (shadow->array_view != VK_NULL_HANDLE) {
        vkDestroyImageView(shadow->device, shadow->array_view, NULL);
    }
    if (shadow->image != VK_NULL_HANDLE) {
        nv_vma_destroy_image(shadow->vma, shadow->image,
                              shadow->allocation);
    }
    free(shadow);
}

void nv_shadow_begin_pass(NvShadow *shadow, NvFrame *fr,
                          uint32_t cascade_index) {
    VkCommandBuffer cmd = fr->cmd[fr->current_frame];

    VkClearValue clear;
    memset(&clear, 0, sizeof(clear));
    clear.depthStencil.depth   = 1.0f;
    clear.depthStencil.stencil = 0;

    VkRenderPassBeginInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType                    = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    rp.renderPass               = shadow->render_pass;
    rp.framebuffer              = shadow->framebuffers[cascade_index];
    rp.renderArea.extent.width  = shadow->resolution;
    rp.renderArea.extent.height = shadow->resolution;
    rp.clearValueCount          = 1;
    rp.pClearValues             = &clear;

    vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);

    vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                       shadow->pipeline);

    VkViewport vp;
    memset(&vp, 0, sizeof(vp));
    vp.width    = (float)shadow->resolution;
    vp.height   = (float)shadow->resolution;
    vp.maxDepth = 1.0f;
    vkCmdSetViewport(cmd, 0, 1, &vp);

    VkRect2D scissor;
    memset(&scissor, 0, sizeof(scissor));
    scissor.extent.width  = shadow->resolution;
    scissor.extent.height = shadow->resolution;
    vkCmdSetScissor(cmd, 0, 1, &scissor);

    /* Depth bias: constant 1.25, slope 1.75 */
    vkCmdSetDepthBias(cmd, 1.25f, 0.0f, 1.75f);
}

void nv_shadow_end_pass(NvFrame *fr) {
    vkCmdEndRenderPass(fr->cmd[fr->current_frame]);
}

VkImageView nv_shadow_array_view(const NvShadow *shadow) {
    return shadow ? shadow->array_view : VK_NULL_HANDLE;
}

VkSampler nv_shadow_sampler(const NvShadow *shadow) {
    return shadow ? shadow->sampler : VK_NULL_HANDLE;
}
