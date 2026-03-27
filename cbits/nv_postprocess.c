/*
 * nv_postprocess — HDR framebuffer, bloom, ACES tonemap, FXAA.
 */

#include "nv_postprocess.h"

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
    uint32_t *code = malloc((size_t)size);
    if (!code) { fclose(f); return VK_NULL_HANDLE; }
    if (fread(code, 1, (size_t)size, f) != (size_t)size) {
        free(code); fclose(f); return VK_NULL_HANDLE;
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
 * HDR image (RGBA16F, full resolution)
 * ---------------------------------------------------------------- */

static int create_hdr_image(NvPostProcess *pp, NvAllocator *alloc) {
    VkImageCreateInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    img_info.imageType     = VK_IMAGE_TYPE_2D;
    img_info.format        = pp->hdr_format;
    img_info.extent.width  = pp->width;
    img_info.extent.height = pp->height;
    img_info.extent.depth  = 1;
    img_info.mipLevels     = 1;
    img_info.arrayLayers   = 1;
    img_info.samples       = VK_SAMPLE_COUNT_1_BIT;
    img_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
    img_info.usage         = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                           | VK_IMAGE_USAGE_SAMPLED_BIT;
    img_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    if (!nv_vma_create_image(alloc->vma, &img_info,
                              &pp->hdr_image, &pp->hdr_allocation)) {
        return 0;
    }
    pp->vma = alloc->vma;

    /* Image view */
    VkImageViewCreateInfo view_info;
    memset(&view_info, 0, sizeof(view_info));
    view_info.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    view_info.image    = pp->hdr_image;
    view_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
    view_info.format   = pp->hdr_format;
    view_info.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
    view_info.subresourceRange.levelCount     = 1;
    view_info.subresourceRange.layerCount     = 1;

    if (vkCreateImageView(pp->device, &view_info, NULL,
                           &pp->hdr_view) != VK_SUCCESS) {
        return 0;
    }

    /* Sampler */
    VkSamplerCreateInfo sam;
    memset(&sam, 0, sizeof(sam));
    sam.sType        = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
    sam.magFilter    = VK_FILTER_LINEAR;
    sam.minFilter    = VK_FILTER_LINEAR;
    sam.mipmapMode   = VK_SAMPLER_MIPMAP_MODE_NEAREST;
    sam.addressModeU = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.addressModeW = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.maxLod       = 1.0f;

    return vkCreateSampler(pp->device, &sam, NULL,
                            &pp->hdr_sampler) == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Bloom mip chain (half-res base, 5 levels)
 * ---------------------------------------------------------------- */

static uint32_t mip_dim(uint32_t base, uint32_t level) {
    uint32_t d = base >> (level + 1);
    return d > 0 ? d : 1;
}

static int create_bloom_image(NvPostProcess *pp, NvAllocator *alloc) {
    uint32_t bw = mip_dim(pp->width, 0);
    uint32_t bh = mip_dim(pp->height, 0);

    VkImageCreateInfo img_info;
    memset(&img_info, 0, sizeof(img_info));
    img_info.sType         = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
    img_info.imageType     = VK_IMAGE_TYPE_2D;
    img_info.format        = pp->hdr_format;
    img_info.extent.width  = bw;
    img_info.extent.height = bh;
    img_info.extent.depth  = 1;
    img_info.mipLevels     = NV_BLOOM_MIP_COUNT;
    img_info.arrayLayers   = 1;
    img_info.samples       = VK_SAMPLE_COUNT_1_BIT;
    img_info.tiling        = VK_IMAGE_TILING_OPTIMAL;
    img_info.usage         = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                           | VK_IMAGE_USAGE_SAMPLED_BIT;
    img_info.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;

    if (!nv_vma_create_image(alloc->vma, &img_info,
                              &pp->bloom_image, &pp->bloom_allocation)) {
        return 0;
    }

    /* Per-mip views */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        VkImageViewCreateInfo vi;
        memset(&vi, 0, sizeof(vi));
        vi.sType    = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        vi.image    = pp->bloom_image;
        vi.viewType = VK_IMAGE_VIEW_TYPE_2D;
        vi.format   = pp->hdr_format;
        vi.subresourceRange.aspectMask     = VK_IMAGE_ASPECT_COLOR_BIT;
        vi.subresourceRange.baseMipLevel   = i;
        vi.subresourceRange.levelCount     = 1;
        vi.subresourceRange.layerCount     = 1;

        if (vkCreateImageView(pp->device, &vi, NULL,
                               &pp->bloom_mip_views[i]) != VK_SUCCESS) {
            return 0;
        }
    }

    /* Sampler */
    VkSamplerCreateInfo sam;
    memset(&sam, 0, sizeof(sam));
    sam.sType        = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
    sam.magFilter    = VK_FILTER_LINEAR;
    sam.minFilter    = VK_FILTER_LINEAR;
    sam.mipmapMode   = VK_SAMPLER_MIPMAP_MODE_LINEAR;
    sam.addressModeU = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.addressModeW = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
    sam.maxLod       = (float)NV_BLOOM_MIP_COUNT;

    return vkCreateSampler(pp->device, &sam, NULL,
                            &pp->bloom_sampler) == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Render passes
 * ---------------------------------------------------------------- */

static int create_hdr_render_pass(NvPostProcess *pp) {
    VkAttachmentDescription attachments[2];
    memset(attachments, 0, sizeof(attachments));

    /* Color (RGBA16F) */
    attachments[0].format         = pp->hdr_format;
    attachments[0].samples        = VK_SAMPLE_COUNT_1_BIT;
    attachments[0].loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachments[0].storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    attachments[0].stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachments[0].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[0].initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attachments[0].finalLayout    = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

    /* Depth */
    attachments[1].format         = pp->depth_format;
    attachments[1].samples        = VK_SAMPLE_COUNT_1_BIT;
    attachments[1].loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    attachments[1].storeOp        = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[1].stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attachments[1].stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attachments[1].initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attachments[1].finalLayout    =
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;

    VkAttachmentReference color_ref = {0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};
    VkAttachmentReference depth_ref = {1,
        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL};

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

    VkRenderPassCreateInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType           = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    rp.attachmentCount = 2;
    rp.pAttachments    = attachments;
    rp.subpassCount    = 1;
    rp.pSubpasses      = &subpass;
    rp.dependencyCount = 1;
    rp.pDependencies   = &dep;

    return vkCreateRenderPass(pp->device, &rp, NULL,
                               &pp->hdr_render_pass) == VK_SUCCESS;
}

static int create_bloom_pass(VkDevice device, VkFormat format,
                              VkAttachmentLoadOp load_op,
                              VkImageLayout initial_layout,
                              VkRenderPass *out) {
    VkAttachmentDescription attach;
    memset(&attach, 0, sizeof(attach));
    attach.format         = format;
    attach.samples        = VK_SAMPLE_COUNT_1_BIT;
    attach.loadOp         = load_op;
    attach.storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    attach.stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attach.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attach.initialLayout  = initial_layout;
    attach.finalLayout    = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

    VkAttachmentReference ref = {0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};

    VkSubpassDescription subpass;
    memset(&subpass, 0, sizeof(subpass));
    subpass.pipelineBindPoint    = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount = 1;
    subpass.pColorAttachments    = &ref;

    VkSubpassDependency deps[2];
    memset(deps, 0, sizeof(deps));
    deps[0].srcSubpass    = VK_SUBPASS_EXTERNAL;
    deps[0].dstSubpass    = 0;
    deps[0].srcStageMask  = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    deps[0].dstStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    deps[0].srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
    deps[0].dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    deps[0].dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;
    deps[1].srcSubpass    = 0;
    deps[1].dstSubpass    = VK_SUBPASS_EXTERNAL;
    deps[1].srcStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    deps[1].dstStageMask  = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    deps[1].srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
    deps[1].dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
    deps[1].dependencyFlags = VK_DEPENDENCY_BY_REGION_BIT;

    VkRenderPassCreateInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType           = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    rp.attachmentCount = 1;
    rp.pAttachments    = &attach;
    rp.subpassCount    = 1;
    rp.pSubpasses      = &subpass;
    rp.dependencyCount = 2;
    rp.pDependencies   = deps;

    return vkCreateRenderPass(device, &rp, NULL, out) == VK_SUCCESS;
}

static int create_tonemap_render_pass(NvPostProcess *pp) {
    VkAttachmentDescription attach;
    memset(&attach, 0, sizeof(attach));
    attach.format         = pp->swapchain_format;
    attach.samples        = VK_SAMPLE_COUNT_1_BIT;
    attach.loadOp         = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attach.storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    attach.stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    attach.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    attach.initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    attach.finalLayout    = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

    VkAttachmentReference ref = {0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL};

    VkSubpassDescription subpass;
    memset(&subpass, 0, sizeof(subpass));
    subpass.pipelineBindPoint    = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount = 1;
    subpass.pColorAttachments    = &ref;

    VkSubpassDependency dep;
    memset(&dep, 0, sizeof(dep));
    dep.srcSubpass    = VK_SUBPASS_EXTERNAL;
    dep.dstSubpass    = 0;
    dep.srcStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dep.dstStageMask  = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dep.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;

    VkRenderPassCreateInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType           = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    rp.attachmentCount = 1;
    rp.pAttachments    = &attach;
    rp.subpassCount    = 1;
    rp.pSubpasses      = &subpass;
    rp.dependencyCount = 1;
    rp.pDependencies   = &dep;

    return vkCreateRenderPass(pp->device, &rp, NULL,
                               &pp->tonemap_render_pass) == VK_SUCCESS;
}

/* ----------------------------------------------------------------
 * Framebuffers
 * ---------------------------------------------------------------- */

static int create_hdr_framebuffer(NvPostProcess *pp, NvSwapchain *sc) {
    VkImageView views[2] = {pp->hdr_view, sc->depth_view};

    VkFramebufferCreateInfo info;
    memset(&info, 0, sizeof(info));
    info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
    info.renderPass      = pp->hdr_render_pass;
    info.attachmentCount = 2;
    info.pAttachments    = views;
    info.width           = pp->width;
    info.height          = pp->height;
    info.layers          = 1;

    return vkCreateFramebuffer(pp->device, &info, NULL,
                                &pp->hdr_framebuffer) == VK_SUCCESS;
}

static int create_bloom_framebuffers(NvPostProcess *pp) {
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        uint32_t w = mip_dim(pp->width, i);
        uint32_t h = mip_dim(pp->height, i);

        VkFramebufferCreateInfo info;
        memset(&info, 0, sizeof(info));
        info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        info.renderPass      = pp->bloom_down_pass;
        info.attachmentCount = 1;
        info.pAttachments    = &pp->bloom_mip_views[i];
        info.width           = w;
        info.height          = h;
        info.layers          = 1;

        if (vkCreateFramebuffer(pp->device, &info, NULL,
                                 &pp->bloom_fbs[i]) != VK_SUCCESS) {
            return 0;
        }
    }
    return 1;
}

static int create_tonemap_framebuffers(NvPostProcess *pp,
                                        NvSwapchain *sc) {
    pp->tonemap_fb_count = sc->image_count;
    pp->tonemap_framebuffers =
        calloc(sc->image_count, sizeof(VkFramebuffer));
    if (!pp->tonemap_framebuffers) return 0;

    for (uint32_t i = 0; i < sc->image_count; i++) {
        VkFramebufferCreateInfo info;
        memset(&info, 0, sizeof(info));
        info.sType           = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        info.renderPass      = pp->tonemap_render_pass;
        info.attachmentCount = 1;
        info.pAttachments    = &sc->image_views[i];
        info.width           = sc->extent.width;
        info.height          = sc->extent.height;
        info.layers          = 1;

        if (vkCreateFramebuffer(pp->device, &info, NULL,
                                 &pp->tonemap_framebuffers[i])
            != VK_SUCCESS) {
            return 0;
        }
    }
    return 1;
}

/* ----------------------------------------------------------------
 * Fullscreen pipeline helper
 * ---------------------------------------------------------------- */

static VkPipeline create_fullscreen_pipeline(
    VkDevice device,
    VkRenderPass render_pass,
    VkPipelineLayout layout,
    VkShaderModule vert,
    VkShaderModule frag,
    int additive_blend) {

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

    /* No vertex input — positions generated from gl_VertexIndex */
    VkPipelineVertexInputStateCreateInfo vi;
    memset(&vi, 0, sizeof(vi));
    vi.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;

    VkPipelineInputAssemblyStateCreateInfo ia;
    memset(&ia, 0, sizeof(ia));
    ia.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    ia.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;

    VkDynamicState dyn[] = {VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR};
    VkPipelineDynamicStateCreateInfo dyn_state;
    memset(&dyn_state, 0, sizeof(dyn_state));
    dyn_state.sType             = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
    dyn_state.dynamicStateCount = 2;
    dyn_state.pDynamicStates    = dyn;

    VkPipelineViewportStateCreateInfo vp;
    memset(&vp, 0, sizeof(vp));
    vp.sType         = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
    vp.viewportCount = 1;
    vp.scissorCount  = 1;

    VkPipelineRasterizationStateCreateInfo raster;
    memset(&raster, 0, sizeof(raster));
    raster.sType       = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    raster.polygonMode = VK_POLYGON_MODE_FILL;
    raster.cullMode    = VK_CULL_MODE_NONE;
    raster.frontFace   = VK_FRONT_FACE_COUNTER_CLOCKWISE;
    raster.lineWidth   = 1.0f;

    VkPipelineMultisampleStateCreateInfo msaa;
    memset(&msaa, 0, sizeof(msaa));
    msaa.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    msaa.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    VkPipelineDepthStencilStateCreateInfo depth;
    memset(&depth, 0, sizeof(depth));
    depth.sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;

    VkPipelineColorBlendAttachmentState blend_attach;
    memset(&blend_attach, 0, sizeof(blend_attach));
    blend_attach.colorWriteMask = VK_COLOR_COMPONENT_R_BIT
                                | VK_COLOR_COMPONENT_G_BIT
                                | VK_COLOR_COMPONENT_B_BIT
                                | VK_COLOR_COMPONENT_A_BIT;
    if (additive_blend) {
        blend_attach.blendEnable         = VK_TRUE;
        blend_attach.srcColorBlendFactor = VK_BLEND_FACTOR_ONE;
        blend_attach.dstColorBlendFactor = VK_BLEND_FACTOR_ONE;
        blend_attach.colorBlendOp        = VK_BLEND_OP_ADD;
        blend_attach.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
        blend_attach.dstAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
        blend_attach.alphaBlendOp        = VK_BLEND_OP_ADD;
    }

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
    ci.pInputAssemblyState = &ia;
    ci.pViewportState      = &vp;
    ci.pRasterizationState = &raster;
    ci.pMultisampleState   = &msaa;
    ci.pDepthStencilState  = &depth;
    ci.pColorBlendState    = &blend;
    ci.pDynamicState       = &dyn_state;
    ci.layout              = layout;
    ci.renderPass          = render_pass;
    ci.subpass             = 0;

    VkPipeline pipeline = VK_NULL_HANDLE;
    vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &ci, NULL,
                               &pipeline);
    return pipeline;
}

/* ----------------------------------------------------------------
 * Bloom pipelines + descriptors
 * ---------------------------------------------------------------- */

static int create_bloom_pipelines(NvPostProcess *pp,
                                   VkShaderModule vert,
                                   VkShaderModule down_frag,
                                   VkShaderModule up_frag) {
    /* Descriptor set layout: single combined image sampler */
    VkDescriptorSetLayoutBinding binding;
    memset(&binding, 0, sizeof(binding));
    binding.binding         = 0;
    binding.descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    binding.descriptorCount = 1;
    binding.stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;

    VkDescriptorSetLayoutCreateInfo dsl;
    memset(&dsl, 0, sizeof(dsl));
    dsl.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    dsl.bindingCount = 1;
    dsl.pBindings    = &binding;

    if (vkCreateDescriptorSetLayout(pp->device, &dsl, NULL,
                                     &pp->bloom_set_layout) != VK_SUCCESS) {
        return 0;
    }

    /* Pipeline layout: 16-byte push constants (texelSize, mip, pad) */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    push.size       = 16;

    VkPipelineLayoutCreateInfo pli;
    memset(&pli, 0, sizeof(pli));
    pli.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pli.setLayoutCount         = 1;
    pli.pSetLayouts            = &pp->bloom_set_layout;
    pli.pushConstantRangeCount = 1;
    pli.pPushConstantRanges    = &push;

    if (vkCreatePipelineLayout(pp->device, &pli, NULL,
                                &pp->bloom_pipe_layout) != VK_SUCCESS) {
        return 0;
    }

    /* Downsample pipeline (no blend) */
    pp->bloom_down_pipeline = create_fullscreen_pipeline(
        pp->device, pp->bloom_down_pass, pp->bloom_pipe_layout,
        vert, down_frag, 0);
    if (pp->bloom_down_pipeline == VK_NULL_HANDLE) return 0;

    /* Upsample pipeline (additive blend) */
    pp->bloom_up_pipeline = create_fullscreen_pipeline(
        pp->device, pp->bloom_up_pass, pp->bloom_pipe_layout,
        vert, up_frag, 1);
    return pp->bloom_up_pipeline != VK_NULL_HANDLE;
}

static int create_bloom_descriptors(NvPostProcess *pp) {
    uint32_t total_sets = NV_BLOOM_MIP_COUNT + (NV_BLOOM_MIP_COUNT - 1);

    VkDescriptorPoolSize pool_size;
    pool_size.type            = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    pool_size.descriptorCount = total_sets;

    VkDescriptorPoolCreateInfo pool_info;
    memset(&pool_info, 0, sizeof(pool_info));
    pool_info.sType         = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
    pool_info.maxSets       = total_sets;
    pool_info.poolSizeCount = 1;
    pool_info.pPoolSizes    = &pool_size;

    if (vkCreateDescriptorPool(pp->device, &pool_info, NULL,
                                &pp->bloom_desc_pool) != VK_SUCCESS) {
        return 0;
    }

    /* Allocate all sets */
    VkDescriptorSetLayout layouts[NV_BLOOM_MIP_COUNT + NV_BLOOM_MIP_COUNT];
    for (uint32_t i = 0; i < total_sets; i++) {
        layouts[i] = pp->bloom_set_layout;
    }

    VkDescriptorSet all_sets[NV_BLOOM_MIP_COUNT + NV_BLOOM_MIP_COUNT];
    VkDescriptorSetAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType              = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    alloc_info.descriptorPool     = pp->bloom_desc_pool;
    alloc_info.descriptorSetCount = total_sets;
    alloc_info.pSetLayouts        = layouts;

    if (vkAllocateDescriptorSets(pp->device, &alloc_info, all_sets)
        != VK_SUCCESS) {
        return 0;
    }

    /* Distribute sets */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        pp->bloom_down_sets[i] = all_sets[i];
    }
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT - 1; i++) {
        pp->bloom_up_sets[i] = all_sets[NV_BLOOM_MIP_COUNT + i];
    }

    /* Write downsample sets */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        VkDescriptorImageInfo img;
        memset(&img, 0, sizeof(img));
        img.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

        if (i == 0) {
            /* First downsample reads the HDR scene */
            img.imageView = pp->hdr_view;
            img.sampler   = pp->hdr_sampler;
        } else {
            /* Subsequent downsamples read previous bloom mip */
            img.imageView = pp->bloom_mip_views[i - 1];
            img.sampler   = pp->bloom_sampler;
        }

        VkWriteDescriptorSet write;
        memset(&write, 0, sizeof(write));
        write.sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        write.dstSet          = pp->bloom_down_sets[i];
        write.dstBinding      = 0;
        write.descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        write.descriptorCount = 1;
        write.pImageInfo      = &img;

        vkUpdateDescriptorSets(pp->device, 1, &write, 0, NULL);
    }

    /* Write upsample sets: set[i] reads bloom mip[i+1] */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT - 1; i++) {
        VkDescriptorImageInfo img;
        memset(&img, 0, sizeof(img));
        img.imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        img.imageView   = pp->bloom_mip_views[NV_BLOOM_MIP_COUNT - 1 - i];
        img.sampler     = pp->bloom_sampler;

        VkWriteDescriptorSet write;
        memset(&write, 0, sizeof(write));
        write.sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        write.dstSet          = pp->bloom_up_sets[i];
        write.dstBinding      = 0;
        write.descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        write.descriptorCount = 1;
        write.pImageInfo      = &img;

        vkUpdateDescriptorSets(pp->device, 1, &write, 0, NULL);
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Tonemap pipeline + descriptors
 * ---------------------------------------------------------------- */

static int create_tonemap_pipeline(NvPostProcess *pp,
                                    VkShaderModule vert,
                                    VkShaderModule frag) {
    /* 2 combined image samplers: hdr + bloom */
    VkDescriptorSetLayoutBinding bindings[2];
    memset(bindings, 0, sizeof(bindings));
    bindings[0].binding         = 0;
    bindings[0].descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    bindings[0].descriptorCount = 1;
    bindings[0].stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;
    bindings[1].binding         = 1;
    bindings[1].descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    bindings[1].descriptorCount = 1;
    bindings[1].stageFlags      = VK_SHADER_STAGE_FRAGMENT_BIT;

    VkDescriptorSetLayoutCreateInfo dsl;
    memset(&dsl, 0, sizeof(dsl));
    dsl.sType        = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
    dsl.bindingCount = 2;
    dsl.pBindings    = bindings;

    if (vkCreateDescriptorSetLayout(pp->device, &dsl, NULL,
                                     &pp->tonemap_set_layout) != VK_SUCCESS) {
        return 0;
    }

    /* Pipeline layout: 16-byte push constants */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_FRAGMENT_BIT;
    push.size       = 16;

    VkPipelineLayoutCreateInfo pli;
    memset(&pli, 0, sizeof(pli));
    pli.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pli.setLayoutCount         = 1;
    pli.pSetLayouts            = &pp->tonemap_set_layout;
    pli.pushConstantRangeCount = 1;
    pli.pPushConstantRanges    = &push;

    if (vkCreatePipelineLayout(pp->device, &pli, NULL,
                                &pp->tonemap_pipe_layout) != VK_SUCCESS) {
        return 0;
    }

    pp->tonemap_pipeline = create_fullscreen_pipeline(
        pp->device, pp->tonemap_render_pass, pp->tonemap_pipe_layout,
        vert, frag, 0);
    return pp->tonemap_pipeline != VK_NULL_HANDLE;
}

static int create_tonemap_descriptors(NvPostProcess *pp) {
    VkDescriptorPoolSize pool_size;
    pool_size.type            = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    pool_size.descriptorCount = 2;

    VkDescriptorPoolCreateInfo pool_info;
    memset(&pool_info, 0, sizeof(pool_info));
    pool_info.sType         = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
    pool_info.maxSets       = 1;
    pool_info.poolSizeCount = 1;
    pool_info.pPoolSizes    = &pool_size;

    if (vkCreateDescriptorPool(pp->device, &pool_info, NULL,
                                &pp->tonemap_desc_pool) != VK_SUCCESS) {
        return 0;
    }

    VkDescriptorSetAllocateInfo alloc_info;
    memset(&alloc_info, 0, sizeof(alloc_info));
    alloc_info.sType              = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
    alloc_info.descriptorPool     = pp->tonemap_desc_pool;
    alloc_info.descriptorSetCount = 1;
    alloc_info.pSetLayouts        = &pp->tonemap_set_layout;

    if (vkAllocateDescriptorSets(pp->device, &alloc_info,
                                  &pp->tonemap_desc_set) != VK_SUCCESS) {
        return 0;
    }

    /* Write: binding 0 = HDR scene, binding 1 = bloom mip 0 */
    VkDescriptorImageInfo imgs[2];
    memset(imgs, 0, sizeof(imgs));
    imgs[0].imageView   = pp->hdr_view;
    imgs[0].sampler     = pp->hdr_sampler;
    imgs[0].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
    imgs[1].imageView   = pp->bloom_mip_views[0];
    imgs[1].sampler     = pp->bloom_sampler;
    imgs[1].imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;

    VkWriteDescriptorSet writes[2];
    memset(writes, 0, sizeof(writes));
    for (uint32_t i = 0; i < 2; i++) {
        writes[i].sType           = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        writes[i].dstSet          = pp->tonemap_desc_set;
        writes[i].dstBinding      = i;
        writes[i].descriptorType  = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        writes[i].descriptorCount = 1;
        writes[i].pImageInfo      = &imgs[i];
    }

    vkUpdateDescriptorSets(pp->device, 2, writes, 0, NULL);
    return 1;
}

/* ----------------------------------------------------------------
 * Destroy helpers
 * ---------------------------------------------------------------- */

static void destroy_size_dependent(NvPostProcess *pp) {
    /* Tonemap framebuffers */
    if (pp->tonemap_framebuffers) {
        for (uint32_t i = 0; i < pp->tonemap_fb_count; i++) {
            if (pp->tonemap_framebuffers[i] != VK_NULL_HANDLE)
                vkDestroyFramebuffer(pp->device,
                                      pp->tonemap_framebuffers[i], NULL);
        }
        free(pp->tonemap_framebuffers);
        pp->tonemap_framebuffers = NULL;
        pp->tonemap_fb_count     = 0;
    }

    /* Bloom framebuffers */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        if (pp->bloom_fbs[i] != VK_NULL_HANDLE) {
            vkDestroyFramebuffer(pp->device, pp->bloom_fbs[i], NULL);
            pp->bloom_fbs[i] = VK_NULL_HANDLE;
        }
    }

    /* HDR framebuffer */
    if (pp->hdr_framebuffer != VK_NULL_HANDLE) {
        vkDestroyFramebuffer(pp->device, pp->hdr_framebuffer, NULL);
        pp->hdr_framebuffer = VK_NULL_HANDLE;
    }

    /* Descriptor pools (frees all sets) */
    if (pp->tonemap_desc_pool != VK_NULL_HANDLE) {
        vkDestroyDescriptorPool(pp->device, pp->tonemap_desc_pool, NULL);
        pp->tonemap_desc_pool = VK_NULL_HANDLE;
    }
    if (pp->bloom_desc_pool != VK_NULL_HANDLE) {
        vkDestroyDescriptorPool(pp->device, pp->bloom_desc_pool, NULL);
        pp->bloom_desc_pool = VK_NULL_HANDLE;
    }

    /* Bloom image + views */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        if (pp->bloom_mip_views[i] != VK_NULL_HANDLE) {
            vkDestroyImageView(pp->device, pp->bloom_mip_views[i], NULL);
            pp->bloom_mip_views[i] = VK_NULL_HANDLE;
        }
    }
    if (pp->bloom_sampler != VK_NULL_HANDLE) {
        vkDestroySampler(pp->device, pp->bloom_sampler, NULL);
        pp->bloom_sampler = VK_NULL_HANDLE;
    }
    if (pp->bloom_image != VK_NULL_HANDLE) {
        nv_vma_destroy_image(pp->vma, pp->bloom_image,
                              pp->bloom_allocation);
        pp->bloom_image = VK_NULL_HANDLE;
    }

    /* HDR image + view */
    if (pp->hdr_view != VK_NULL_HANDLE) {
        vkDestroyImageView(pp->device, pp->hdr_view, NULL);
        pp->hdr_view = VK_NULL_HANDLE;
    }
    if (pp->hdr_sampler != VK_NULL_HANDLE) {
        vkDestroySampler(pp->device, pp->hdr_sampler, NULL);
        pp->hdr_sampler = VK_NULL_HANDLE;
    }
    if (pp->hdr_image != VK_NULL_HANDLE) {
        nv_vma_destroy_image(pp->vma, pp->hdr_image,
                              pp->hdr_allocation);
        pp->hdr_image = VK_NULL_HANDLE;
    }
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvPostProcess *nv_postprocess_create(NvDevice *dev,
                                      NvAllocator *alloc,
                                      NvSwapchain *sc,
                                      const char *fullscreen_vert,
                                      const char *bloom_down_frag,
                                      const char *bloom_up_frag,
                                      const char *tonemap_frag) {
    if (!dev || !alloc || !sc) return NULL;

    NvPostProcess *pp = calloc(1, sizeof(NvPostProcess));
    if (!pp) return NULL;

    pp->device           = dev->handle;
    pp->width            = sc->extent.width;
    pp->height           = sc->extent.height;
    pp->hdr_format       = VK_FORMAT_R16G16B16A16_SFLOAT;
    pp->depth_format     = sc->depth_format;
    pp->swapchain_format = sc->image_format;

    /* Load shaders */
    VkShaderModule vert_mod     = load_shader(pp->device, fullscreen_vert);
    VkShaderModule down_mod     = load_shader(pp->device, bloom_down_frag);
    VkShaderModule up_mod       = load_shader(pp->device, bloom_up_frag);
    VkShaderModule tonemap_mod  = load_shader(pp->device, tonemap_frag);

    if (!vert_mod || !down_mod || !up_mod || !tonemap_mod) goto fail_shaders;

    /* Create resources */
    if (!create_hdr_image(pp, alloc)) goto fail_shaders;
    if (!create_bloom_image(pp, alloc)) goto fail_shaders;
    if (!create_hdr_render_pass(pp)) goto fail_shaders;
    if (!create_bloom_pass(pp->device, pp->hdr_format,
                            VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                            VK_IMAGE_LAYOUT_UNDEFINED,
                            &pp->bloom_down_pass)) goto fail_shaders;
    if (!create_bloom_pass(pp->device, pp->hdr_format,
                            VK_ATTACHMENT_LOAD_OP_LOAD,
                            VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                            &pp->bloom_up_pass)) goto fail_shaders;
    if (!create_tonemap_render_pass(pp)) goto fail_shaders;
    if (!create_hdr_framebuffer(pp, sc)) goto fail_shaders;
    if (!create_bloom_framebuffers(pp)) goto fail_shaders;
    if (!create_tonemap_framebuffers(pp, sc)) goto fail_shaders;
    if (!create_bloom_pipelines(pp, vert_mod, down_mod, up_mod)) goto fail_shaders;
    if (!create_tonemap_pipeline(pp, vert_mod, tonemap_mod)) goto fail_shaders;
    if (!create_bloom_descriptors(pp)) goto fail_shaders;
    if (!create_tonemap_descriptors(pp)) goto fail_shaders;

    /* Clean up shader modules */
    vkDestroyShaderModule(pp->device, vert_mod, NULL);
    vkDestroyShaderModule(pp->device, down_mod, NULL);
    vkDestroyShaderModule(pp->device, up_mod, NULL);
    vkDestroyShaderModule(pp->device, tonemap_mod, NULL);
    return pp;

fail_shaders:
    if (vert_mod)    vkDestroyShaderModule(pp->device, vert_mod, NULL);
    if (down_mod)    vkDestroyShaderModule(pp->device, down_mod, NULL);
    if (up_mod)      vkDestroyShaderModule(pp->device, up_mod, NULL);
    if (tonemap_mod) vkDestroyShaderModule(pp->device, tonemap_mod, NULL);
    nv_postprocess_destroy(pp);
    return NULL;
}

void nv_postprocess_destroy(NvPostProcess *pp) {
    if (!pp) return;

    destroy_size_dependent(pp);

    /* Pipelines */
    if (pp->bloom_down_pipeline)
        vkDestroyPipeline(pp->device, pp->bloom_down_pipeline, NULL);
    if (pp->bloom_up_pipeline)
        vkDestroyPipeline(pp->device, pp->bloom_up_pipeline, NULL);
    if (pp->tonemap_pipeline)
        vkDestroyPipeline(pp->device, pp->tonemap_pipeline, NULL);

    /* Pipeline layouts */
    if (pp->bloom_pipe_layout)
        vkDestroyPipelineLayout(pp->device, pp->bloom_pipe_layout, NULL);
    if (pp->tonemap_pipe_layout)
        vkDestroyPipelineLayout(pp->device, pp->tonemap_pipe_layout, NULL);

    /* Descriptor set layouts */
    if (pp->bloom_set_layout)
        vkDestroyDescriptorSetLayout(pp->device, pp->bloom_set_layout, NULL);
    if (pp->tonemap_set_layout)
        vkDestroyDescriptorSetLayout(pp->device, pp->tonemap_set_layout, NULL);

    /* Render passes */
    if (pp->hdr_render_pass)
        vkDestroyRenderPass(pp->device, pp->hdr_render_pass, NULL);
    if (pp->bloom_down_pass)
        vkDestroyRenderPass(pp->device, pp->bloom_down_pass, NULL);
    if (pp->bloom_up_pass)
        vkDestroyRenderPass(pp->device, pp->bloom_up_pass, NULL);
    if (pp->tonemap_render_pass)
        vkDestroyRenderPass(pp->device, pp->tonemap_render_pass, NULL);

    free(pp);
}

int nv_postprocess_recreate(NvPostProcess *pp, NvSwapchain *sc) {
    if (!pp || !sc) return 0;

    vkDeviceWaitIdle(pp->device);
    destroy_size_dependent(pp);

    pp->width  = sc->extent.width;
    pp->height = sc->extent.height;

    /* Re-create with a temporary allocator reference. We stored
     * pp->vma during initial creation and reuse it here. */
    NvAllocator tmp_alloc;
    tmp_alloc.vma    = pp->vma;
    tmp_alloc.device = pp->device;

    if (!create_hdr_image(pp, &tmp_alloc)) return 0;
    if (!create_bloom_image(pp, &tmp_alloc)) return 0;
    if (!create_hdr_framebuffer(pp, sc)) return 0;
    if (!create_bloom_framebuffers(pp)) return 0;
    if (!create_tonemap_framebuffers(pp, sc)) return 0;
    if (!create_bloom_descriptors(pp)) return 0;
    if (!create_tonemap_descriptors(pp)) return 0;

    return 1;
}

/* ----------------------------------------------------------------
 * Pass management
 * ---------------------------------------------------------------- */

static void set_viewport_scissor(VkCommandBuffer cmd,
                                  uint32_t w, uint32_t h) {
    VkViewport vp;
    memset(&vp, 0, sizeof(vp));
    vp.width    = (float)w;
    vp.height   = (float)h;
    vp.maxDepth = 1.0f;
    vkCmdSetViewport(cmd, 0, 1, &vp);

    VkRect2D scissor;
    memset(&scissor, 0, sizeof(scissor));
    scissor.extent.width  = w;
    scissor.extent.height = h;
    vkCmdSetScissor(cmd, 0, 1, &scissor);
}

void nv_postprocess_begin_hdr_pass(NvPostProcess *pp, NvFrame *fr,
                                    NvSwapchain *sc, NvPipeline *pip) {
    VkCommandBuffer cmd = fr->cmd[fr->current_frame];

    VkClearValue clears[2];
    memset(clears, 0, sizeof(clears));
    clears[0].color.float32[0] = 0.0f;
    clears[0].color.float32[1] = 0.0f;
    clears[0].color.float32[2] = 0.0f;
    clears[0].color.float32[3] = 1.0f;
    clears[1].depthStencil.depth   = 1.0f;
    clears[1].depthStencil.stencil = 0;

    VkRenderPassBeginInfo rp;
    memset(&rp, 0, sizeof(rp));
    rp.sType             = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
    rp.renderPass        = pp->hdr_render_pass;
    rp.framebuffer       = pp->hdr_framebuffer;
    rp.renderArea.extent = sc->extent;
    rp.clearValueCount   = 2;
    rp.pClearValues      = clears;

    vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);
    vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS, pip->handle);
    set_viewport_scissor(cmd, sc->extent.width, sc->extent.height);
}

void nv_postprocess_end_hdr_pass(NvFrame *fr) {
    vkCmdEndRenderPass(fr->cmd[fr->current_frame]);
}

void nv_postprocess_record(NvPostProcess *pp, NvFrame *fr,
                            NvSwapchain *sc) {
    VkCommandBuffer cmd = fr->cmd[fr->current_frame];

    /* ---- Bloom downsample (5 passes: mip 0..4) ---- */
    for (uint32_t i = 0; i < NV_BLOOM_MIP_COUNT; i++) {
        uint32_t w = mip_dim(pp->width, i);
        uint32_t h = mip_dim(pp->height, i);

        VkClearValue clear;
        memset(&clear, 0, sizeof(clear));

        VkRenderPassBeginInfo rp;
        memset(&rp, 0, sizeof(rp));
        rp.sType                    = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        rp.renderPass               = pp->bloom_down_pass;
        rp.framebuffer              = pp->bloom_fbs[i];
        rp.renderArea.extent.width  = w;
        rp.renderArea.extent.height = h;
        rp.clearValueCount          = 1;
        rp.pClearValues             = &clear;

        vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                           pp->bloom_down_pipeline);
        set_viewport_scissor(cmd, w, h);

        vkCmdBindDescriptorSets(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                                 pp->bloom_pipe_layout, 0, 1,
                                 &pp->bloom_down_sets[i], 0, NULL);

        /* Push: texelSize of SOURCE, currentMip, pad */
        uint32_t src_w = (i == 0) ? pp->width : mip_dim(pp->width, i - 1);
        uint32_t src_h = (i == 0) ? pp->height : mip_dim(pp->height, i - 1);
        float push_data[4] = {
            1.0f / (float)src_w,
            1.0f / (float)src_h,
            (float)i,
            0.0f
        };
        vkCmdPushConstants(cmd, pp->bloom_pipe_layout,
                            VK_SHADER_STAGE_FRAGMENT_BIT, 0, 16, push_data);

        vkCmdDraw(cmd, 3, 1, 0, 0);
        vkCmdEndRenderPass(cmd);
    }

    /* ---- Bloom upsample (4 passes: mip 3..0) ---- */
    for (uint32_t pass = 0; pass < NV_BLOOM_MIP_COUNT - 1; pass++) {
        uint32_t target_mip = NV_BLOOM_MIP_COUNT - 2 - pass;
        uint32_t w = mip_dim(pp->width, target_mip);
        uint32_t h = mip_dim(pp->height, target_mip);

        VkRenderPassBeginInfo rp;
        memset(&rp, 0, sizeof(rp));
        rp.sType                    = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        rp.renderPass               = pp->bloom_up_pass;
        rp.framebuffer              = pp->bloom_fbs[target_mip];
        rp.renderArea.extent.width  = w;
        rp.renderArea.extent.height = h;

        vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                           pp->bloom_up_pipeline);
        set_viewport_scissor(cmd, w, h);

        vkCmdBindDescriptorSets(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                                 pp->bloom_pipe_layout, 0, 1,
                                 &pp->bloom_up_sets[pass], 0, NULL);

        /* Push: texelSize of SOURCE (smaller mip), currentMip, pad */
        uint32_t src_mip = target_mip + 1;
        uint32_t src_w = mip_dim(pp->width, src_mip);
        uint32_t src_h = mip_dim(pp->height, src_mip);
        float push_data[4] = {
            1.0f / (float)src_w,
            1.0f / (float)src_h,
            (float)target_mip,
            0.0f
        };
        vkCmdPushConstants(cmd, pp->bloom_pipe_layout,
                            VK_SHADER_STAGE_FRAGMENT_BIT, 0, 16, push_data);

        vkCmdDraw(cmd, 3, 1, 0, 0);
        vkCmdEndRenderPass(cmd);
    }

    /* ---- Tonemap + FXAA (1 pass to swapchain) ---- */
    {
        VkClearValue clear;
        memset(&clear, 0, sizeof(clear));

        VkRenderPassBeginInfo rp;
        memset(&rp, 0, sizeof(rp));
        rp.sType             = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        rp.renderPass        = pp->tonemap_render_pass;
        rp.framebuffer       = pp->tonemap_framebuffers[fr->image_index];
        rp.renderArea.extent = sc->extent;
        rp.clearValueCount   = 1;
        rp.pClearValues      = &clear;

        vkCmdBeginRenderPass(cmd, &rp, VK_SUBPASS_CONTENTS_INLINE);
        vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                           pp->tonemap_pipeline);
        set_viewport_scissor(cmd, sc->extent.width, sc->extent.height);

        vkCmdBindDescriptorSets(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                                 pp->tonemap_pipe_layout, 0, 1,
                                 &pp->tonemap_desc_set, 0, NULL);

        float push_data[4] = {
            1.0f / (float)sc->extent.width,
            1.0f / (float)sc->extent.height,
            0.04f,  /* bloomStrength */
            0.0f
        };
        vkCmdPushConstants(cmd, pp->tonemap_pipe_layout,
                            VK_SHADER_STAGE_FRAGMENT_BIT, 0, 16, push_data);

        vkCmdDraw(cmd, 3, 1, 0, 0);
        vkCmdEndRenderPass(cmd);
    }
}

VkRenderPass nv_postprocess_hdr_render_pass(const NvPostProcess *pp) {
    return pp ? pp->hdr_render_pass : VK_NULL_HANDLE;
}
