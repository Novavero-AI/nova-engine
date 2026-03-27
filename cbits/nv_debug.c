/*
 * nv_debug — Debug line rendering and GPU timestamp profiling.
 */

#include "nv_debug.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nv_vma.h"

/* ----------------------------------------------------------------
 * Shader loading
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
 * Public API — Lifecycle
 * ---------------------------------------------------------------- */

NvDebug *nv_debug_create(NvDevice *dev, NvAllocator *alloc,
                          VkRenderPass render_pass,
                          const char *vert_path,
                          const char *frag_path) {
    if (!dev || !alloc || !vert_path || !frag_path
        || render_pass == VK_NULL_HANDLE) {
        return NULL;
    }

    NvDebug *dbg = calloc(1, sizeof(NvDebug));
    if (!dbg) return NULL;
    dbg->device = dev->handle;

    /* ---- Host-visible line buffer ---- */
    VkBuffer buf = VK_NULL_HANDLE;
    NvVmaAllocation buf_alloc = NULL;
    if (!nv_vma_create_buffer(alloc->vma, NV_DEBUG_BUFFER_SIZE,
                               VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
                               0, &buf, &buf_alloc)) {
        goto fail;
    }
    dbg->line_buffer     = buf;
    dbg->line_allocation = buf_alloc;
    dbg->vma             = alloc->vma;

    /* ---- Pipeline ---- */
    VkShaderModule vert = load_shader(dbg->device, vert_path);
    VkShaderModule frag = load_shader(dbg->device, frag_path);
    if (!vert || !frag) {
        if (vert) vkDestroyShaderModule(dbg->device, vert, NULL);
        if (frag) vkDestroyShaderModule(dbg->device, frag, NULL);
        goto fail;
    }

    /* Push constant: mat4 viewProjection (64 bytes) */
    VkPushConstantRange push;
    memset(&push, 0, sizeof(push));
    push.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
    push.size       = 64;

    VkPipelineLayoutCreateInfo pli;
    memset(&pli, 0, sizeof(pli));
    pli.sType                  = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    pli.pushConstantRangeCount = 1;
    pli.pPushConstantRanges    = &push;

    if (vkCreatePipelineLayout(dbg->device, &pli, NULL,
                                &dbg->line_layout) != VK_SUCCESS) {
        vkDestroyShaderModule(dbg->device, vert, NULL);
        vkDestroyShaderModule(dbg->device, frag, NULL);
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

    /* Vertex input: V3 pos (12) + V4 color (16) = 28 bytes */
    VkVertexInputBindingDescription binding;
    memset(&binding, 0, sizeof(binding));
    binding.stride    = NV_DEBUG_BYTES_PER_VERT;
    binding.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

    VkVertexInputAttributeDescription attrs[2];
    memset(attrs, 0, sizeof(attrs));
    attrs[0].location = 0;
    attrs[0].format   = VK_FORMAT_R32G32B32_SFLOAT;
    attrs[0].offset   = 0;
    attrs[1].location = 1;
    attrs[1].format   = VK_FORMAT_R32G32B32A32_SFLOAT;
    attrs[1].offset   = 12;

    VkPipelineVertexInputStateCreateInfo vi;
    memset(&vi, 0, sizeof(vi));
    vi.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vi.vertexBindingDescriptionCount   = 1;
    vi.pVertexBindingDescriptions      = &binding;
    vi.vertexAttributeDescriptionCount = 2;
    vi.pVertexAttributeDescriptions    = attrs;

    VkPipelineInputAssemblyStateCreateInfo ia;
    memset(&ia, 0, sizeof(ia));
    ia.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    ia.topology = VK_PRIMITIVE_TOPOLOGY_LINE_LIST;

    VkDynamicState dyn[] = {VK_DYNAMIC_STATE_VIEWPORT,
                             VK_DYNAMIC_STATE_SCISSOR};
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
    raster.lineWidth   = 1.0f;

    VkPipelineMultisampleStateCreateInfo msaa;
    memset(&msaa, 0, sizeof(msaa));
    msaa.sType                = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    msaa.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;

    VkPipelineDepthStencilStateCreateInfo depth;
    memset(&depth, 0, sizeof(depth));
    depth.sType            = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
    depth.depthTestEnable  = VK_TRUE;
    depth.depthWriteEnable = VK_FALSE; /* lines on top of geometry */
    depth.depthCompareOp   = VK_COMPARE_OP_LESS_OR_EQUAL;

    VkPipelineColorBlendAttachmentState blend_attach;
    memset(&blend_attach, 0, sizeof(blend_attach));
    blend_attach.blendEnable         = VK_TRUE;
    blend_attach.srcColorBlendFactor = VK_BLEND_FACTOR_SRC_ALPHA;
    blend_attach.dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
    blend_attach.colorBlendOp        = VK_BLEND_OP_ADD;
    blend_attach.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
    blend_attach.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
    blend_attach.alphaBlendOp        = VK_BLEND_OP_ADD;
    blend_attach.colorWriteMask      = VK_COLOR_COMPONENT_R_BIT
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
    ci.pInputAssemblyState = &ia;
    ci.pViewportState      = &vp;
    ci.pRasterizationState = &raster;
    ci.pMultisampleState   = &msaa;
    ci.pDepthStencilState  = &depth;
    ci.pColorBlendState    = &blend;
    ci.pDynamicState       = &dyn_state;
    ci.layout              = dbg->line_layout;
    ci.renderPass          = render_pass;
    ci.subpass             = 0;

    VkResult result = vkCreateGraphicsPipelines(
        dbg->device, VK_NULL_HANDLE, 1, &ci, NULL, &dbg->line_pipeline);

    vkDestroyShaderModule(dbg->device, vert, NULL);
    vkDestroyShaderModule(dbg->device, frag, NULL);
    if (result != VK_SUCCESS) goto fail;

    /* ---- Timestamp query pool ---- */
    VkQueryPoolCreateInfo qp;
    memset(&qp, 0, sizeof(qp));
    qp.sType      = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO;
    qp.queryType  = VK_QUERY_TYPE_TIMESTAMP;
    qp.queryCount = NV_DEBUG_MAX_TIMESTAMPS;

    if (vkCreateQueryPool(dbg->device, &qp, NULL,
                           &dbg->timestamp_pool) != VK_SUCCESS) {
        dbg->timestamp_pool = VK_NULL_HANDLE;
    }

    /* Default timestamp period; caller can override if needed.
     * The actual value comes from VkPhysicalDeviceProperties. */
    dbg->timestamp_period = 1.0f;

    return dbg;

fail:
    nv_debug_destroy(dbg);
    return NULL;
}

void nv_debug_destroy(NvDebug *dbg) {
    if (!dbg) return;
    if (dbg->timestamp_pool != VK_NULL_HANDLE)
        vkDestroyQueryPool(dbg->device, dbg->timestamp_pool, NULL);
    if (dbg->line_pipeline != VK_NULL_HANDLE)
        vkDestroyPipeline(dbg->device, dbg->line_pipeline, NULL);
    if (dbg->line_layout != VK_NULL_HANDLE)
        vkDestroyPipelineLayout(dbg->device, dbg->line_layout, NULL);
    if (dbg->line_buffer != VK_NULL_HANDLE)
        nv_vma_destroy_buffer(dbg->vma, dbg->line_buffer,
                               dbg->line_allocation);
    free(dbg);
}

/* ----------------------------------------------------------------
 * Line drawing
 * ---------------------------------------------------------------- */

void nv_debug_line(NvDebug *dbg,
                    float x0, float y0, float z0,
                    float x1, float y1, float z1,
                    float r, float g, float b, float a) {
    if (!dbg || dbg->line_count >= NV_DEBUG_MAX_LINES) return;

    uint32_t base = dbg->line_count * 2;
    dbg->line_verts[base].px = x0;
    dbg->line_verts[base].py = y0;
    dbg->line_verts[base].pz = z0;
    dbg->line_verts[base].cr = r;
    dbg->line_verts[base].cg = g;
    dbg->line_verts[base].cb = b;
    dbg->line_verts[base].ca = a;

    dbg->line_verts[base + 1].px = x1;
    dbg->line_verts[base + 1].py = y1;
    dbg->line_verts[base + 1].pz = z1;
    dbg->line_verts[base + 1].cr = r;
    dbg->line_verts[base + 1].cg = g;
    dbg->line_verts[base + 1].cb = b;
    dbg->line_verts[base + 1].ca = a;

    dbg->line_count++;
}

void nv_debug_box(NvDebug *dbg,
                   float cx, float cy, float cz,
                   float hx, float hy, float hz,
                   float r, float g, float b, float a) {
    float x0 = cx - hx, x1 = cx + hx;
    float y0 = cy - hy, y1 = cy + hy;
    float z0 = cz - hz, z1 = cz + hz;

    /* Bottom face */
    nv_debug_line(dbg, x0,y0,z0, x1,y0,z0, r,g,b,a);
    nv_debug_line(dbg, x1,y0,z0, x1,y0,z1, r,g,b,a);
    nv_debug_line(dbg, x1,y0,z1, x0,y0,z1, r,g,b,a);
    nv_debug_line(dbg, x0,y0,z1, x0,y0,z0, r,g,b,a);
    /* Top face */
    nv_debug_line(dbg, x0,y1,z0, x1,y1,z0, r,g,b,a);
    nv_debug_line(dbg, x1,y1,z0, x1,y1,z1, r,g,b,a);
    nv_debug_line(dbg, x1,y1,z1, x0,y1,z1, r,g,b,a);
    nv_debug_line(dbg, x0,y1,z1, x0,y1,z0, r,g,b,a);
    /* Verticals */
    nv_debug_line(dbg, x0,y0,z0, x0,y1,z0, r,g,b,a);
    nv_debug_line(dbg, x1,y0,z0, x1,y1,z0, r,g,b,a);
    nv_debug_line(dbg, x1,y0,z1, x1,y1,z1, r,g,b,a);
    nv_debug_line(dbg, x0,y0,z1, x0,y1,z1, r,g,b,a);
}

void nv_debug_sphere(NvDebug *dbg,
                      float cx, float cy, float cz, float radius,
                      int segments,
                      float r, float g, float b, float a) {
    if (segments < 4) segments = 4;
    float step = 2.0f * 3.14159265359f / (float)segments;
    for (int i = 0; i < segments; i++) {
        float a0 = (float)i * step;
        float a1 = (float)(i + 1) * step;
        float c0 = cosf(a0), s0 = sinf(a0);
        float c1 = cosf(a1), s1 = sinf(a1);
        /* XY circle */
        nv_debug_line(dbg, cx+radius*c0, cy+radius*s0, cz,
                            cx+radius*c1, cy+radius*s1, cz, r,g,b,a);
        /* XZ circle */
        nv_debug_line(dbg, cx+radius*c0, cy, cz+radius*s0,
                            cx+radius*c1, cy, cz+radius*s1, r,g,b,a);
        /* YZ circle */
        nv_debug_line(dbg, cx, cy+radius*c0, cz+radius*s0,
                            cx, cy+radius*c1, cz+radius*s1, r,g,b,a);
    }
}

void nv_debug_flush(NvDebug *dbg, NvFrame *fr,
                     const float *view_projection) {
    if (!dbg || !fr || dbg->line_count == 0) return;

    VkCommandBuffer cmd = fr->cmd[fr->current_frame];
    uint32_t vert_count = dbg->line_count * 2;
    uint32_t byte_count = vert_count * NV_DEBUG_BYTES_PER_VERT;

    /* Upload line data */
    void *mapped = nv_vma_map(dbg->vma, dbg->line_allocation);
    if (mapped) {
        memcpy(mapped, dbg->line_verts, byte_count);
        nv_vma_unmap(dbg->vma, dbg->line_allocation);
    }

    /* Draw */
    vkCmdBindPipeline(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                       dbg->line_pipeline);
    vkCmdPushConstants(cmd, dbg->line_layout,
                        VK_SHADER_STAGE_VERTEX_BIT,
                        0, 64, view_projection);

    VkBuffer     buffers[] = {dbg->line_buffer};
    VkDeviceSize offsets[] = {0};
    vkCmdBindVertexBuffers(cmd, 0, 1, buffers, offsets);
    vkCmdDraw(cmd, vert_count, 1, 0, 0);

    /* Reset for next frame */
    dbg->line_count = 0;
}

/* ----------------------------------------------------------------
 * GPU timestamps
 * ---------------------------------------------------------------- */

void nv_debug_timestamp_reset(NvDebug *dbg, NvFrame *fr) {
    if (!dbg || !fr || dbg->timestamp_pool == VK_NULL_HANDLE) return;
    VkCommandBuffer cmd = fr->cmd[fr->current_frame];
    vkCmdResetQueryPool(cmd, dbg->timestamp_pool, 0,
                         NV_DEBUG_MAX_TIMESTAMPS);
    dbg->timestamp_count = 0;
}

int nv_debug_timestamp_write(NvDebug *dbg, NvFrame *fr) {
    if (!dbg || !fr || dbg->timestamp_pool == VK_NULL_HANDLE) return -1;
    if (dbg->timestamp_count >= NV_DEBUG_MAX_TIMESTAMPS) return -1;

    VkCommandBuffer cmd = fr->cmd[fr->current_frame];
    uint32_t idx = dbg->timestamp_count++;
    vkCmdWriteTimestamp(cmd, VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
                         dbg->timestamp_pool, idx);
    return (int)idx;
}

int nv_debug_timestamp_read(NvDebug *dbg, float *results) {
    if (!dbg || !results || dbg->timestamp_pool == VK_NULL_HANDLE)
        return 0;

    uint64_t raw[NV_DEBUG_MAX_TIMESTAMPS];
    memset(raw, 0, sizeof(raw));

    uint32_t count = dbg->timestamp_count;
    if (count == 0) return 0;

    VkResult res = vkGetQueryPoolResults(
        dbg->device, dbg->timestamp_pool, 0, count,
        count * sizeof(uint64_t), raw, sizeof(uint64_t),
        VK_QUERY_RESULT_64_BIT);

    if (res != VK_SUCCESS && res != VK_NOT_READY) return 0;

    for (uint32_t i = 0; i < count; i++) {
        results[i] = (float)raw[i] * dbg->timestamp_period;
    }
    return (int)count;
}
