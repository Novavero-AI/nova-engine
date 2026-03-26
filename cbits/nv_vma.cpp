/*
 * nv_vma — VMA implementation + C-linkage wrappers.
 *
 * This is the only C++ file in the project.  It compiles the VMA
 * implementation and provides C-callable wrapper functions so that
 * all other engine code remains pure C99.
 */

#define VMA_IMPLEMENTATION
#define VMA_STATIC_VULKAN_FUNCTIONS  0
#define VMA_DYNAMIC_VULKAN_FUNCTIONS 0

#include "vk_mem_alloc.h"
#include "nv_vma.h"

/* ----------------------------------------------------------------
 * Allocator lifecycle
 * ---------------------------------------------------------------- */

extern "C" NvVmaAllocator
nv_vma_create(VkInstance instance, VkPhysicalDevice physical_device,
              VkDevice device, uint32_t vulkan_api_version) {
    VmaVulkanFunctions funcs = {};
    funcs.vkGetInstanceProcAddr = vkGetInstanceProcAddr;
    funcs.vkGetDeviceProcAddr   = vkGetDeviceProcAddr;

    VmaAllocatorCreateInfo info = {};
    info.vulkanApiVersion = vulkan_api_version;
    info.instance         = instance;
    info.physicalDevice   = physical_device;
    info.device           = device;
    info.pVulkanFunctions = &funcs;

    VmaAllocator allocator = VK_NULL_HANDLE;
    if (vmaCreateAllocator(&info, &allocator) != VK_SUCCESS) {
        return nullptr;
    }
    return static_cast<NvVmaAllocator>(allocator);
}

extern "C" void nv_vma_destroy(NvVmaAllocator allocator) {
    if (allocator) {
        vmaDestroyAllocator(static_cast<VmaAllocator>(allocator));
    }
}

/* ----------------------------------------------------------------
 * Buffer operations
 * ---------------------------------------------------------------- */

extern "C" int
nv_vma_create_buffer(NvVmaAllocator allocator, VkDeviceSize size,
                     VkBufferUsageFlags usage, int gpu_only,
                     VkBuffer *out_buffer,
                     NvVmaAllocation *out_allocation) {
    VkBufferCreateInfo buf_info = {};
    buf_info.sType       = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
    buf_info.size        = size;
    buf_info.usage       = usage;
    buf_info.sharingMode = VK_SHARING_MODE_EXCLUSIVE;

    VmaAllocationCreateInfo alloc_info = {};
    alloc_info.usage = VMA_MEMORY_USAGE_AUTO;
    if (gpu_only) {
        alloc_info.flags =
            VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT;
        alloc_info.requiredFlags =
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
    } else {
        alloc_info.flags =
            VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
            | VMA_ALLOCATION_CREATE_MAPPED_BIT;
    }

    VmaAllocation allocation = VK_NULL_HANDLE;
    VkResult result = vmaCreateBuffer(
        static_cast<VmaAllocator>(allocator), &buf_info,
        &alloc_info, out_buffer, &allocation, nullptr);

    *out_allocation = static_cast<NvVmaAllocation>(allocation);
    return result == VK_SUCCESS ? 1 : 0;
}

extern "C" void
nv_vma_destroy_buffer(NvVmaAllocator allocator, VkBuffer buffer,
                      NvVmaAllocation allocation) {
    if (allocator && buffer != VK_NULL_HANDLE) {
        vmaDestroyBuffer(static_cast<VmaAllocator>(allocator),
                         buffer,
                         static_cast<VmaAllocation>(allocation));
    }
}

/* ----------------------------------------------------------------
 * Image operations
 * ---------------------------------------------------------------- */

extern "C" int
nv_vma_create_image(NvVmaAllocator allocator,
                    const VkImageCreateInfo *image_info,
                    VkImage *out_image,
                    NvVmaAllocation *out_allocation) {
    VmaAllocationCreateInfo alloc_info = {};
    alloc_info.usage         = VMA_MEMORY_USAGE_AUTO;
    alloc_info.requiredFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;

    VmaAllocation allocation = VK_NULL_HANDLE;
    VkResult result = vmaCreateImage(
        static_cast<VmaAllocator>(allocator), image_info,
        &alloc_info, out_image, &allocation, nullptr);

    *out_allocation = static_cast<NvVmaAllocation>(allocation);
    return result == VK_SUCCESS ? 1 : 0;
}

extern "C" void
nv_vma_destroy_image(NvVmaAllocator allocator, VkImage image,
                     NvVmaAllocation allocation) {
    if (allocator && image != VK_NULL_HANDLE) {
        vmaDestroyImage(static_cast<VmaAllocator>(allocator), image,
                        static_cast<VmaAllocation>(allocation));
    }
}

/* ----------------------------------------------------------------
 * Mapping
 * ---------------------------------------------------------------- */

extern "C" void *nv_vma_map(NvVmaAllocator allocator,
                             NvVmaAllocation allocation) {
    void *mapped = nullptr;
    if (vmaMapMemory(static_cast<VmaAllocator>(allocator),
                     static_cast<VmaAllocation>(allocation),
                     &mapped)
        != VK_SUCCESS) {
        return nullptr;
    }
    return mapped;
}

extern "C" void nv_vma_unmap(NvVmaAllocator allocator,
                              NvVmaAllocation allocation) {
    vmaUnmapMemory(static_cast<VmaAllocator>(allocator),
                   static_cast<VmaAllocation>(allocation));
}
