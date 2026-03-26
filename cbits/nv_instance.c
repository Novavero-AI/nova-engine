/*
 * nv_instance — Vulkan instance, surface, and physical device.
 */

#include "nv_instance.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NV_MAX_INSTANCE_EXTENSIONS 16
#define NV_MAX_PHYSICAL_DEVICES    8
#define NV_MAX_QUEUE_FAMILIES     16

/* ----------------------------------------------------------------
 * Debug messenger
 * ---------------------------------------------------------------- */

static VKAPI_ATTR VkBool32 VKAPI_CALL debug_callback(
    VkDebugUtilsMessageSeverityFlagBitsEXT severity,
    VkDebugUtilsMessageTypeFlagsEXT type,
    const VkDebugUtilsMessengerCallbackDataEXT *data,
    void *user_data) {
    (void)type;
    (void)user_data;

    const char *level = "INFO";
    if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT) {
        level = "ERROR";
    } else if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT) {
        level = "WARN";
    }

    fprintf(stderr, "[vulkan %s] %s\n", level, data->pMessage);
    return VK_FALSE;
}

static void setup_debug_messenger(NvInstance *inst) {
    VkDebugUtilsMessengerCreateInfoEXT info;
    memset(&info, 0, sizeof(info));
    info.sType           = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT;
    info.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                         | VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
    info.messageType     = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                         | VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                         | VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT;
    info.pfnUserCallback = debug_callback;

    PFN_vkCreateDebugUtilsMessengerEXT func =
        (PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(
            inst->handle, "vkCreateDebugUtilsMessengerEXT");

    if (func) {
        func(inst->handle, &info, NULL, &inst->debug_messenger);
    }
}

static void destroy_debug_messenger(NvInstance *inst) {
    if (inst->debug_messenger == VK_NULL_HANDLE) {
        return;
    }
    PFN_vkDestroyDebugUtilsMessengerEXT func =
        (PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(
            inst->handle, "vkDestroyDebugUtilsMessengerEXT");
    if (func) {
        func(inst->handle, inst->debug_messenger, NULL);
    }
}

/* ----------------------------------------------------------------
 * Extension / layer checks
 * ---------------------------------------------------------------- */

static int check_instance_extension(const char *name) {
    uint32_t count = 0;
    vkEnumerateInstanceExtensionProperties(NULL, &count, NULL);
    if (count == 0) {
        return 0;
    }

    VkExtensionProperties *props = malloc(count * sizeof(*props));
    if (!props) {
        return 0;
    }
    vkEnumerateInstanceExtensionProperties(NULL, &count, props);

    int found = 0;
    for (uint32_t i = 0; i < count; i++) {
        if (strcmp(props[i].extensionName, name) == 0) {
            found = 1;
            break;
        }
    }
    free(props);
    return found;
}

static int check_validation_support(void) {
    uint32_t count = 0;
    vkEnumerateInstanceLayerProperties(&count, NULL);
    if (count == 0) {
        return 0;
    }

    VkLayerProperties *layers = malloc(count * sizeof(*layers));
    if (!layers) {
        return 0;
    }
    vkEnumerateInstanceLayerProperties(&count, layers);

    int found = 0;
    for (uint32_t i = 0; i < count; i++) {
        if (strcmp(layers[i].layerName, "VK_LAYER_KHRONOS_validation") == 0) {
            found = 1;
            break;
        }
    }
    free(layers);
    return found;
}

static int check_device_extension(VkPhysicalDevice device,
                                  const char *name) {
    uint32_t count = 0;
    vkEnumerateDeviceExtensionProperties(device, NULL, &count, NULL);
    if (count == 0) {
        return 0;
    }

    VkExtensionProperties *props = malloc(count * sizeof(*props));
    if (!props) {
        return 0;
    }
    vkEnumerateDeviceExtensionProperties(device, NULL, &count, props);

    int found = 0;
    for (uint32_t i = 0; i < count; i++) {
        if (strcmp(props[i].extensionName, name) == 0) {
            found = 1;
            break;
        }
    }
    free(props);
    return found;
}

/* ----------------------------------------------------------------
 * Physical device selection
 * ---------------------------------------------------------------- */

static int find_queue_families(VkPhysicalDevice device,
                               VkSurfaceKHR surface,
                               uint32_t *out_graphics,
                               uint32_t *out_present) {
    uint32_t count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, NULL);
    if (count == 0) {
        return 0;
    }
    if (count > NV_MAX_QUEUE_FAMILIES) {
        count = NV_MAX_QUEUE_FAMILIES;
    }

    VkQueueFamilyProperties families[NV_MAX_QUEUE_FAMILIES];
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, families);

    uint32_t graphics = UINT32_MAX;
    uint32_t present  = UINT32_MAX;

    for (uint32_t i = 0; i < count; i++) {
        int is_graphics =
            (families[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) != 0;

        VkBool32 present_support = VK_FALSE;
        vkGetPhysicalDeviceSurfaceSupportKHR(
            device, i, surface, &present_support);

        /* Best case: same family supports both. */
        if (is_graphics && present_support) {
            *out_graphics = i;
            *out_present  = i;
            return 1;
        }

        if (is_graphics && graphics == UINT32_MAX) {
            graphics = i;
        }
        if (present_support && present == UINT32_MAX) {
            present = i;
        }
    }

    if (graphics == UINT32_MAX || present == UINT32_MAX) {
        return 0;
    }

    *out_graphics = graphics;
    *out_present  = present;
    return 1;
}

static int check_swapchain_adequate(VkPhysicalDevice device,
                                    VkSurfaceKHR surface) {
    uint32_t format_count = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(
        device, surface, &format_count, NULL);

    uint32_t present_count = 0;
    vkGetPhysicalDeviceSurfacePresentModesKHR(
        device, surface, &present_count, NULL);

    return format_count > 0 && present_count > 0;
}

static int select_physical_device(NvInstance *inst) {
    uint32_t count = 0;
    vkEnumeratePhysicalDevices(inst->handle, &count, NULL);
    if (count == 0) {
        return 0;
    }
    if (count > NV_MAX_PHYSICAL_DEVICES) {
        count = NV_MAX_PHYSICAL_DEVICES;
    }

    VkPhysicalDevice devices[NV_MAX_PHYSICAL_DEVICES];
    vkEnumeratePhysicalDevices(inst->handle, &count, devices);

    uint32_t         best_score       = 0;
    VkPhysicalDevice best_device      = VK_NULL_HANDLE;
    uint32_t         best_gfx         = 0;
    uint32_t         best_present     = 0;
    int              best_portability = 0;

    for (uint32_t i = 0; i < count; i++) {
        uint32_t gfx, prs;
        if (!find_queue_families(devices[i], inst->surface, &gfx, &prs)) {
            continue;
        }
        if (!check_device_extension(
                devices[i], VK_KHR_SWAPCHAIN_EXTENSION_NAME)) {
            continue;
        }
        if (!check_swapchain_adequate(devices[i], inst->surface)) {
            continue;
        }

        VkPhysicalDeviceProperties props;
        vkGetPhysicalDeviceProperties(devices[i], &props);

        uint32_t score = 1;
        switch (props.deviceType) {
        case VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU:
            score += 1000;
            break;
        case VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU:
            score += 100;
            break;
        case VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU:
            score += 50;
            break;
        default:
            break;
        }

        if (score > best_score) {
            best_score       = score;
            best_device      = devices[i];
            best_gfx         = gfx;
            best_present     = prs;
            best_portability = check_device_extension(
                devices[i], "VK_KHR_portability_subset");
        }
    }

    if (best_device == VK_NULL_HANDLE) {
        return 0;
    }

    inst->physical_device        = best_device;
    inst->graphics_family        = best_gfx;
    inst->present_family         = best_present;
    inst->has_portability_subset = best_portability;
    return 1;
}

/* ----------------------------------------------------------------
 * Public API
 * ---------------------------------------------------------------- */

NvInstance *nv_instance_create(NvWindow *window, const char *app_name,
                               int enable_validation) {
    if (!window) {
        return NULL;
    }

    NvInstance *inst = calloc(1, sizeof(NvInstance));
    if (!inst) {
        return NULL;
    }

    /* ---- Gather instance extensions ---- */
    uint32_t sdl_ext_count = 0;
    const char *const *sdl_exts = NULL;
    nv_window_vulkan_extensions(&sdl_ext_count, &sdl_exts);

    const char *extensions[NV_MAX_INSTANCE_EXTENSIONS];
    uint32_t ext_count = 0;

    for (uint32_t i = 0;
         i < sdl_ext_count && ext_count < NV_MAX_INSTANCE_EXTENSIONS;
         i++) {
        extensions[ext_count++] = sdl_exts[i];
    }

    int has_debug = 0;
    if (enable_validation
        && check_instance_extension(VK_EXT_DEBUG_UTILS_EXTENSION_NAME)
        && ext_count < NV_MAX_INSTANCE_EXTENSIONS) {
        extensions[ext_count++] = VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
        has_debug = 1;
    }

    int has_portability_enum = check_instance_extension(
        VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME);
    if (has_portability_enum
        && ext_count < NV_MAX_INSTANCE_EXTENSIONS) {
        extensions[ext_count++] =
            VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME;
    }

    /* ---- Validation layers ---- */
    const char *layers[1];
    uint32_t layer_count = 0;

    if (enable_validation && check_validation_support()) {
        layers[layer_count++] = "VK_LAYER_KHRONOS_validation";
    }

    /* ---- VkInstance ---- */
    VkApplicationInfo app_info;
    memset(&app_info, 0, sizeof(app_info));
    app_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pApplicationName   = app_name;
    app_info.applicationVersion = VK_MAKE_API_VERSION(0, 0, 2, 0);
    app_info.pEngineName        = "nova-engine";
    app_info.engineVersion      = VK_MAKE_API_VERSION(0, 0, 2, 0);
    app_info.apiVersion         = VK_API_VERSION_1_2;

    VkInstanceCreateInfo create_info;
    memset(&create_info, 0, sizeof(create_info));
    create_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    create_info.pApplicationInfo        = &app_info;
    create_info.enabledExtensionCount   = ext_count;
    create_info.ppEnabledExtensionNames = extensions;
    create_info.enabledLayerCount       = layer_count;
    create_info.ppEnabledLayerNames     = layers;
    if (has_portability_enum) {
        create_info.flags |=
            VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR;
    }

    if (vkCreateInstance(&create_info, NULL, &inst->handle)
        != VK_SUCCESS) {
        goto fail_alloc;
    }

    /* ---- Debug messenger ---- */
    if (has_debug && layer_count > 0) {
        setup_debug_messenger(inst);
    }

    /* ---- Surface ---- */
    if (!nv_window_create_surface(
            window, inst->handle, &inst->surface)) {
        goto fail_instance;
    }

    /* ---- Physical device ---- */
    if (!select_physical_device(inst)) {
        goto fail_surface;
    }

    vkGetPhysicalDeviceProperties(
        inst->physical_device, &inst->device_props);
    vkGetPhysicalDeviceMemoryProperties(
        inst->physical_device, &inst->mem_props);

    fprintf(stderr, "[nova] GPU: %s\n", inst->device_props.deviceName);

    return inst;

fail_surface:
    vkDestroySurfaceKHR(inst->handle, inst->surface, NULL);
fail_instance:
    destroy_debug_messenger(inst);
    vkDestroyInstance(inst->handle, NULL);
fail_alloc:
    free(inst);
    return NULL;
}

void nv_instance_destroy(NvInstance *inst) {
    if (!inst) {
        return;
    }
    if (inst->surface != VK_NULL_HANDLE) {
        vkDestroySurfaceKHR(inst->handle, inst->surface, NULL);
    }
    destroy_debug_messenger(inst);
    if (inst->handle != VK_NULL_HANDLE) {
        vkDestroyInstance(inst->handle, NULL);
    }
    free(inst);
}

const char *nv_instance_device_name(const NvInstance *inst) {
    if (!inst) {
        return "unknown";
    }
    return inst->device_props.deviceName;
}
