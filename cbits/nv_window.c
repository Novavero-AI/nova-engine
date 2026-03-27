/*
 * nv_window — SDL3 window with Vulkan surface support.
 */

#include "nv_window.h"

#include <stdlib.h>

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

NvWindow *nv_window_create(const char *title, uint32_t width,
                           uint32_t height) {
    if (!SDL_Init(SDL_INIT_VIDEO)) {
        return NULL;
    }

    SDL_Window *handle = SDL_CreateWindow(
        title, (int)width, (int)height,
        SDL_WINDOW_VULKAN | SDL_WINDOW_RESIZABLE |
            SDL_WINDOW_HIGH_PIXEL_DENSITY);

    if (!handle) {
        SDL_Quit();
        return NULL;
    }

    NvWindow *window = calloc(1, sizeof(NvWindow));
    if (!window) {
        SDL_DestroyWindow(handle);
        SDL_Quit();
        return NULL;
    }

    window->handle       = handle;
    window->width        = width;
    window->height       = height;
    window->should_close = 0;
    window->was_resized  = 0;

    return window;
}

void nv_window_destroy(NvWindow *window) {
    if (!window) {
        return;
    }
    if (window->handle) {
        SDL_DestroyWindow(window->handle);
    }
    free(window);
    SDL_Quit();
}

/* ----------------------------------------------------------------
 * Per-frame
 * ---------------------------------------------------------------- */

void nv_window_poll_events(NvWindow *window) {
    if (!window) {
        return;
    }

    window->was_resized = 0;

    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
        case SDL_EVENT_QUIT:
            window->should_close = 1;
            break;
        case SDL_EVENT_WINDOW_RESIZED: {
            window->was_resized = 1;
            /* Use pixel size, not window coords (correct on HiDPI). */
            int pw = 0, ph = 0;
            SDL_GetWindowSizeInPixels(window->handle, &pw, &ph);
            window->width  = (uint32_t)pw;
            window->height = (uint32_t)ph;
            break;
        }
        default:
            break;
        }
    }
}

int nv_window_should_close(const NvWindow *window) {
    if (!window) {
        return 1;
    }
    return window->should_close;
}

int nv_window_was_resized(const NvWindow *window) {
    if (!window) {
        return 0;
    }
    return window->was_resized;
}

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

void nv_window_drawable_size(const NvWindow *window, uint32_t *w,
                             uint32_t *h) {
    if (!window || !window->handle) {
        if (w) { *w = 0; }
        if (h) { *h = 0; }
        return;
    }
    int iw = 0;
    int ih = 0;
    SDL_GetWindowSizeInPixels(window->handle, &iw, &ih);
    if (w) { *w = (uint32_t)iw; }
    if (h) { *h = (uint32_t)ih; }
}

int nv_window_vulkan_extensions(uint32_t *count,
                                const char *const **names) {
    if (!count || !names) {
        return 0;
    }
    const char *const *exts = SDL_Vulkan_GetInstanceExtensions(count);
    if (!exts) {
        return 0;
    }
    *names = exts;
    return 1;
}

int nv_window_create_surface(NvWindow *window, VkInstance instance,
                             VkSurfaceKHR *surface) {
    if (!window || !window->handle || !instance || !surface) {
        return 0;
    }
    return SDL_Vulkan_CreateSurface(window->handle, instance, NULL,
                                    surface)
               ? 1
               : 0;
}
