/*
 * nv_window — SDL3 window with Vulkan surface support.
 *
 * Creates and manages an SDL3 window configured for Vulkan
 * rendering.  Handles event polling, close detection, resize
 * tracking, Vulkan instance extension enumeration, and Vulkan
 * surface creation.
 *
 * Haskell sees NvWindow* as an opaque Ptr ().
 */

#ifndef NV_WINDOW_H
#define NV_WINDOW_H

#include <stdint.h>

#include <vulkan/vulkan.h>
#include <SDL3/SDL.h>
#include <SDL3/SDL_vulkan.h>

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvWindow {
    SDL_Window *handle;
    uint32_t    width;
    uint32_t    height;
    int         should_close;
    int         was_resized;
} NvWindow;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create an SDL3 window with Vulkan support.
 * Initialises SDL video subsystem.  Returns NULL on failure. */
NvWindow *nv_window_create(const char *title, uint32_t width,
                           uint32_t height);

/* Destroy the window and shut down SDL. */
void nv_window_destroy(NvWindow *window);

/* ----------------------------------------------------------------
 * Per-frame
 * ---------------------------------------------------------------- */

/* Poll all pending SDL events.  Call exactly once per frame.
 * Updates should_close and was_resized flags. */
void nv_window_poll_events(NvWindow *window);

/* 1 if a quit event has been received, 0 otherwise. */
int nv_window_should_close(const NvWindow *window);

/* 1 if the window was resized during the last poll, 0 otherwise. */
int nv_window_was_resized(const NvWindow *window);

/* ----------------------------------------------------------------
 * Queries
 * ---------------------------------------------------------------- */

/* Get the drawable size in pixels (differs from window size on
 * HiDPI / Retina displays). */
void nv_window_drawable_size(const NvWindow *window, uint32_t *w,
                             uint32_t *h);

/* Get the Vulkan instance extensions required by SDL3.
 * On success writes to *count and *names and returns 1.
 * The returned array is owned by SDL — do not free it.
 * SDL must be initialised (call after nv_window_create). */
int nv_window_vulkan_extensions(uint32_t *count,
                                const char *const **names);

/* Create a Vulkan surface for this window.
 * Returns 1 on success, writing the surface handle to *surface. */
int nv_window_create_surface(NvWindow *window, VkInstance instance,
                             VkSurfaceKHR *surface);

#endif /* NV_WINDOW_H */
