/*
 * nv_input — SDL3 input polling and action mapping.
 */

#include "nv_input.h"

#include <stdlib.h>
#include <string.h>

#include <SDL3/SDL.h>

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

NvInput *nv_input_create(void) {
    NvInput *input = calloc(1, sizeof(NvInput));
    if (!input) return NULL;

    /* Initialize all bindings to unbound */
    for (uint32_t i = 0; i < NV_MAX_ACTIONS; i++) {
        input->actions[i].key_binding   = -1;
        input->actions[i].mouse_binding = -1;
    }
    return input;
}

void nv_input_destroy(NvInput *input) {
    free(input);
}

/* ----------------------------------------------------------------
 * Per-frame update
 * ---------------------------------------------------------------- */

int nv_input_poll(NvInput *input, NvWindow *window) {
    if (!input) return 0;

    /* Clear per-frame state */
    memset(input->keys_pressed, 0, sizeof(input->keys_pressed));
    memset(input->keys_released, 0, sizeof(input->keys_released));
    memset(input->mouse_pressed, 0, sizeof(input->mouse_pressed));
    memset(input->mouse_released, 0, sizeof(input->mouse_released));
    input->mouse_dx = 0.0f;
    input->mouse_dy = 0.0f;
    input->scroll_x = 0.0f;
    input->scroll_y = 0.0f;

    if (window) {
        window->was_resized = 0;
    }

    /* Poll SDL events */
    SDL_Event event;
    while (SDL_PollEvent(&event)) {
        switch (event.type) {
        case SDL_EVENT_QUIT:
            if (window) {
                window->should_close = 1;
            }
            return 0;

        case SDL_EVENT_KEY_DOWN:
            if (!event.key.repeat) {
                int sc = (int)event.key.scancode;
                if (sc >= 0 && sc < NV_MAX_KEYS) {
                    input->keys_down[sc]    = 1;
                    input->keys_pressed[sc] = 1;
                }
            }
            break;

        case SDL_EVENT_KEY_UP: {
            int sc = (int)event.key.scancode;
            if (sc >= 0 && sc < NV_MAX_KEYS) {
                input->keys_down[sc]     = 0;
                input->keys_released[sc] = 1;
            }
        } break;

        case SDL_EVENT_MOUSE_MOTION:
            input->mouse_x  = event.motion.x;
            input->mouse_y  = event.motion.y;
            input->mouse_dx += event.motion.xrel;
            input->mouse_dy += event.motion.yrel;
            break;

        case SDL_EVENT_MOUSE_BUTTON_DOWN: {
            int btn = (int)event.button.button - 1;
            if (btn >= 0 && btn < NV_MAX_MOUSE_BTNS) {
                input->mouse_down[btn]    = 1;
                input->mouse_pressed[btn] = 1;
            }
        } break;

        case SDL_EVENT_MOUSE_BUTTON_UP: {
            int btn = (int)event.button.button - 1;
            if (btn >= 0 && btn < NV_MAX_MOUSE_BTNS) {
                input->mouse_down[btn]     = 0;
                input->mouse_released[btn] = 1;
            }
        } break;

        case SDL_EVENT_WINDOW_RESIZED:
            if (window) {
                window->was_resized = 1;
                int pw = 0, ph = 0;
                SDL_GetWindowSizeInPixels(window->handle, &pw, &ph);
                window->width  = (uint32_t)pw;
                window->height = (uint32_t)ph;
            }
            break;

        case SDL_EVENT_MOUSE_WHEEL:
            input->scroll_x += event.wheel.x;
            input->scroll_y += event.wheel.y;
            break;

        default:
            break;
        }
    }

    /* Update action states */
    for (uint32_t i = 0; i < input->action_count; i++) {
        int active = 0;
        int kb = input->actions[i].key_binding;
        int mb = input->actions[i].mouse_binding;
        if (kb >= 0 && kb < NV_MAX_KEYS && input->keys_down[kb])
            active = 1;
        if (mb >= 0 && mb < NV_MAX_MOUSE_BTNS && input->mouse_down[mb])
            active = 1;
        input->actions[i].active = (uint8_t)active;
    }

    return 1;
}

/* ----------------------------------------------------------------
 * Raw state queries
 * ---------------------------------------------------------------- */

int nv_input_key_down(const NvInput *input, int scancode) {
    if (!input || scancode < 0 || scancode >= NV_MAX_KEYS) return 0;
    return input->keys_down[scancode];
}

int nv_input_key_pressed(const NvInput *input, int scancode) {
    if (!input || scancode < 0 || scancode >= NV_MAX_KEYS) return 0;
    return input->keys_pressed[scancode];
}

int nv_input_key_released(const NvInput *input, int scancode) {
    if (!input || scancode < 0 || scancode >= NV_MAX_KEYS) return 0;
    return input->keys_released[scancode];
}

float nv_input_mouse_x(const NvInput *input) {
    return input ? input->mouse_x : 0.0f;
}

float nv_input_mouse_y(const NvInput *input) {
    return input ? input->mouse_y : 0.0f;
}

float nv_input_mouse_dx(const NvInput *input) {
    return input ? input->mouse_dx : 0.0f;
}

float nv_input_mouse_dy(const NvInput *input) {
    return input ? input->mouse_dy : 0.0f;
}

float nv_input_scroll_x(const NvInput *input) {
    return input ? input->scroll_x : 0.0f;
}

float nv_input_scroll_y(const NvInput *input) {
    return input ? input->scroll_y : 0.0f;
}

int nv_input_mouse_down(const NvInput *input, int button) {
    if (!input || button < 0 || button >= NV_MAX_MOUSE_BTNS) return 0;
    return input->mouse_down[button];
}

int nv_input_mouse_pressed(const NvInput *input, int button) {
    if (!input || button < 0 || button >= NV_MAX_MOUSE_BTNS) return 0;
    return input->mouse_pressed[button];
}

/* ----------------------------------------------------------------
 * Action mapping
 * ---------------------------------------------------------------- */

static int find_or_create_action(NvInput *input, const char *name) {
    /* Search existing */
    for (uint32_t i = 0; i < input->action_count; i++) {
        if (strncmp(input->actions[i].name, name,
                     NV_MAX_ACTION_NAME - 1) == 0) {
            return (int)i;
        }
    }
    /* Create new */
    if (input->action_count >= NV_MAX_ACTIONS) return -1;
    uint32_t idx = input->action_count++;
    strncpy(input->actions[idx].name, name, NV_MAX_ACTION_NAME - 1);
    input->actions[idx].name[NV_MAX_ACTION_NAME - 1] = '\0';
    input->actions[idx].key_binding   = -1;
    input->actions[idx].mouse_binding = -1;
    return (int)idx;
}

int nv_input_bind_key(NvInput *input, const char *name,
                       int scancode) {
    if (!input || !name) return -1;
    int idx = find_or_create_action(input, name);
    if (idx >= 0) input->actions[idx].key_binding = scancode;
    return idx;
}

int nv_input_bind_mouse(NvInput *input, const char *name,
                          int button) {
    if (!input || !name) return -1;
    int idx = find_or_create_action(input, name);
    if (idx >= 0) input->actions[idx].mouse_binding = button;
    return idx;
}

int nv_input_action_active(const NvInput *input, const char *name) {
    if (!input || !name) return 0;
    for (uint32_t i = 0; i < input->action_count; i++) {
        if (strncmp(input->actions[i].name, name,
                     NV_MAX_ACTION_NAME - 1) == 0) {
            return input->actions[i].active;
        }
    }
    return 0;
}
