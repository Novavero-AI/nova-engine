/*
 * nv_input — SDL3 input polling and action mapping.
 *
 * Polls keyboard, mouse, and gamepad state each frame.  Provides
 * raw key/button state queries and a simple action mapping system
 * for binding actions to inputs.
 *
 * Haskell sees NvInput* as an opaque Ptr ().
 */

#ifndef NV_INPUT_H
#define NV_INPUT_H

#include <stdint.h>

#define NV_MAX_KEYS        512
#define NV_MAX_MOUSE_BTNS  8
#define NV_MAX_ACTIONS     64
#define NV_MAX_ACTION_NAME 32

/* ----------------------------------------------------------------
 * Types
 * ---------------------------------------------------------------- */

typedef struct NvInput {
    /* Keyboard */
    uint8_t keys_down[NV_MAX_KEYS];
    uint8_t keys_pressed[NV_MAX_KEYS];
    uint8_t keys_released[NV_MAX_KEYS];

    /* Mouse */
    float   mouse_x;
    float   mouse_y;
    float   mouse_dx;
    float   mouse_dy;
    float   scroll_x;
    float   scroll_y;
    uint8_t mouse_down[NV_MAX_MOUSE_BTNS];
    uint8_t mouse_pressed[NV_MAX_MOUSE_BTNS];
    uint8_t mouse_released[NV_MAX_MOUSE_BTNS];

    /* Action map */
    struct {
        char    name[NV_MAX_ACTION_NAME];
        int     key_binding;   /* SDL scancode, -1 = unbound */
        int     mouse_binding; /* mouse button index, -1 = unbound */
        uint8_t active;        /* 1 if pressed this frame */
    } actions[NV_MAX_ACTIONS];
    uint32_t action_count;
} NvInput;

/* ----------------------------------------------------------------
 * Lifecycle
 * ---------------------------------------------------------------- */

/* Create the input system. Returns NULL on failure. */
NvInput *nv_input_create(void);

/* Destroy the input system. */
void nv_input_destroy(NvInput *input);

/* ----------------------------------------------------------------
 * Per-frame update
 * ---------------------------------------------------------------- */

/* Poll SDL3 events and update input state.
 * Call once per frame before querying state.
 * Returns 1 if the application should continue, 0 if quit
 * was requested. */
int nv_input_poll(NvInput *input);

/* ----------------------------------------------------------------
 * Raw state queries
 * ---------------------------------------------------------------- */

/* Is the key currently held down? (scancode) */
int nv_input_key_down(const NvInput *input, int scancode);

/* Was the key pressed this frame? */
int nv_input_key_pressed(const NvInput *input, int scancode);

/* Was the key released this frame? */
int nv_input_key_released(const NvInput *input, int scancode);

/* Mouse position (screen pixels). */
float nv_input_mouse_x(const NvInput *input);
float nv_input_mouse_y(const NvInput *input);

/* Mouse delta since last frame. */
float nv_input_mouse_dx(const NvInput *input);
float nv_input_mouse_dy(const NvInput *input);

/* Scroll wheel delta this frame. */
float nv_input_scroll_x(const NvInput *input);
float nv_input_scroll_y(const NvInput *input);

/* Mouse button state (0 = left, 1 = right, 2 = middle). */
int nv_input_mouse_down(const NvInput *input, int button);
int nv_input_mouse_pressed(const NvInput *input, int button);

/* ----------------------------------------------------------------
 * Action mapping
 * ---------------------------------------------------------------- */

/* Bind an action name to a keyboard scancode.
 * Returns the action index, or -1 on failure. */
int nv_input_bind_key(NvInput *input, const char *name,
                       int scancode);

/* Bind an action name to a mouse button.
 * Returns the action index, or -1 on failure. */
int nv_input_bind_mouse(NvInput *input, const char *name,
                          int button);

/* Is the named action active this frame? */
int nv_input_action_active(const NvInput *input, const char *name);

#endif /* NV_INPUT_H */
