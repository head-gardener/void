#ifndef VOID_GUI
#define VOID_GUI

#include "window.h"
#include <SDL2/SDL.h>

enum void_return_codes {
  VOID_RETURN_CONTINUE = 0,
  VOID_RETURN_EXIT,
};

/**
 * Initialize GUI.
 */
struct void_window *void_gui_init(void);

/**
 * Repeatedly handle GUI events until external action is requested, upon which
 * the function exits.
 */
int void_gui_exec(struct void_window *window);

/**
 * Free resources and discard GUI.
 */
int void_gui_finish(struct void_window *window);

/**
 * Push entry to the end of the spreadsheet.
 */
int void_gui_add(wchar_t *name, wchar_t *phone, struct void_window *window);

#endif
