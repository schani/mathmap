/* -*- c -*- */

#ifndef __MATHMAP_H__
#define __MATHMAP_H__

#include <libgimp/gimp.h>

extern char error_string[];
extern int auto_preview;

extern int originX, originY, img_width, img_height;

void dialog_update_preview (void);

void mathmap_get_pixel (int drawable_index, int x, int y, guchar *pixel);
void mathmap_get_fast_pixel(int drawable_index, int x, int y, guchar *pixel);

int alloc_input_drawable (GDrawable *drawable);
void free_input_drawable (int index);
GDrawable* get_input_drawable (int index);

#endif
