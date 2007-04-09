/*
 * color.h
 *
 * MathMap
 *
 * Copyright (C) 2002-2007 Mark Probst
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef __COLOR_H__
#define __COLOR_H__

typedef unsigned int color_t;

typedef struct
{
    float red;
    float green;
    float blue;
    float alpha;
} float_color_t;

#define MAKE_RGBA_COLOR_UNSAFE(r,g,b,a)     ((((color_t)(r))<<24)|(((color_t)(g))<<16)|(((color_t)(b))<<8)|((color_t)(a)))
#define MAKE_RGBA_COLOR(r,g,b,a)            MAKE_RGBA_COLOR_UNSAFE((color_t)(r)&0xff, (color_t)(g)&0xff, (color_t)(b)&0xff, (color_t)(a)&0xff)
#define MAKE_RGBA_COLOR_FLOAT(r,g,b,a)      MAKE_RGBA_COLOR((int)((r)*255.0),(int)((g)*255.0),(int)((b)*255.0),(int)((a)*255.0))

#define RED(c)                              ((c)>>24)
#define GREEN(c)                            (((c)>>16)&0xff)
#define BLUE(c)                             (((c)>>8)&0xff)
#define ALPHA(c)                            ((c)&0xff)

#define RED_FLOAT(c)                        (RED((c))/255.0)
#define GREEN_FLOAT(c)                      (GREEN((c))/255.0)
#define BLUE_FLOAT(c)                       (BLUE((c))/255.0)
#define ALPHA_FLOAT(c)                      (ALPHA((c))/255.0)

#define COLOR_ADD(a,b)                      (MAKE_RGBA_COLOR(RED((a))+RED((b)),GREEN((a))+GREEN((b)),BLUE((a))+BLUE((b)),ALPHA((a))+ALPHA((b))))
#define COLOR_MUL_FLOAT(c,f)                ((float_color_t){RED((c))*(f), GREEN((c))*(f), BLUE((c))*(f), ALPHA((c))*(f)})

#define FLOAT_COLOR_ADD(a,b)                ((float_color_t){(a).red+(b).red, (a).green+(b).green, (a).blue+(b).blue, (a).alpha+(b).alpha})
#define FLOAT_COLOR_TO_COLOR(fc)            (MAKE_RGBA_COLOR(rintf((fc).red), rintf((fc).green), rintf((fc).blue), rintf((fc).alpha)))

#define COLOR_BLACK                         (MAKE_RGBA_COLOR(0,0,0,0xff))
#define COLOR_WHITE                         (MAKE_RGBA_COLOR(0xff,0xff,0xff,0xff))

#endif
