/*
 * userval.h
 *
 * MathMap
 *
 * Copyright (C) 1997-2010 Mark Probst
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

#ifndef __USERVAL_H__
#define __USERVAL_H__

#include "glib.h"

#include "tuples.h"
#include "exprtree.h"
#include "color.h"

/* TEMPLATE userval_points */
#define USER_CURVE_POINTS       1024
#define USER_GRADIENT_POINTS    1024
/* END */

#define USERVAL_INT_CONST   1
#define USERVAL_FLOAT_CONST 2
#define USERVAL_BOOL_CONST  3
#define USERVAL_COLOR       4
#define USERVAL_CURVE       5
#define USERVAL_GRADIENT    6
/* TEMPLATE userval_image */
#define USERVAL_IMAGE       7
/* END */

typedef struct _userval_info_t
{
    char *name;
    int type;
    int index;

    union
    {
	struct
	{
	    int min;
	    int max;
	    int default_value;
	} int_const;
	struct
	{
	    float min;
	    float max;
	    float default_value;
	} float_const;
	struct
	{
	    int default_value;
	} bool_const;
	struct
	{
	    unsigned int flags;
	} image;
    } v;

    struct _userval_info_t *next;
} userval_info_t;

struct _image_t;
struct _input_drawable_t;

/* TEMPLATE userval */
typedef struct
{
    int num_control_points;
    double *control_xs;
    double *control_ys;

    float *values;		/* calculated from the control points */
} curve_t;

typedef struct
{
    color_t *values;
} gradient_t;

typedef struct _userval_t
{
    union
    {
	int int_const;
	float float_const;
	int bool_const;
	struct _image_t *image;
	curve_t *curve;
	gradient_t *gradient;
	color_t color;
    } v;
} userval_t;
/* END */

userval_info_t* lookup_userval (userval_info_t *infos, const char *name);
userval_info_t* lookup_matching_userval (userval_info_t *infos, userval_info_t *test_info);

userval_info_t* register_int_const (userval_info_t **infos, const char *name, int min, int max, int default_value);
userval_info_t* register_float_const (userval_info_t **infos, const char *name, float min, float max, float default_value);
userval_info_t* register_bool (userval_info_t **infos, const char *name, int default_value);
userval_info_t* register_color (userval_info_t **infos, const char *name);
userval_info_t* register_curve (userval_info_t **infos, const char *name);
userval_info_t* register_gradient (userval_info_t **infos, const char *name);
userval_info_t* register_image (userval_info_t **infos, const char *name, unsigned int flags);

void instantiate_userval (userval_t *val, userval_info_t *info, struct _mathmap_invocation_t *invocation);
userval_t* instantiate_uservals (userval_info_t *infos, struct _mathmap_invocation_t *invocation);
void free_uservals (userval_t *uservals, userval_info_t *infos);
void free_userval_infos (userval_info_t *infos);

void set_userval_to_default (userval_t *val, userval_info_t *info, struct _mathmap_invocation_t *invocation);

void copy_userval (userval_t *dst, userval_t *src, int type);

void assign_image_userval_drawable (userval_info_t *info, userval_t *val, struct _input_drawable_t *drawable);

void update_uservals (userval_info_t *infos, userval_t *uservals);

const char* userval_type_name (int type);

gradient_t* get_default_gradient (void);

#endif
