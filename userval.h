#ifndef __USERVAL_H__
#define __USERVAL_H__

#include <gtk/gtk.h>

#include "tuples.h"
#include "exprtree.h"

#define USER_CURVE_POINTS       1024

#define USERVAL_SLIDER      1
#define USERVAL_BOOL        2
#define USERVAL_COLOR       3
#define USERVAL_CURVE       4
#define USERVAL_IMAGE       5

typedef struct _userval_t
{
    ident name;
    int type;
    GtkWidget *widget;
    int tag;
    union
    {
	struct
	{
	    float min;
	    float max;
	    float value;
	} slider;

	struct
	{
	    float value;
	} bool;

	struct
	{
	    tuple_t value;
	} color;

	struct
	{
	    float values[USER_CURVE_POINTS];
	} curve;

	struct
	{
	    int index;
	} image;
    } v;
    struct _userval_t *next;
} userval_t;

void untag_uservals (void);
void clear_untagged_uservals (void);

userval_t* lookup_userval (const char *name, int type);

userval_t* register_slider (const char *name, float min, float max);
userval_t* register_bool (const char *name);
userval_t* register_color (const char *name);
userval_t* register_curve (const char *name);
userval_t* register_image (const char *name);

GtkWidget* make_userval_table (void);

void update_uservals (void);

#endif
