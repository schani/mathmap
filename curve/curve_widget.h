#ifndef __CURVE_WIDGET_H__
#define __CURVE_WIDGET_H__

#include <glib.h>
#include <gtk/gtk.h>
#include "gegl-curve.h"

#define CURVE_WIDGET_LINE_WIDTH 1.0
#define CURVE_WIDGET_POINT_RADIUS 3.0
#define CURVE_WIDGET_MAX_POINT_GRAB_DISTANCE 5.0

#define CURVE_WIDGET_BG_COLOR 0.9, 0.9, 0.9
#define CURVE_WIDGET_CURVE_COLOR 1.0, 0.0, 0.0
#define CURVE_WIDGET_POINT_COLOR 0.0, 0.0, 0.0


typedef void (*curve_widget_curve_changed_callback_t) (GtkWidget *widget);

GtkWidget *curve_widget_new(curve_widget_curve_changed_callback_t curve_changed_callback);
void curve_widget_set_curve(GtkWidget *widget, GeglCurve *curve);
GeglCurve *curve_widget_get_curve(GtkWidget *widget);


#endif // __CURVE_WIDGET_H__
