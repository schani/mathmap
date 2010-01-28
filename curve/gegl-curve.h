/* This file is part of GEGL
 *
 * GEGL is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * GEGL is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with GEGL; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2007 Mark Probst <mark.probst@gmail.com>
 */

#ifndef __GEGL_CURVE_H__
#define __GEGL_CURVE_H__

/***
 * GeglCurve:
 *
 * #GeglCurve is a curve describing a unique mapping of values.
 *
 * Used for things like the curves widget in gimp it is a form of doodle
 * alpha.
 */
#include <glib-object.h>

G_BEGIN_DECLS

#define GEGL_TYPE_CURVE            (gegl_curve_get_type ())
#define GEGL_CURVE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GEGL_TYPE_CURVE, GeglCurve))
#define GEGL_CURVE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  GEGL_TYPE_CURVE, GeglCurveClass))
#define GEGL_IS_CURVE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GEGL_TYPE_CURVE))
#define GEGL_IS_CURVE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  GEGL_TYPE_CURVE))
#define GEGL_CURVE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  GEGL_TYPE_CURVE, GeglCurveClass))

typedef struct _GeglCurveClass  GeglCurveClass;

struct _GeglCurve
{
  GObject parent_instance;
};

#ifdef MATHMAP_VERSION
typedef struct _GeglCurve GeglCurve;
#endif

struct _GeglCurveClass
{
  GObjectClass parent_class;
};

GType        gegl_curve_get_type       (void) G_GNUC_CONST;

/**
 * gegl_curve_new:
 * @y_min: minimum y value for curve.
 * @y_max: maximum y value for curve.
 *
 * Create a #GeglCurve that can store a curve with values between @y_min and
 * @y_max.
 *
 * Returns the newly created #GeglCurve.
 */
GeglCurve  * gegl_curve_new            (gdouble      y_min,
                                        gdouble      y_max);


/**
 * gegl_curve_get_y_bounds:
 * @curve: a #GeglCurve.
 * @min_y: return location for minimal value.
 * @max_y: return location for maximal value.
 *
 * Get the bounds on the values of the curve and store the values in
 * the return locaitons provided in @min_y and @max_y.
 */
void         gegl_curve_get_y_bounds   (GeglCurve   *curve,
                                        gdouble     *min_y,
                                        gdouble     *max_y);

/**
 * gegl_curve_add_point:
 * @curve: a #GeglCurve.
 * @x: x coordinate
 * @y: y coordinate
 *
 * Add a point to the curve at @x @y (replacing the value exactly for @x if it
 * already exists.
 */
guint        gegl_curve_add_point      (GeglCurve   *curve,
                                        gdouble      x,
                                        gdouble      y);

/**
 * gegl_curve_get_point:
 * @curve: a #GeglCurve.
 * @index: the position of the value number to retrieve.
 * @x: x coordinate return location.
 * @y: y coordinate return location.
 *
 * Retrive the coordinates for an index.
 */
void         gegl_curve_get_point      (GeglCurve   *curve,
                                        guint        index,
                                        gdouble     *x,
                                        gdouble     *y);

/**
 * gegl_curve_set_point:
 * @curve: a #GeglCurve.
 * @index: the position of the value number to retrieve.
 * @x: x coordinate
 * @y: y coordinate
 *
 * Replace an existing point in a curve.
 */
void         gegl_curve_set_point      (GeglCurve   *curve,
                                        guint        index,
                                        gdouble      x,
                                        gdouble      y);

/**
 * gegl_curve_num_points:
 * @curve: a #GeglCurve.
 *
 * Retrieve the number of points in the curve.
 *
 * Returns the number of points for the coordinates in the curve.
 */
guint        gegl_curve_num_points     (GeglCurve   *curve);

/**
 * gegl_curve_calc_value:
 * @curve: a #GeglCurve.
 *
 * Retrieve the number of points in the curve.
 *
 * Returns the number of points for the coordinates in the curve.
 */
gdouble      gegl_curve_calc_value     (GeglCurve   *curve,
                                        gdouble      x);

/**
 * gegl_curve_calc_values:
 * @curve: a #GeglCurve.
 * @x_min: the minimum value to compute for
 * @x_max: the maxmimum value to compute for
 * @num_samples: number of samples to calculate
 * @xs: return location for the x coordinates
 * @ys: return location for the y coordinates
 *
 * Compute a set (lookup table) of coordinates.
 */
void         gegl_curve_calc_values    (GeglCurve   *curve,
                                        gdouble      x_min,
                                        gdouble      x_max,
                                        guint        num_samples,
                                        gdouble     *xs,
                                        gdouble     *ys);

/***
 */

#define GEGL_TYPE_PARAM_CURVE           (gegl_param_curve_get_type ())
#define GEGL_IS_PARAM_SPEC_CURVE(pspec) (G_TYPE_CHECK_INSTANCE_TYPE ((pspec), GEGL_TYPE_PARAM_CURVE))

GeglCurve  * gegl_curve_default_curve  (void) G_GNUC_CONST;

GType        gegl_param_curve_get_type (void) G_GNUC_CONST;

GParamSpec * gegl_param_spec_curve     (const gchar *name,
                                        const gchar *nick,
                                        const gchar *blurb,
                                        GeglCurve   *default_curve,
                                        GParamFlags  flags);

G_END_DECLS

#endif /* __GEGL_CURVE_H__ */

