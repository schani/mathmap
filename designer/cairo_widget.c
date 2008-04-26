/* -*- c -*- */

/*
 * cairo_widget.c
 *
 * MathMap
 *
 * Copyright (C) 2008 Herbert Poetzl
 * Copyright (C) 2008 Mark Probst
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

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <gtk/gtk.h>

#include "../lispreader/lispreader.h"

#include "designer.h"
#include "cairo_widget.h"


static void
circle_path(cairo_t *cr, _point_t p, double r)
{
    cairo_arc(cr, p.x, p.y, r, 0, 2 * M_PI);
}

static void
rect_path(cairo_t *cr, _rect_t r)
{
    cairo_rectangle(cr, r.o.x, r.o.y, r.s.w, r.s.h);
}

static void
cross_path(cairo_t *cr, _rect_t r, double d)
{
    _corn_t c = _corners(r);

    cairo_move_to(cr, c.ll.x + d, c.ll.y + d);
    cairo_line_to(cr, c.ur.x - d, c.ur.y - d);
    cairo_move_to(cr, c.lr.x - d, c.lr.y + d);
    cairo_line_to(cr, c.ul.x + d, c.ul.y - d);
}

static const double round_s[4] = { 10, 10, 10, 10 };
static const double upper_s[4] = { 10, 10, 0, 0 };
static const double lower_s[4] = { 0, 0, 10, 10 };

static const double circ4_d[2] = { 10 * M_PI / 12.0, 6 * M_PI / 12.0 };


static void
round_path(cairo_t *cr, _rect_t r, const double s[4])
{
    _corn_t c = _corners(r);

    cairo_move_to(cr, c.lr.x - s[1], c.lr.y);
    if (s[1])
	cairo_curve_to(cr, c.lr.x, c.lr.y, c.lr.x, c.lr.y,
		c.lr.x, c.lr.y + s[1]);
    cairo_line_to(cr, c.ur.x, c.ur.y - s[2]);
    if (s[2])
	cairo_curve_to(cr, c.ur.x, c.ur.y, c.ur.x, c.ur.y,
		c.ur.x - s[2], c.ur.y);
    cairo_line_to(cr, c.ul.x + s[3], c.ul.y);
    if (s[3])
	cairo_curve_to(cr, c.ul.x, c.ul.y, c.ul.x, c.ul.y,
		c.ul.x, c.ur.y - s[3]);
    cairo_line_to(cr, c.ll.x, c.ll.y + s[0]);
    if (s[0])
	cairo_curve_to(cr, c.ll.x, c.ll.y, c.ll.x, c.ll.y,
		c.ll.x + s[0], c.ll.y);
    cairo_close_path(cr);
}


static void
set_title_font(cairo_t *cr)
{
    cairo_select_font_face(cr, "Sans",
	    CAIRO_FONT_SLANT_NORMAL,
	    CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size (cr, 12.0);
}

static void
set_slot_font(cairo_t *cr)
{
    cairo_select_font_face(cr, "Sans",
	    CAIRO_FONT_SLANT_NORMAL,
	    CAIRO_FONT_WEIGHT_NORMAL);
    cairo_set_font_size (cr, 11.0);
}

static _rect_t
text_rect(cairo_t *cr, const char *t)
{
    cairo_text_extents_t extents;

    cairo_text_extents(cr, t, &extents);
    return _rect(
	extents.x_bearing, extents.y_bearing,
	extents.width, extents.height);
}


typedef enum
{
    STATE_IDLE = 0,
    STATE_MOVING,
    STATE_CONOUT,
    STATE_CONIN,
} _state_t;




typedef struct
{
    designer_design_t *design;
    designer_design_changed_callback_t design_changed_callback;
    designer_node_focussed_callback_t node_focussed_callback;
    GtkWidget *widget;
    GtkWidget *drawing_area;
    GtkWidget *hscrollbar;
    GtkWidget *vscrollbar;

    gboolean dragging;
    _point_t mouse, mouse_down, mouse_up;
    _state_t state;

    designer_node_t *active_node;
    int active_slot_id;
    _point_t origin;
    
    designer_node_t *target_node;
    int target_slot_id;
    _point_t target;
    int target_check;
} widget_data_t;


typedef struct
{

    /* calculated */
    _size_t offset;
} slot_spec_data_t;

typedef struct
{

    /* calculated */
    _rect_t br, ir, or, sr;
    int is, os;
} node_type_data_t;


typedef struct
{
    _point_t origin;

    /* calculated */
    _rect_t nr, lr, tr, br, ir, or, xr;
} node_data_t;


typedef enum
{
    HIT_NOTHING = 0,
    HIT_TITLE,
    HIT_LABEL,
    HIT_CLOSE,
    HIT_BODY,
    HIT_INPUT,
    HIT_OUTPUT,
} _hit_t;




static slot_spec_data_t *
slot_spec_data(designer_slot_spec_t *ss)
{
    slot_spec_data_t *ssd;

    if (!(ssd = designer_slot_spec_get_widget_data(ss))) {
	ssd = calloc(sizeof(slot_spec_data_t), 1);
	g_assert(ssd != NULL);
	designer_slot_spec_set_widget_data(ss, ssd);
    }
    return ssd;
}

static node_type_data_t *
node_type_data(designer_node_type_t *nt)
{
    node_type_data_t *ntd;

    if (!(ntd = designer_node_type_get_widget_data(nt))) {
	ntd = calloc(sizeof(node_type_data_t), 1);
	g_assert(ntd != NULL);
	designer_node_type_set_widget_data(nt, ntd);
    }
    return ntd;
}

static node_data_t *
node_data(designer_node_t *n)
{
    node_data_t *nd;

    if (!(nd = designer_node_get_widget_data(n))) {
	nd = calloc(sizeof(node_data_t), 1);
	g_assert(nd != NULL);
	designer_node_set_widget_data(n, nd);
    }
    return nd;
}




static void
calc_node_type(cairo_t *cr, designer_node_type_t *nt)
{
    node_type_data_t *ntd = node_type_data(nt);
    _rect_t br, ir, or, sr;

    ntd->is = g_slist_length(nt->input_slot_specs);
    ntd->os = g_slist_length(nt->output_slot_specs);

    set_slot_font(cr);
    sr = text_rect(cr, "Mg");
    /* add sep space */
    sr.s.h += 3;

    ir = _rect(0, 0, 10, 0);
    for (int i=0; i<ntd->is; i++) {
	designer_slot_spec_t *slot =
	    g_slist_nth_data(nt->input_slot_specs, i);
	slot_spec_data_t *ssd = slot_spec_data(slot);
	
	ssd->offset = _size(3, 5 + sr.s.h * i);
	ir = _union(ir, text_rect(cr, slot->name));
    }
    /* offset left */
    ir = _offset(ir, _size(0, 5));
    /* add slot and sep space */
    ir.s.w += 5+5;
    /* height including sep space */
    ir.s.h = ntd->is * sr.s.h - 3;

    or = _rect(0, 0, 10, 0);
    for (int i=0; i<ntd->os; i++) {
	designer_slot_spec_t *slot =
	    g_slist_nth_data(nt->output_slot_specs, i);
	slot_spec_data_t *ssd = slot_spec_data(slot);
	
	ssd->offset = _size(3, 5 + sr.s.h * i);
	or = _union(or, text_rect(cr, slot->name));
    }
    /* move right, including sep space */
    or = _offset(or, _size(ir.s.w + 5, 5));
    /* add slot and sep space */
    or.s.w += 5+5;
    /* height including sep space */
    or.s.h = ntd->os * sr.s.h - 3;

    br = _union(ir, or);
    /* reserve space around area */
    br = _inset(br, _size(-5, -5));

    ntd->br = br;
    ntd->ir = _splith(&ir, 6);
    _splith(&or, or.s.w - 6);
    ntd->or = or;
    ntd->sr = sr;
}


static void
calc_node(cairo_t *cr, designer_node_t *n)
{
    node_data_t *nd = node_data(n);
    node_type_data_t *ntd = node_type_data(n->type);
    _rect_t nr, lr, tr, ir, or, br, xr;

    set_title_font(cr);
    lr = text_rect(cr, n->name);

    tr = _inset(lr, _size(-5, -5));
    tr.s.w += 20; /* button space */

    br = ntd->br;

    /* move into place */
    br.o.y = tr.o.y + tr.s.h;
    nr = _union(tr, br);

    /* break down areas once again */
    br = nr; tr = _splitv(&br, tr.s.h);

    /* split off close button */
    xr = tr; _splith(&xr, tr.s.w - 16);
    xr = _splitv(&xr, 16);
    xr = _inset(xr, _size(4, 4));

    ir = ntd->ir;
    /* align inputs to the left */
    ir.o = _move(br.o, _size(5, 5));

    or = ntd->or;
    /* align outputs to the right */
    or.o = _move(br.o, _size(br.s.w - or.s.w - 5, 5));

    nd->nr = nr;
    nd->lr = lr;
    nd->tr = tr;
    nd->br = br;
    nd->ir = ir;
    nd->or = or;
    nd->xr = xr;
}


static void 
draw_node(cairo_t *cr, designer_node_t *n)
{
    designer_node_type_t *nt = n->type;
    node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);

    _point_t pos = nd->origin;

    cairo_save(cr);
    cairo_translate(cr, pos.x, pos.y);

    /* drop shadow */
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.1);
    round_path(cr, _offset(nd->nr, _size(3, 3)), round_s);
    cairo_fill(cr);

    /* title area color */
    cairo_set_source_rgba(cr, 0.3, 1.0, 0.8, 0.8);
    round_path(cr, nd->tr, upper_s);
    cairo_fill(cr);

    /* title text */
    set_title_font(cr);
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.8);
    cairo_move_to(cr, nd->lr.o.x - 1, 
	nd->lr.o.y + nd->lr.s.h - 1);
    cairo_show_text(cr, n->name);

    /* close button */
    cairo_set_line_width(cr, 1.0);
    rect_path(cr, nd->xr);
    cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.8);
    cairo_fill_preserve(cr);
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.5);
    cairo_stroke(cr);
    cross_path(cr, nd->xr, 2);
    cairo_stroke(cr);

    /* body area color */
    cairo_set_source_rgba(cr, 1.0, 1.0, 1.0, 0.7);
    round_path(cr, nd->br, lower_s);
    cairo_fill(cr);

#if 0
    /* slot border */
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.3);
    cairo_set_line_width(cr, 1.0);
    rect_path(cr, nd->ir);
    cairo_stroke(cr);
    rect_path(cr, nd->or);
    cairo_stroke(cr);
#endif

    /* prepare for slots */
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.6);
    cairo_set_line_width(cr, 1.0);
    set_slot_font(cr);

    /* input slots */
    for (int i=0; i<ntd->is; i++) {
	designer_slot_spec_t *slot =
	    g_slist_nth_data(nt->input_slot_specs, i);
	slot_spec_data_t *ssd = slot_spec_data(slot);
	_point_t sl = _move(nd->ir.o, ssd->offset);
	
	cairo_move_to(cr, sl.x + 6, sl.y + 3);
	cairo_show_text(cr, slot->name);
	cairo_stroke(cr);
	circle_path(cr, sl, 3.0);
	cairo_stroke(cr);
    }

    for (int i=0; i<ntd->os; i++) {
	designer_slot_spec_t *slot =
	    g_slist_nth_data(nt->output_slot_specs, i);
	slot_spec_data_t *ssd = slot_spec_data(slot);
	_rect_t sr = text_rect(cr, slot->name);
	_point_t sl = _move(nd->or.o, ssd->offset);

	cairo_move_to(cr, sl.x - sr.s.w - 8, sl.y + 3);
	cairo_show_text(cr, slot->name);
	cairo_stroke(cr);
	circle_path(cr, sl, 3.0);
	cairo_stroke(cr);
    }
    /* filter border */
    cairo_set_line_width(cr, 1.0);
    cairo_set_source_rgba(cr, 0.0, 0.0, 0.0, 0.8);
    round_path(cr, nd->nr, round_s);
    cairo_stroke(cr);
    cairo_move_to(cr, nd->br.o.x, nd->br.o.y);
    cairo_line_to(cr, nd->br.o.x + nd->br.s.w, nd->br.o.y);
    cairo_stroke(cr);

/*
    rect_path(cr, nd->ir);
    cairo_stroke(cr);
    rect_path(cr, nd->or);
    cairo_stroke(cr); */

    cairo_restore(cr);
}



static void 
draw_connect(cairo_t *cr, _point_t p1, _point_t p2, int type)
{
    _point_t cp1, cp2;
    _size_t d = _delta(p1, p2);
    float f = 0.5;

    if (d.w*d.w > d.h*d.h) {
    	cp1 = _move(p1, _size(d.w*f, 0));
    	cp2 = _move(p2, _size(-d.w*f, 0));
    } else {
    	cp1 = _move(p1, _size(0, d.h*f));
    	cp2 = _move(p2, _size(0, -d.h*f));
    }

    cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);

    cairo_new_path(cr);
    cairo_move_to(cr, p1.x, p1.y);
    cairo_curve_to(cr, cp1.x, cp1.y, cp2.x, cp2.y, p2.x, p2.y);

    cairo_set_source_rgba(cr, 0.4, 0.0, 0.0, 0.6);
    cairo_set_line_width(cr, 4.0);
    cairo_stroke_preserve(cr);

    cairo_set_source_rgba(cr, 1.0, 0.6, 0.6, 0.6);
    cairo_set_line_width(cr, 3.0);
    cairo_stroke(cr);
}


static _hit_t 
hit_node(designer_node_t *n, _point_t m)
{
    // designer_node_type_t *nt = n->type;
    // node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);

    _point_t pos = nd->origin;
    _point_t mr = _point(m.x - pos.x, m.y - pos.y);

    if (!_inrect(mr, nd->nr))
	return HIT_NOTHING;

    /* check for head rect */
    if (_inrect(mr, nd->tr)) {
	if (_inrect(mr, nd->xr))
	    return HIT_CLOSE;
	if (_inrect(mr, nd->lr))
	    return HIT_LABEL;
	return HIT_TITLE;
    } else {
	if (_inrect(mr, nd->ir))
	    return HIT_INPUT;
	if (_inrect(mr, nd->or))
	    return HIT_OUTPUT;
	return HIT_BODY;
    }
}

static int
hit_input_slot(designer_node_t *n, _point_t m)
{
    designer_node_type_t *nt = n->type;
    node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);
    _size_t d = _delta(_move(nd->ir.o, _ptos(nd->origin)), m);

    return d.h / ntd->sr.s.h;
}

static int
hit_output_slot(designer_node_t *n, _point_t m)
{
    designer_node_type_t *nt = n->type;
    node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);
    _size_t d = _delta(_move(nd->or.o, _ptos(nd->origin)), m);

    return d.h / ntd->sr.s.h;
}

static _point_t
input_slot_origin(designer_node_t *n, int i)
{
    designer_node_type_t *nt = n->type;
    node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);

    return _move(_move(nd->ir.o,
	_size(3, 5 + ntd->sr.s.h * i)),
	_ptos(nd->origin));
}

static _point_t
output_slot_origin(designer_node_t *n, int i)
{
    designer_node_type_t *nt = n->type;
    node_type_data_t *ntd = node_type_data(nt);
    node_data_t *nd = node_data(n);

    return _move(_move(nd->or.o,
	_size(nd->or.s.w - 3, 5 + ntd->sr.s.h * i)),
	_ptos(nd->origin));
}

static designer_slot_spec_t *
input_slot_spec(designer_node_t *n, int i)
{
    return g_slist_nth_data(n->type->input_slot_specs, i);
}

static designer_slot_spec_t *
output_slot_spec(designer_node_t *n, int i)
{
    return g_slist_nth_data(n->type->output_slot_specs, i);
}



static void
draw_slot_highlight(cairo_t *cr, _point_t sl, int check)
{
    switch (check) {
    case DESIGNER_CONNECTION_UNCONNECTABLE:
    	cairo_set_source_rgba(cr, 0.6, 0.6, 0.7, 0.5);
    	cairo_set_line_width(cr, 2.0);
    	break;
    case DESIGNER_CONNECTION_CONNECTABLE:
    	cairo_set_source_rgba(cr, 0.0, 0.5, 1.0, 0.5);
    	cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
    	cairo_set_line_width(cr, 3.0);
    	cairo_set_dash(cr, circ4_d, 2, 0);
    	break;
    case DESIGNER_CONNECTION_FREE:
    	cairo_set_source_rgba(cr, 0.0, 0.5, 1.0, 0.5);
    	cairo_set_line_width(cr, 2.0);
    	break;
    }
    circle_path(cr, sl, 4.0);
    cairo_stroke(cr);
}

static void 
draw_highlight(cairo_t *cr, designer_node_t *n, _point_t m, _hit_t hit, int check)
{
    node_data_t *nd = node_data(n);

    _point_t pos = nd->origin;
    _point_t mr = _point(m.x - pos.x, m.y - pos.y);

    cairo_save(cr);

    switch (hit) {
    case HIT_LABEL:
    case HIT_TITLE:
    case HIT_CLOSE:
    case HIT_BODY:
	cairo_translate(cr, pos.x, pos.y);
	cairo_set_line_width(cr, 4.0);
	cairo_set_source_rgba(cr, 0.0, 0.5, 1.0, 0.5);
	round_path(cr, _inset(nd->nr, _size(-1, -1)), round_s);
	cairo_stroke(cr);
	break;

	break;
    case HIT_INPUT: {
	/* FIME: replace by target */
	int si = hit_input_slot(n, m);
	_point_t sl = input_slot_origin(n, si);

	draw_slot_highlight(cr, sl, check);
	break;
	}
    case HIT_OUTPUT: {
	/* FIME: replace by target */
	int si = hit_output_slot(n, m);
	_point_t sl = output_slot_origin(n, si);

	draw_slot_highlight(cr, sl, check);
	break;
	}
    }
    cairo_restore(cr);
}


static void
signal_design_change (widget_data_t *data)
{
    if (data->design_changed_callback != NULL)
	data->design_changed_callback(data->widget, data->design);
}

static widget_data_t *
get_widget_data (GtkWidget *widget)
{
    widget_data_t *data = g_object_get_data(G_OBJECT(widget), "designer-data");

    g_assert(data != NULL);
    return data;
}

static void
set_scrollable_size (widget_data_t *data, unsigned int width, unsigned int height)
{
    gtk_layout_set_size (GTK_LAYOUT(data->drawing_area), width, height);
}

static gboolean
expose_event (GtkWidget *widget, GdkEventExpose *event)
{
    widget_data_t *data = get_widget_data(widget);
    designer_node_t *hn = NULL;
    _hit_t hit;
    int hs = -1;

    cairo_t *cr = gdk_cairo_create(GTK_LAYOUT(widget)->bin_window);
    // _size_t ws = _size(widget->allocation.width, widget->allocation.height);
    
    cairo_set_source_rgb(cr, 0.9, 0.9, 0.9);
    cairo_paint(cr);

    cairo_translate(cr, 0.5, 0.5);


    /* draw the slot conenctions */
    for (GSList *list = data->design->nodes;
	list != NULL; list = list->next) {
	designer_node_t *dst = list->data;
	node_data_t *ndd = node_data(dst);

	calc_node_type(cr, dst->type);
	calc_node(cr, dst);

	for (GSList *slot_list = dst->input_slots; 
	    slot_list != NULL; slot_list = slot_list->next) {
	    designer_slot_t *slot = slot_list->data;
	    designer_node_t *src = slot->source;
	    node_data_t *nsd = node_data(src);

	    designer_slot_spec_t *src_spec = slot->output_slot_spec;
	    designer_slot_spec_t *dst_spec = slot->input_slot_spec;
	    slot_spec_data_t *ssd = slot_spec_data(src_spec);
	    slot_spec_data_t *dsd = slot_spec_data(dst_spec);

	    _point_t sp = _move(_move(nsd->or.o, _ptos(nsd->origin)), ssd->offset);
	    _point_t dp = _move(_move(ndd->ir.o, _ptos(ndd->origin)), dsd->offset);

	    draw_connect(cr, sp, dp, 0);
	}

	/* check for 'highest' node hit */
	_hit_t nht = hit_node(dst, data->mouse);
	if (nht) {
	    hn = dst;
	    hit = nht;
	}
    }

    /* update target info */
    switch (hit) {
    case HIT_INPUT:
	hs = hit_input_slot(hn, data->mouse);
	data->target = input_slot_origin(hn, hs);
	if (data->state == STATE_CONIN)
	    data->target_check = 0;
	else if (data->state == STATE_CONOUT &&
	    ((hn != data->target_node) || (hs != data->target_slot_id))) {
	    designer_connect_nodes_with_override(data->active_node,
		output_slot_spec(data->active_node, data->active_slot_id),
		hn, input_slot_spec(hn, hs),
		&data->target_check);
	}
	break;
    case HIT_OUTPUT:
	hs = hit_output_slot(hn, data->mouse);
	data->target = output_slot_origin(hn, hs);
	if (data->state == STATE_CONOUT)
	    data->target_check = 0;
	else if (data->state == STATE_CONIN &&
	    ((hn != data->target_node) || (hs != data->target_slot_id))) {
	    designer_connect_nodes_with_override(hn,
		output_slot_spec(hn, hs), data->active_node,
		input_slot_spec(data->active_node, data->active_slot_id),
		&data->target_check);
	}
	break;
    default:
	hs = -1;
	break;
    }
    data->target_node = hn;
    data->target_slot_id = hs;

    /* draw the nodes */
    for (GSList *list = data->design->nodes;
	list != NULL; list = list->next) {
	designer_node_t *node = list->data;

	draw_node(cr, node);
	if (node == hn)
	    draw_highlight(cr, node, data->mouse, hit,
		data->target_check);
    }

    if (data->state == STATE_CONIN) {
	draw_connect(cr, data->origin, data->mouse, 0);
    }
    if (data->state == STATE_CONOUT) {
	draw_connect(cr, data->origin, data->mouse, 0);
    }

    cairo_destroy(cr);


    return FALSE;
}



static gboolean
button_press_event (GtkWidget *widget, GdkEventButton *event)
{
    widget_data_t *data = get_widget_data(widget);

    data->mouse_down = _point(event->x, event->y);
    
    designer_node_t *hn = NULL;
    _hit_t ht = HIT_NOTHING; 

     /* check for best node hit */
    for (GSList *list = data->design->nodes;
	list != NULL; list = list->next) {
	designer_node_t *node = list->data;

	_hit_t nht = hit_node(node, data->mouse_down);
	if (nht) {
	    hn = node;
	    ht = nht;
	}
    }

    switch(ht) {
    case HIT_LABEL:
    case HIT_TITLE:
	data->active_node = hn;
	data->state = STATE_MOVING;
	data->origin = node_data(hn)->origin;
	designer_node_push_back(hn);
	break;
    case HIT_CLOSE:
	designer_disconnect_and_delete_node(hn);
	break;
    case HIT_INPUT:
	data->active_node = hn;
	data->active_slot_id = hit_input_slot(hn, data->mouse_down);
	data->origin = input_slot_origin(hn, data->active_slot_id);
	// g_print("hit %lf,%lf -> %d\n", event->x, event->y, data->active_slot_id);
	data->state = STATE_CONIN;
	break;
    case HIT_OUTPUT:
	data->active_node = hn;
	data->active_slot_id = hit_output_slot(hn, data->mouse_down);
	data->origin = output_slot_origin(hn, data->active_slot_id);
	// g_print("hit %lf,%lf -> %d\n", event->x, event->y, data->active_slot_Id);
	data->state = STATE_CONOUT;
	break;
    default:
	data->state = STATE_IDLE;
	break;
    }

    data->dragging = TRUE;

    gtk_widget_queue_draw(widget);
    return TRUE;
}

static gboolean
button_release_event (GtkWidget *widget, GdkEventButton *event)
{
    widget_data_t *data = get_widget_data(widget);

    data->mouse_up = _point(event->x, event->y);

    switch(data->state) {
    case STATE_MOVING:
	data->state = STATE_IDLE;
	break;
    case STATE_CONIN:
	if (data->target_check) {
	    designer_connect_nodes_with_override(
		data->target_node,
		output_slot_spec(data->target_node, data->target_slot_id),
		data->active_node,
		input_slot_spec(data->active_node, data->active_slot_id),
		NULL);
	}
	data->state = STATE_IDLE;
	break;
    case STATE_CONOUT:
	if (data->target_check) {
	    designer_connect_nodes_with_override(
		data->active_node,
		output_slot_spec(data->active_node, data->active_slot_id),
		data->target_node,
		input_slot_spec(data->target_node, data->target_slot_id),
		NULL);
	data->state = STATE_IDLE;
	}
	break;
    default:
	break;
    }

    /* clean up active/target */
    if (data->state == STATE_IDLE) {
	data->active_node = NULL;
	data->target_node = NULL;
	data->active_slot_id = -1;
	data->target_slot_id = -1;
	data->target_check = 0;
    }
    data->dragging = FALSE;

    gtk_widget_queue_draw(widget);
    return TRUE;
}

static gboolean
motion_notify_event (GtkWidget *widget, GdkEventMotion *event)
{
    widget_data_t *data = get_widget_data(widget);
    int x, y;
    GdkModifierType state;

    if (event->is_hint)
	gdk_window_get_pointer (event->window, &x, &y, &state);
    else
    {
	x = event->x;
	y = event->y;
	state = event->state;
    }

    data->mouse = _point(x, y);

    switch(data->state) {
    case STATE_MOVING:
	if (data->active_node) {
	    node_data(data->active_node)->origin =
		_move(data->origin,
		_delta(data->mouse_down, data->mouse));
	}
	break;
    case STATE_CONIN:
    
    
	break;
    case STATE_CONOUT:
	break;

    default:
	break;
    }

    gtk_widget_queue_draw(widget);
    return TRUE;
}



static void
populate_table (widget_data_t *data)
{
    GtkWidget *table = data->widget;
    GtkWidget *drawing_area, *w;

    drawing_area = gtk_layout_new(NULL, NULL);

    gtk_widget_show(drawing_area);
    data->drawing_area = drawing_area;

    gtk_table_attach (GTK_TABLE (table), drawing_area,
		      0, 1, 0, 1,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);

    w = gtk_hscrollbar_new (GTK_LAYOUT (drawing_area)->hadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      0, 1, 1, 2,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      GTK_FILL,
		      0, 0);
    gtk_widget_show (w);
    data->hscrollbar = w;

    w = gtk_vscrollbar_new (GTK_LAYOUT (drawing_area)->vadjustment);
    gtk_table_attach (GTK_TABLE (table), w,
		      1, 2, 0, 1,
		      GTK_FILL,
		      GTK_EXPAND | GTK_FILL | GTK_SHRINK,
		      0, 0);
    gtk_widget_show (w);
    data->vscrollbar = w;

    gtk_signal_connect(GTK_OBJECT(drawing_area), "expose_event",
		       (GtkSignalFunc)expose_event, NULL);
    gtk_signal_connect(GTK_OBJECT(drawing_area), "motion_notify_event",
		       (GtkSignalFunc)motion_notify_event, NULL);
    gtk_signal_connect(GTK_OBJECT(drawing_area), "button_press_event",
		       (GtkSignalFunc)button_press_event, NULL);
    gtk_signal_connect(GTK_OBJECT(drawing_area), "button_release_event",
		       (GtkSignalFunc)button_release_event, NULL);
    gtk_widget_set_events(drawing_area, GDK_EXPOSURE_MASK
			  | GDK_LEAVE_NOTIFY_MASK
			  | GDK_BUTTON_PRESS_MASK
			  | GDK_BUTTON_RELEASE_MASK
			  | GDK_POINTER_MOTION_MASK
			  | GDK_POINTER_MOTION_HINT_MASK);
}

GtkWidget *
designer_widget_new (designer_design_t *design,
		     designer_design_changed_callback_t design_changed_callback,
		     designer_node_focussed_callback_t node_focussed_callback)
{
    GtkWidget *table;
    widget_data_t *data;

    data = g_new0(widget_data_t, 1);
    data->design = design;
    data->design_changed_callback = design_changed_callback;
    data->node_focussed_callback = node_focussed_callback;

    table = gtk_table_new (2, 2, FALSE);
    gtk_table_set_row_spacings (GTK_TABLE (table), 4);
    gtk_table_set_col_spacings (GTK_TABLE (table), 4);
    gtk_widget_show (table);

    data->widget = table;

    populate_table(data);

    g_object_set_data(G_OBJECT(table), "designer-data", data);
    g_object_set_data(G_OBJECT(data->drawing_area), "designer-data", data);

    return table;
}

void
designer_widget_add_node (GtkWidget *widget, designer_node_t *node, double x, double y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p adds node %s at %gx%g\n", data, node->name, x, y);

    // designer_node_type_set_widget_data(node->type, NULL);
    // designer_node_set_widget_data(node, NULL);
}

void
designer_widget_set_design (GtkWidget *widget, designer_design_t *design)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p sets design to %p\n", data, design);

    data->design = design;
}

void
designer_widget_get_node_position (GtkWidget *widget, designer_node_t *node, double *x, double *y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p retrieves position of node %s\n", data, node->name);

    *x = *y = 0.0;
}

void
designer_widget_move_node (GtkWidget *widget, designer_node_t *node, double x, double y)
{
    widget_data_t *data = get_widget_data(widget);

    g_print("widget %p moves node %s to %gx%g\n", data, node->name, x, y);
}

void
designer_widget_design_loaded_callback (designer_design_t *design, gpointer user_data)
{
    GtkWidget *widget = GTK_WIDGET(user_data);
    widget_data_t *data = get_widget_data(widget);

    g_print("design loaded for widget %p\n", data);

    designer_widget_set_design(widget, design);
}

void
designer_widget_node_aux_load_callback (designer_node_t *node, lisp_object_t *obj, gpointer user_data)
{
    lisp_object_t *x = lisp_proplist_lookup_symbol(obj, ":x");
    lisp_object_t *y = lisp_proplist_lookup_symbol(obj, ":y");

    g_assert(lisp_number_p(x) && lisp_number_p(y));

    designer_widget_move_node(GTK_WIDGET(user_data), node, lisp_real(x), lisp_real(y));
}

void
designer_widget_node_aux_print (designer_node_t *node, gpointer user_data, FILE *out)
{
    double x, y;

    designer_widget_get_node_position(GTK_WIDGET(user_data), node, &x, &y);

    lisp_print_open_paren(out);

    lisp_print_symbol(":x", out);
    lisp_print_real(x, out);
    lisp_print_symbol(":y", out);
    lisp_print_real(y, out);

    lisp_print_close_paren(out);
}



/*
	designer_disconnect_slot();
	designer_connect_nodes();
	designer_connect_nodes_with_override();
*/
