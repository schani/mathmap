/* -*- c -*- */

/*
 * cairo_widget.h
 *
 * MathMap
 *
 * Copyright (C) 2008 Herbert Poetzl
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

#ifndef __CAIRO_WIDGET_H__
#define __CAIRO_WIDGET_H__

#include <glib.h>


// #define	RGBA_TITLE_AREA		0.3, 1.0, 0.8, 0.8
#define	RGBA_TITLE_AREA		1.0, 0.9, 0.2, 0.8
#define	RGBA_TITLE_TEXT		0.0, 0.0, 0.0, 0.8

#define	RGBA_BODY_AREA		1.0, 1.0, 1.0, 0.7

// #define	RGBA_ROOT_NODE		0.4, 0.4, 0.4, 0.7
// #define	RGBA_ROOT_NODE		0.8, 0.0, 0.1, 0.3
#define	RGBA_ROOT_NODE		0.0, 0.0, 0.0, 0.7

#define	RGBA_DROP_SHADOW	0.0, 0.0, 0.0, 0.1

#define	RGBA_BLOCKED		0.5, 0.5, 0.7, 0.5
#define	RGBA_HIGHLIGHT		0.2, 0.5, 1.0, 0.4

#define	RGBA_CONNECT		0.4, 0.0, 0.0, 0.6
#define	RGBA_CONNECT_HL		1.0, 0.6, 0.6, 0.6


typedef struct _point
{
	double x;
	double y;
} _point_t;

const _point_t _zerop = { 0 };

typedef struct _size
{
	double w;
	double h;
} _size_t;

const _size_t _zeros = { 0 };

typedef struct _rect
{
	_point_t o;
	_size_t s;
} _rect_t;

const _rect_t _zeror = { { 0 } };

typedef	union _corn
{
    struct {
	_point_t ll;
	_point_t lr;
	_point_t ur;
	_point_t ul;
    };
    _point_t c[4];
} _corn_t;




static inline
_point_t _point(double x, double y)
{
	_point_t p = { .x = x, .y = y };
	return p;
}

static inline
_size_t _size(double w, double h)
{
	_size_t s = { .w = w, .h = h };
	return s;
}

static inline
_rect_t _rect(double x, double y, double w, double h)
{
	_rect_t r = { .o = _point(x, y), .s = _size(w, h) };
	return r;
}



static inline
int _eqp(_point_t p1, _point_t p2)
{
	return (p1.x == p2.x) && (p1.y == p2.y);
}

static inline
int _eqs(_size_t s1, _size_t s2)
{
	return (s1.w == s2.w) && (s1.h == s2.h);
}

static inline
int _eqr(_rect_t r1, _rect_t r2)
{
	return _eqp(r1.o, r2.o) && _eqs(r1.s, r2.s);
}



static inline
_size_t _delta(_point_t p1, _point_t p2)
{
	return _size(p2.x - p1.x, p2.y - p1.y);
}

static inline
_point_t _move(_point_t p, _size_t s)
{
	return _point(p.x + s.w, p.y + s.h);
}

static inline
_size_t _ptos(_point_t p)
{
	return _size(p.x, p.y);
}

static inline
_point_t _stop(_size_t s)
{
	return _point(s.w, s.h);
}

static inline
_size_t _ptoo(_point_t p)
{
	return _size(-p.x, -p.y);
}

static inline
_point_t _midp(_point_t p1, _point_t p2)
{
	return _point((p1.x + p2.x)/2, (p1.y + p2.y)/2);
}

static inline
_point_t _mixp(_point_t p1, _point_t p2, double m)
{
	return _point(p1.x*m + p2.x*(1.0-m),
		p1.y*m + p2.y*(1.0-m));
}

static inline
_point_t _rotate(_point_t p, double a)
{
	return _point(p.x * cos(a) + p.y * sin(a),
		p.y * cos(a) - p.x * sin(a));
}





static inline
_corn_t _corners(_rect_t r)
{
	_corn_t c;

	c.ll = r.o;
	c.ur = _move(c.ll, r.s);
	c.lr = _point(c.ur.x, c.ll.y);
	c.ul = _point(c.ll.x, c.ur.y);
	return c;
}



static inline
_point_t _pmin(_point_t p1, _point_t p2)
{
	return _point(MIN(p1.x, p2.x), MIN(p1.y, p2.y));
}

static inline
_point_t _pmax(_point_t p1, _point_t p2)
{
	return _point(MAX(p1.x, p2.x), MAX(p1.y, p2.y));
}


static inline
_rect_t _prect(_point_t p1, _point_t p2)
{
	_rect_t r;

	r.o = _pmin(p1, p2);
	r.s = _delta(r.o, _pmax(p1, p2));
	return r;
}



static inline
_size_t _smin(_size_t s1, _size_t s2)
{
	return _size(MIN(s1.w, s2.w), MIN(s1.h, s2.h));
}

static inline
_size_t _smax(_size_t s1, _size_t s2)
{
	return _size(MAX(s1.w, s2.w), MAX(s1.h, s2.h));
}


static inline
double sign(double value) { 
	return (value > 0) ? 1.0 : ((value < 0) ? -1.0 : 0.0);
}

static inline
_size_t _ssmin(_size_t s1, _size_t s2)
{
	double sgw = sign(s1.w);
	double sgh = sign(s1.h);

	return _size(sgw*MIN(fabs(s1.w), fabs(s2.w)), sgh*MIN(fabs(s1.h), fabs(s2.h)));
}

static inline
_size_t _ssmax(_size_t s1, _size_t s2)
{
	double sgw = sign(s1.w);
	double sgh = sign(s1.h);

	return _size(sgw*MAX(fabs(s1.w), fabs(s2.w)), sgh*MAX(fabs(s1.h), fabs(s2.h)));
}

static inline
_size_t _smult(_size_t s, double f)
{
	return _size(s.w*f, s.h*f);
}


static inline
_size_t _sshrink(_size_t s1, _size_t s2)
{
	if (s2.w && s1.w && s2.w < s1.w)
		s1 = _size(s2.w, s1.h*s2.w/s1.w);
	if (s2.h && s1.h && s2.h < s1.h)
		s1 = _size(s1.w*s2.h/s1.h, s2.h);
	return s1;
}

static inline
_size_t _sgrow(_size_t s1, _size_t s2)
{
	if (s2.w && s1.w && s2.w > s1.w)
		s1 = _size(s2.w, s1.h*s2.w/s1.w);
	if (s2.h && s1.h && s2.h > s1.h)
		s1 = _size(s1.w*s2.h/s1.h, s2.h);
	return s1;
}


static inline
int _inrect(_point_t p, _rect_t r)
{
	return ((p.x >= r.o.x) &&
		(p.y >= r.o.y) &&
		(p.x < r.o.x + r.s.w) &&
		(p.y < r.o.y + r.s.h));
}

static inline
int _isreal(_rect_t r)
{
	return ((r.s.w > 0) && (r.s.h > 0));
}




static inline
_rect_t _union(_rect_t r1, _rect_t r2)
{
	_corn_t c1 = _corners(r1);
	_corn_t c2 = _corners(r2);
	
	return _prect(_pmin(c1.ll, c2.ll), _pmax(c1.ur, c2.ur));
}

static inline
_rect_t _isect(_rect_t r1, _rect_t r2)
{
	_corn_t c1 = _corners(r1);
	_corn_t c2 = _corners(r2);
	
	if ((c1.ll.x > c2.ur.x) ||
	    (c2.ll.x > c1.ur.x) ||
	    (c1.ll.y > c2.ur.y) ||
	    (c2.ll.y > c1.ur.y))
	    return _zeror;
	return _prect(_pmax(c1.ll, c2.ll), _pmin(c1.ur, c2.ur));
}

static inline
_rect_t _norm(_rect_t r)
{
	if (r.s.w < 0) {
		r.o.x -= r.s.w;
		r.s.w *= -1;
	}
	if (r.s.h < 0) {
		r.o.y -= r.s.h;
		r.s.h *= -1;
	}
	return r;
}

static inline
_rect_t _inset(_rect_t r, _size_t s)
{
	return _rect(
		r.o.x + s.w,
		r.o.y + s.h,
		r.s.w - 2*s.w,
		r.s.h - 2*s.h);
}

static inline
_rect_t _offset(_rect_t r, _size_t s)
{
	return _rect(
		r.o.x + s.w,
		r.o.y + s.h,
		r.s.w, r.s.h);
}



static inline
_rect_t _splith(_rect_t *r, double w)
{
	double mw = MIN(w, r->s.w);
	_rect_t s = _rect(r->o.x, r->o.y, mw, r->s.h);

	r->o.x += mw; r->s.w -= mw;
	return s;
}

static inline
_rect_t _splitv(_rect_t *r, double h)
{
	double mh = MIN(h, r->s.h);
	_rect_t s = _rect(r->o.x, r->o.y, r->s.w, mh);

	r->o.y += mh; r->s.h -= mh;
	return s;
}



#endif
