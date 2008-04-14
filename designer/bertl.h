


#define	min(a,b)	(((a) < (b)) ? (a) : (b))
#define	max(a,b)	(((a) > (b)) ? (a) : (b))


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
	return _point(min(p1.x, p2.x), min(p1.y, p2.y));
}

static inline
_point_t _pmax(_point_t p1, _point_t p2)
{
	return _point(max(p1.x, p2.x), max(p1.y, p2.y));
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
	return _size(min(s1.w, s2.w), min(s1.h, s2.h));
}

static inline
_size_t _smax(_size_t s1, _size_t s2)
{
	return _size(max(s1.w, s2.w), max(s1.h, s2.h));
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
	double mw = min(w, r->s.w);
	_rect_t s = _rect(r->o.x, r->o.y, mw, r->s.h);

	r->o.x += mw; r->s.w -= mw;
	return s;
}

static inline
_rect_t _splitv(_rect_t *r, double h)
{
	double mh = min(h, r->s.h);
	_rect_t s = _rect(r->o.x, r->o.y, r->s.w, mh);

	r->o.y += mh; r->s.h -= mh;
	return s;
}


