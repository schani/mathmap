#ifndef __CGEN_H__
#define __CGEN_H__

#include <glib.h>

#include "tuples.h"
#include "exprtree.h"

typedef tuple_t* (*mathfunc_t) (void);

extern mathfunc_t eval_c_code;

gboolean gen_and_load_c_code (exprtree *tree);

#endif
