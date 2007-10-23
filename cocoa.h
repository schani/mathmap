/*
 *  cocoa.h
 *  MathMap
 *
 *  Created by Herbert Poetzl on 10/23/07.
 *  Copyright (c) 2007 Herbert Poetzl. All rights reserved.
 *
 */

#include "mathmap.h"

mathmap_t* compile_mathmap_cocoa (const char *expression, char *template_filename, char *opmacros_filename);

void call_invocation(mathmap_invocation_t *invocation, int first_row, int last_row, unsigned char *output);
