/*
 *  cocoa.c
 *  MathMap
 *
 *  Created by Herbert Poetzl on 10/23/07.
 *  Copyright (c) 2007 Herbert Poetzl. All rights reserved.
 *
 */

#include "cocoa.h"

mathmap_t* compile_mathmap_cocoa (const char *expression, char *template_filename, char *opmacros_filename)
{
    FILE *template = fopen(template_filename, "r");
	mathmap_t *ret;
    
	ret = compile_mathmap ((char *)expression, template, opmacros_filename);
    fclose(template);
	return ret;
}


void call_invocation(mathmap_invocation_t *invocation, int first_row, int last_row, unsigned char *output)
{
	call_invocation_parallel (invocation, 0, first_row, invocation->img_width, last_row, output, 1);
}