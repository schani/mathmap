/* -*- c -*- */

/*
 * mathmap_cmdline.c
 *
 * MathMap
 *
 * Copyright (C) 1997-2000 Mark Probst
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

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "getopt.h"

#include "exprtree.h"
#include "builtins.h"
#include "postfix.h"
#include "tags.h"
#include "scanner.h"
#include "vars.h"
#include "userval.h"
#include "internals.h"
#include "macros.h"
#include "jump.h"
#include "mathmap.h"
#include "noise.h"
#ifdef USE_CGEN
#include "cgen.h"
#endif
#include "mathmap_common.h"
#include "readimage.h"
#include "writeimage.h"

#ifdef USE_CGEN
#define EVAL_EXPR     eval_c_code
#else
#define EVAL_EXPR     eval_postfix
#endif

extern int mmparse (void);

typedef struct {
    guchar *data;
    gint bpp;
    int used;
} input_drawable_t;

#define MAX_INPUT_DRAWABLES 64

static input_drawable_t input_drawables[MAX_INPUT_DRAWABLES];

exprtree *theExprtree = 0;

char error_string[1024];

int img_width, img_height;

guchar edge_color[4] = { 0, 0, 0, 0 };

int num_gradient_samples = 1;
tuple_t gradient_samples[1];

void
mathmap_get_pixel (int drawable_index, int x, int y, guchar *pixel)
{
    guchar *p;
    int i;
    input_drawable_t *drawable;

    if (drawable_index < 0 || drawable_index >= MAX_INPUT_DRAWABLES || !input_drawables[drawable_index].used
	|| x < 0 || x >= img_width
	|| y < 0 || y >= img_height)
    {
	for (i = 0; i < outputBPP; ++i)
	    pixel[i] = edge_color[i];
	return;
    }

    drawable = &input_drawables[drawable_index];

    p = drawable->data + drawable->bpp * (img_width * y + x);

    if (drawable->bpp == 1 || drawable->bpp == 2)
	pixel[0] = pixel[1] = pixel[2] = p[0];
    else if (drawable->bpp == 3 || drawable->bpp == 4)
	for (i = 0; i < 3; ++i)
	    pixel[i] = p[i];
    else
	assert(0);

    if (drawable->bpp == 1 || drawable->bpp == 3)
	pixel[3] = 255;
    else
	pixel[3] = p[drawable->bpp - 1];
}

int
generate_code (char *expr)
{
    static int result;

    result = 1;

    untag_uservals();

    DO_JUMP_CODE {
	clear_all_variables();
	internals_clear_used();
	scanFromString(expr);
	mmparse();
	endScanningFromString();

	assert(theExprtree != 0);

	if (theExprtree->result.number != rgba_tag_number || theExprtree->result.length != 4)
	{
	    sprintf(error_string, "The expression must have the result type rgba:4.");
	    JUMP(1);
	}

#ifdef USE_CGEN
	assert(gen_and_load_c_code(theExprtree));
#else
	make_postfix(theExprtree);
	output_postfix();
#endif
    } WITH_JUMP_HANDLER {
	result = 0;
    } END_JUMP_HANDLER;

    clear_untagged_uservals();
    untag_uservals();

    update_image_internals();

    return result;
}

void
usage (void)
{
    printf("Usage:\n"
	   "  mathmap --version\n"
	   "      print out version number\n"
	   "  mathmap --help\n"
	   "      print this help text\n"
	   "  mathmap [option ...] <expression> <outimage> <inimage> ...\n"
	   "      transform one or more <inimage>s with <expression> and write\n"
	   "      the result image to <outimage>\n"
	   "Options:\n"
	   "  -i, --intersampling         use intersampling\n"
	   "  -o, --oversampling          use oversampling\n"
	   "\n"
	   "Report bug reports and suggestions to schani@complang.tuwien.ac.at\n");
}

int
main (int argc, char *argv[])
{
    guchar *output, *dest;
    int row, col;
    int i;

    intersamplingEnabled = 0;
    oversamplingEnabled = 0;

    for (;;)
    {
	static struct option long_options[] =
	    {
		{ "version", no_argument, 0, 256 },
		{ "help", no_argument, 0, 257 },
		{ "intersampling", no_argument, 0, 'i' },
		{ "oversampling", no_argument, 0, 'o' },
		{ 0, 0, 0, 0 }
	    };

	int option, option_index;

	option = getopt_long(argc, argv, "io", long_options, &option_index);

	if (option == -1)
	    break;

	switch (option)
	{
	    case 256 :
		printf("MathMap " MATHMAP_VERSION "\n"
		       "\n"
		       "Copyright (C) 1997-2000 Mark Probst\n"
		       "\n"
		       "This program is free software; you can redistribute it and/or modify\n"
		       "it under the terms of the GNU General Public License as published by\n"
		       "the Free Software Foundation; either version 2 of the License, or\n"
		       "(at your option) any later version.\n"
		       "\n"
		       "This program is distributed in the hope that it will be useful,\n"
		       "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
		       "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
		       "GNU General Public License for more details.\n"
		       "\n"
		       "You should have received a copy of the GNU General Public License\n"
		       "along with this program; if not, write to the Free Software\n"
		       "Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n");
		return 0;

	    case 257 :
		usage();
		return 0;

	    case 'i' :
		intersamplingEnabled = 1;
		break;

	    case 'o' :
		oversamplingEnabled = 1;
		break;
	}
    }

    if (argc - optind < 3)
    {
	usage();
	return 1;
    }

    init_builtins();
    init_tags();
    init_internals();
    init_macros();
    init_noise();

    for (i = 0; i < argc - optind - 2; ++i)
    {
	int width, height;

	input_drawables[i].data = read_image(argv[optind + i + 1], &width, &height);
	assert(input_drawables[i].data != 0);

	if (i == 0)
	{
	    img_width = width;
	    img_height = height;
	}
	else
	{
	    assert(img_width == width && img_height == height);
	}

	input_drawables[i].bpp = 3;
	input_drawables[i].used = 1;
    }

    outputBPP = 3;
    originX = originY = 0;

    imageW = img_width;
    imageH = img_height;

    middleX = img_width / 2.0;
    middleY = img_height / 2.0;

    if (middleX > img_width - middleX)
	imageX = middleX;
    else
	imageX = img_width - middleX;

    if (middleY > img_height - middleY)
	imageY = middleY;
    else
	imageY = img_height - middleY;
    
    imageR = hypot(imageX, imageY);

    currentT = 0.0;

    if (!generate_code(argv[optind]))
    {
	printf("%s\n", error_string);
	exit(1);
    }

    output = (guchar*)malloc(outputBPP * img_width * img_height);
    assert(output != 0);

    dest = output;

    if (oversamplingEnabled)
    {
	guchar *line1, *line2, *line3;

	line1 = (guchar*)malloc((img_width + 1) * outputBPP);
	line2 = (guchar*)malloc(img_width * outputBPP);
	line3 = (guchar*)malloc((img_width + 1) * outputBPP);

	for (col = 0; col <= img_width; ++col)
	{
	    currentX = col - middleX;
	    currentY = middleY;
	    calc_ra();
	    update_pixel_internals();
	    write_tuple_to_pixel(EVAL_EXPR(), line1 + col * outputBPP);
	}

	for (row = 0; row < img_height; ++row)
	{
	    for (col = 0; col < img_width; ++col)
	    {
		currentX = col + 0.5 - middleX;
		currentY = -(row + 0.5 - middleY);
		calc_ra();
		update_pixel_internals();
		write_tuple_to_pixel(EVAL_EXPR(), line2 + col * outputBPP);
	    }
	    for (col = 0; col <= img_width; ++col)
	    {
		currentX = col - middleX;
		currentY = -(row + 1.0 - middleY);
		calc_ra();
		update_pixel_internals();
		write_tuple_to_pixel(EVAL_EXPR(), line3 + col * outputBPP);
	    }
	    
	    for (col = 0; col < img_width; ++col)
	    {
		int i;

		for (i = 0; i < outputBPP; ++i)
		    dest[i] = (line1[col*outputBPP+i]
			       + line1[(col+1)*outputBPP+i]
			       + 2*line2[col*outputBPP+i]
			       + line3[col*outputBPP+i]
			       + line3[(col+1)*outputBPP+i]) / 6;
		dest += outputBPP;
	    }

	    memcpy(line1, line3, (img_width + 1) * outputBPP);
	}
    }
    else
    {
	for (row = 0; row < img_height; row++)
	{
	    for (col = 0; col < img_width; col++)
	    {
		currentX = col - middleX;
		currentY = -(row - middleY);
		calc_ra();
		update_pixel_internals();
		write_tuple_to_pixel(EVAL_EXPR(), dest);
		dest += outputBPP;
	    }
	}
    }

    write_image(argv[argc - 1], img_width, img_height, output, IMAGE_FORMAT_PNG);

    return 0;
}
