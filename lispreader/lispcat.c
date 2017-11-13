/*
 * lispcat.c
 *
 * Copyright (C) 2004 Mark Probst <schani@complang.tuwien.ac.at>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>

#include <lispreader.h>
#include <pools.h>

int 
main (int argc, char *argv[])
{
    lisp_object_t *obj;
    lisp_stream_t stream;
    pools_t pools;
    allocator_t allocator;
    int do_dump = 1;
    char *filename = 0;

    assert(argc == 1 || argc == 2 || argc == 3);

    if (argc > 1 && strcmp(argv[1], "--null") == 0)
    {
	do_dump = 0;
	if (argc > 2)
	    filename = argv[2];
    }
    else
    {
	assert(argc < 3);
	if (argc == 2)
	    filename = argv[1];
    }

    if (filename == 0)
    {
	if (lisp_stream_init_file(&stream, stdin) == 0)
	{
	    fprintf(stderr, "could not init file stream\n");
	    return 1;
	}
    }
    else
    {
	if (lisp_stream_init_path(&stream, filename) == 0)
	{
	    fprintf(stderr, "could not init path stream\n");
	    return 1;
	}
    }

    init_pools(&pools);
    init_pools_allocator(&allocator, &pools);

    for (;;)
    {
	reset_pools(&pools);
	obj = lisp_read_with_allocator(&allocator, &stream);

	switch (lisp_type(obj))
	{
	    case LISP_TYPE_EOF :
		goto done;

	    case LISP_TYPE_PARSE_ERROR :
		fprintf(stderr, "parse error\n");
		return 1;

	    default :
		if (do_dump)
		{
		    lisp_dump(obj, stdout);
		    fputc('\n', stdout);
		}
	}
    }

 done:
    free_pools(&pools);

    if (filename != 0)
	lisp_stream_free_path(&stream);

    return 0;
}
