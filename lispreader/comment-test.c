/* $Id: comment-test.c 187 2000-07-09 21:08:28Z schani $ */
/*
 * comment_test.c
 *
 * Copyright (C) 2000 Masatake YAMATO <jet@gyve.org>
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

#include <lispreader.h>

int 
main (void)
{
    char * script = 
	"; This is comments(1)\n"
	"\"This is script (1)\"\n"
	"; This is comments(2)\n" 
	"\"This is script (2)\"\n"
	"; This is comments(3)" ;
    
    lisp_object_t *obj;
    lisp_stream_t stream;

    if (NULL == lisp_stream_init_string(&stream, script))
    {
	fprintf(stderr, "Fail in lisp_stream_init_string\n");
	return 1;
    }

    while (1)
    {
	int type;

	obj = lisp_read(&stream);

	type = lisp_type(obj);
      
	if (type == LISP_TYPE_STRING)
	{
	    fprintf(stderr, "->%s\n", lisp_string(obj));
	    lisp_free(obj);
	}
	else if (type == LISP_TYPE_PARSE_ERROR)
	{
	    printf("parse error\n");
	    lisp_free(obj);
	}
	else if (type == LISP_TYPE_EOF)
	{
	    printf ("eof\n");
	    lisp_free(obj);
	    break;
	}
	else 
	    printf ("wrong type\n");
    }
    return 0;
}
