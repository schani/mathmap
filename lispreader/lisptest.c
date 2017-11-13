/* $Id: lisptest.c 191 2004-07-02 21:20:49Z schani $ */

#include "lispreader.h"

static lisp_object_t*
make_fib_tree (int n)
{
    if (n < 2)
	return lisp_nil();

    return lisp_make_cons(make_fib_tree(n - 1), make_fib_tree(n - 2));
}

static void
free_test (void)
{
    int i;

    for (i = 0; i < 50; ++i)
    {
	lisp_object_t *obj = make_fib_tree(25);

	lisp_free(obj);
    }
}

int
main (void)
{
    lisp_object_t *obj;
    lisp_stream_t stream;

    lisp_dump(make_fib_tree(5), stdout);
    printf("\n");

    free_test();

    lisp_stream_init_file(&stream, stdin);

    while (1)
    {
	obj = lisp_read(&stream);
	if (obj == 0 || lisp_type(obj) != LISP_TYPE_EOF)
	{
	    lisp_object_t *vars[5];

	    lisp_dump(obj, stdout);
	    fprintf(stdout, "\n");

	    if (lisp_match_string("(beidel #?(or (heusl #?(integer)) #?(string)) #?(boolean) . #?(list))",
				  obj, vars))
	    {
		lisp_dump(vars[0], stdout);
		fprintf(stdout, "\n");
		lisp_dump(vars[1], stdout);
		fprintf(stdout, "\n");
		lisp_dump(vars[2], stdout);
		fprintf(stdout, "\n");
		lisp_dump(vars[3], stdout);
		fprintf(stdout, "\n");
		lisp_dump(vars[4], stdout);
		fprintf(stdout, "\n");
	    }

	    lisp_free(obj);
	}
	else
	    break;
    }

    return 0;
}
