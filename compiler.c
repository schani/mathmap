#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <glib.h>

#include "vars.h"
#include "internals.h"
#include "tags.h"
#include "exprtree.h"

void
mathmap_get_pixel (int drawable_index, int frame, int x, int y, guchar *pixel)
{
}


#define TYPE_FLOAT           1
#define TYPE_INT             2
#define TYPE_COLOR           3

typedef struct
{
    int type;
    int number;
    value_t *current;
    int last_index;
} temporary_t;

struct _statement_list_t;

struct _value_t
{
    variable_t *var;		/* 0 if value is a temporary */
    temporary_t *temp;		/* 0 if value is a variable */
    int n;			/* n/a if value is a temporary */
    int index;			/* SSA index */
    struct _statement_list_t *uses;
};

#define RHS_VALUE            1
#define RHS_INTERNAL         2
#define RHS_OP               3
#define RHS_INT_CONST        4
#define RHS_FLOAT_CONST      5

#define MAX_OP_ARGS          3

typedef int operation_t;	/* FIXME: dummy */

typedef struct
{
    int type;
    union
    {
	value_t *value;
	struct
	{
	    internal_t *internal;
	    int n;
	} internal;
	struct
	{
	    operation_t *op;
	    value_t *args[MAX_OP_ARGS];
	} op;
	int int_const;
	float float_const;
    } v;
} rhs_t;

#define STMT_NIL             0
#define STMT_ASSIGN          1
#define STMT_PHI_ASSIGN      2
#define STMT_IF_COND         3
#define STMT_WHILE_LOOP      4

typedef struct _statement_t
{
    int type;
    int index;
    union
    {
	struct
	{
	    value_t *lhs;
	    rhs_t *rhs;
	    rhs_t *rhs2;	/* only valid for STMT_PHI_ASSIGN */
	    value_t *old_value;	/* only valid for STMT_PHI_ASSIGN */
	} assign;
	struct
	{
	    rhs_t *condition;
	    struct _statement_t *consequent;
	    struct _statement_t *alternative;
	} if_cond;
	struct
	{
	    struct _statement_t *entry;
	    rhs_t *invariant;
	    struct _statement_t *body;
	} while_loop;
    } v;
    struct _statement_t *next;
} statement_t;

typedef struct _statement_list_t
{
    statement_t *stmt;
    struct _statement_list_t *next;
} statement_list_t;

static int next_stmt_index = 0;
static int next_temp_number = 1;

static statement_t *first_stmt = 0;
static statement_t **emit_loc = &first_stmt;

#define STMT_STACK_SIZE            64

static statement_t *stmt_stack[STMT_STACK_SIZE];
static int stmt_stackp = 0;

#define alloc_stmt()               ((statement_t*)malloc(sizeof(statement_t)))
#define alloc_value()              ((value_t*)malloc(sizeof(value_t)))
#define alloc_rhs()                ((rhs_t*)malloc(sizeof(rhs_t)))

temporary_t*
make_temporary (int type)
{
    temporary_t *temp = (temporary_t*)malloc(sizeof(temporary_t));

    temp->type = type;
    temp->number = next_temp_number++;
    temp->current = 0;
    temp->last_index = 0;

    return temp;
}

value_t*
make_variable_lhs (variable_t *var, int n)
{
    value_t *val = alloc_value();

    val->var = var;
    val->temp = 0;
    val->n = n;
    val->index = -1;
    val->uses = 0;

    return val;
}

value_t*
make_temporary_lhs (temporary_t *temp)
{
    value_t *val = alloc_value();

    val->var = 0;
    val->temp = temp;
    val->index = -1;
    val->uses = 0;

    return val;
}

void
add_use (value_t *val, statement_t *stmt)
{
    statement_list_t *lst = (statement_list_t*)malloc(sizeof(statement_list_t));

    lst->stmt = stmt;
    lst->next = val->uses;
    val->uses = lst;
}

void
remove_use (value_t *val, statement_t *stmt)
{
    statement_list_t **lst = &val->uses;

    while (*lst != 0)
    {
	statement_list_t *elem = *lst;

	if (elem->stmt == stmt)
	{
	    *lst = elem->next;

	    free(elem);

	    return;
	}
    }

    assert(0);
}

value_t*
current_value (value_t *val)
{
    if (val->var != 0)
	return val->var->current[val->n];
    else
	return val->temp->current;
}

void
assign_value_index_and_make_current (value_t *val)
{
    if (val->var != 0)
    {
	val->index = ++val->var->last_index[val->n];
	val->var->current[val->n] = val;
    }
    else
    {
	val->index = ++val->temp->last_index;
	val->temp->current = val;
    }
}

value_t*
make_value_copy (value_t *val)
{
    if (val->var != 0)
	return make_variable_lhs(val->var, val->n);
    else
	return make_temporary_lhs(val->temp);
}

void
set_value_current (value_t *val, value_t *new_current)
{
    if (val->var != 0)
	val->var->current[val->n] = new_current;
    else
	val->temp->current = new_current;
}

rhs_t*
make_int_const_rhs (int int_const)
{
    rhs_t *rhs = alloc_rhs();

    rhs->type = RHS_INT_CONST;
    rhs->v.int_const = int_const;

    return rhs;
}

rhs_t*
make_value_rhs (value_t *val)
{
    rhs_t *rhs = alloc_rhs();

    rhs->type = RHS_VALUE;
    rhs->v.value = val;

    return rhs;
}

rhs_t*
make_variable_rhs (variable_t *var, int n)
{
    assert(var->current[n] != 0);

    return make_value_rhs(var->current[n]);
}

rhs_t*
make_temporary_rhs (temporary_t *temp)
{
    assert(temp->current != 0);

    return make_value_rhs(temp->current);
}

void
free_rhs (rhs_t *rhs)
{
    /* FIXME */
}

statement_t*
find_phi_assign (statement_t *stmts, value_t *val)
{
    for (; stmts != 0; stmts = stmts->next)
    {
	assert(stmts->type == STMT_PHI_ASSIGN);

	if ((val->var != 0 && stmts->v.assign.lhs->var == val->var)
	    || (val->temp != 0 && stmts->v.assign.lhs->temp == val->temp))
	    return stmts;
    }

    return 0;
}

void
rewrite_uses (value_t *old, value_t *new, int start_index)
{
    statement_list_t **lst;

    assert(old != new);

    lst = &old->uses;
    while (*lst != 0)
    {
	statement_list_t *elem = *lst;
	statement_t *stmt = elem->stmt;
	rhs_t *rhs;

	if (stmt->index >= start_index)
	{
	    switch (stmt->type)
	    {
		case STMT_ASSIGN :
		    rhs = stmt->v.assign.rhs;
		    break;

		case STMT_PHI_ASSIGN :
		    if (stmt->v.assign.rhs->type == RHS_VALUE
			&& stmt->v.assign.rhs->v.value == old)
			rhs = stmt->v.assign.rhs;
		    else
			rhs = stmt->v.assign.rhs2;
		    break;

		case STMT_IF_COND :
		    rhs = stmt->v.if_cond.condition;
		    break;

		case STMT_WHILE_LOOP :
		    rhs = stmt->v.while_loop.invariant;
		    break;

		default :
		    assert(0);
	    }

	    assert(stmt->v.assign.rhs->type == RHS_VALUE
		   && stmt->v.assign.rhs->v.value == old);

	    stmt->v.assign.rhs->v.value = new;
	    add_use(new, stmt);

	    *lst = elem->next;

	    free(elem);
	}
	else
	    lst = &elem->next;
    }
}

void
commit_assign (statement_t *stmt)
{
    statement_t *tos;

    if (stmt_stackp > 0)
    {
	tos = stmt_stack[stmt_stackp - 1];

	switch (tos->type)
	{
	    case STMT_IF_COND :
		{
		    statement_t *phi_assign = find_phi_assign(tos->next, stmt->v.assign.lhs);

		    if (phi_assign == 0)
		    {
			phi_assign = alloc_stmt();

			phi_assign->type = STMT_PHI_ASSIGN;

			phi_assign->v.assign.lhs = make_value_copy(stmt->v.assign.lhs);

			phi_assign->v.assign.rhs = make_value_rhs(current_value(stmt->v.assign.lhs));
			add_use(current_value(stmt->v.assign.lhs), phi_assign);

			phi_assign->v.assign.rhs2 = make_value_rhs(current_value(stmt->v.assign.lhs));
			add_use(current_value(stmt->v.assign.lhs), phi_assign);

			phi_assign->v.assign.old_value = current_value(stmt->v.assign.lhs);

			phi_assign->next = tos->next;
			tos->next = phi_assign;
		    }

		    if (tos->v.if_cond.alternative == 0)
		    {
			assert(phi_assign->v.assign.rhs->type = RHS_VALUE);
			remove_use(phi_assign->v.assign.rhs->v.value, phi_assign);
			free_rhs(phi_assign->v.assign.rhs);

			phi_assign->v.assign.rhs = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		    else
		    {
			assert(phi_assign->v.assign.rhs2->type = RHS_VALUE);
			remove_use(phi_assign->v.assign.rhs2->v.value, phi_assign);
			free_rhs(phi_assign->v.assign.rhs2);

			phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
			add_use(stmt->v.assign.lhs, phi_assign);
		    }
		}
		break;

	    case STMT_WHILE_LOOP :
		{
		    statement_t *phi_assign = find_phi_assign(tos->v.while_loop.entry, stmt->v.assign.lhs);

		    if (phi_assign == 0)
		    {
			phi_assign = alloc_stmt();

			phi_assign->type = STMT_PHI_ASSIGN;

			phi_assign->v.assign.lhs = make_value_copy(stmt->v.assign.lhs);

			phi_assign->v.assign.rhs = make_value_rhs(current_value(stmt->v.assign.lhs));
			add_use(current_value(stmt->v.assign.lhs), phi_assign);

			phi_assign->next = tos->v.while_loop.entry;
			tos->v.while_loop.entry = phi_assign;
		    }
		    else
		    {
			assert(phi_assign->v.assign.rhs2->type = RHS_VALUE);
			remove_use(phi_assign->v.assign.rhs2->v.value, phi_assign);
			free_rhs(phi_assign->v.assign.rhs2);
		    }

		    phi_assign->v.assign.rhs2 = make_value_rhs(stmt->v.assign.lhs);
		    add_use(stmt->v.assign.lhs, phi_assign);

		    rewrite_uses(current_value(stmt->v.assign.lhs), phi_assign->v.assign.lhs, tos->index);
		}
		break;

	    default :
		assert(0);
	}
    }

    assign_value_index_and_make_current(stmt->v.assign.lhs);
}

void
emit_stmt (statement_t *stmt)
{
    stmt->index = next_stmt_index++;

    *emit_loc = stmt;
    emit_loc = &stmt->next;

    switch (stmt->type)
    {
	case STMT_ASSIGN :
	    if (stmt->v.assign.rhs->type == RHS_VALUE)
		add_use(stmt->v.assign.rhs->v.value, stmt);
	    break;

	case STMT_PHI_ASSIGN :
	    if (stmt->v.assign.rhs->type == RHS_VALUE)
		add_use(stmt->v.assign.rhs->v.value, stmt);
	    if (stmt->v.assign.rhs2->type == RHS_VALUE)
		add_use(stmt->v.assign.rhs2->v.value, stmt);
	    break;

	case STMT_IF_COND :
	    if (stmt->v.if_cond.condition->type == RHS_VALUE)
		add_use(stmt->v.if_cond.condition->v.value, stmt);
	    break;

	case STMT_WHILE_LOOP :
	    if (stmt->v.while_loop.invariant->type == RHS_VALUE)
		add_use(stmt->v.while_loop.invariant->v.value, stmt);
	    break;
    }
}

void
emit_nil (void)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_NIL;
    stmt->next = 0;

    emit_stmt(stmt);
}

void
emit_assign (value_t *lhs, rhs_t *rhs)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_ASSIGN;
    stmt->next = 0;

    stmt->v.assign.lhs = lhs;
    stmt->v.assign.rhs = rhs;

    emit_stmt(stmt);

    commit_assign(stmt);
}

void
start_if_cond (rhs_t *condition)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_IF_COND;
    stmt->next = 0;

    stmt->v.if_cond.condition = condition;
    stmt->v.if_cond.consequent = 0;
    stmt->v.if_cond.alternative = 0;

    *emit_loc = stmt;
    stmt_stack[stmt_stackp++] = stmt;

    emit_loc = &stmt->v.if_cond.consequent;
}

void
switch_if_branch (void)
{
    statement_t *stmt, *phi;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[stmt_stackp - 1];

    assert(stmt->type == STMT_IF_COND && stmt->v.if_cond.alternative == 0);

    if (stmt->v.if_cond.consequent == 0)
	emit_nil();

    for (phi = stmt->next; phi != 0; phi = phi->next)
    {
	assert(phi->type == STMT_PHI_ASSIGN);

	set_value_current(phi->v.assign.lhs, phi->v.assign.old_value);

	phi->v.assign.old_value = 0;
    }

    emit_loc = &stmt->v.if_cond.alternative;
}

void
end_if_cond (void)
{
    statement_t *stmt, *phi;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[--stmt_stackp];

    assert(stmt->type == STMT_IF_COND && stmt->v.if_cond.consequent != 0);

    if (stmt->v.if_cond.alternative == 0)
	emit_nil();

    for (phi = stmt->next; phi != 0; phi = phi->next)
    {
	assert(phi->type == STMT_PHI_ASSIGN);

	commit_assign(phi);

	if (phi->next == 0)
	    stmt = phi;
    }

    emit_loc = &stmt->next;
}

void
start_while_loop (rhs_t *invariant)
{
    statement_t *stmt = alloc_stmt();

    stmt->type = STMT_WHILE_LOOP;
    stmt->next = 0;

    stmt->v.while_loop.entry = 0;
    stmt->v.while_loop.invariant = invariant;
    stmt->v.while_loop.body = 0;

    emit_stmt(stmt);
    stmt_stack[stmt_stackp++] = stmt;

    emit_loc = &stmt->v.while_loop.body;
}

void
end_while_loop (void)
{
    statement_t *stmt, *phi;

    assert(stmt_stackp > 0);

    stmt = stmt_stack[--stmt_stackp];

    assert(stmt->type == STMT_WHILE_LOOP);

    if (stmt->v.while_loop.body == 0)
	emit_nil();

    for (phi = stmt->v.while_loop.entry; phi != 0; phi = phi->next)
    {
	assert(phi->type == STMT_PHI_ASSIGN);

	commit_assign(phi);
    }

    emit_loc = &stmt->next;
}

void
print_indent (int indent)
{
    int i;

    for (i = 0; i < indent; ++i)
	fputs("  ", stdout);
}

void
print_value (value_t *val)
{
    if (val->var != 0)
	printf("%s[%d]_%d", val->var->name, val->n, val->index);
    else
	printf("$t%d_%d", val->temp->number, val->index);
}

void
print_rhs (rhs_t *rhs)
{
    switch (rhs->type)
    {
	case RHS_VALUE :
	    print_value(rhs->v.value);
	    break;

	case RHS_INT_CONST :
	    printf("%d", rhs->v.int_const);
	    break;

	case RHS_FLOAT_CONST :
	    printf("%f", rhs->v.float_const);
	    break;

	default :
	    assert(0);
    }
}

void
dump_code (statement_t *stmt, int indent)
{
    while (stmt != 0)
    {
	switch (stmt->type)
	{
	    case STMT_NIL :
		print_indent(indent);
		printf("nil\n");
		break;

	    case STMT_ASSIGN :
		print_indent(indent);
		print_value(stmt->v.assign.lhs);
		printf(" = ");
		print_rhs(stmt->v.assign.rhs);
		printf("\n");
		break;

	    case STMT_PHI_ASSIGN :
		print_indent(indent);
		print_value(stmt->v.assign.lhs);
		printf(" = phi(");
		print_rhs(stmt->v.assign.rhs);
		printf(", ");
		print_rhs(stmt->v.assign.rhs2);
		printf(")\n");
		break;

	    case STMT_IF_COND :
		print_indent(indent);
		printf("if ");
		print_rhs(stmt->v.if_cond.condition);
		printf("\n");
		dump_code(stmt->v.if_cond.consequent, indent + 1);
		print_indent(indent);
		printf("else\n");
		dump_code(stmt->v.if_cond.alternative, indent + 1);
		break;

	    case STMT_WHILE_LOOP :
		print_indent(indent);
		printf("start while\n");
		dump_code(stmt->v.while_loop.entry, indent + 1);
		print_indent(indent);
		printf("while ");
		print_rhs(stmt->v.while_loop.invariant);
		printf("\n");
		dump_code(stmt->v.while_loop.body, indent + 1);
		break;

	    default :
		assert(0);
	}

	stmt = stmt->next;
    }
}

void
test_compiler (void)
{
    variable_t *vars = 0;

    variable_t *a = register_variable(&vars, "a", make_tuple_info(nil_tag_number, 1));
    temporary_t *t = make_temporary(TYPE_INT);
    variable_t *b = register_variable(&vars, "b", make_tuple_info(nil_tag_number, 1));

    emit_assign(make_variable_lhs(a, 0), make_int_const_rhs(0));
    emit_assign(make_variable_lhs(b, 0), make_int_const_rhs(1));
    emit_assign(make_temporary_lhs(t), make_variable_rhs(a, 0));
    emit_assign(make_variable_lhs(a, 0), make_variable_rhs(b, 0));
    emit_assign(make_variable_lhs(b, 0), make_temporary_rhs(t));

    start_if_cond(make_variable_rhs(a, 0));
    /* emit_assign(make_variable_lhs(b, 0), make_int_const_rhs(1)); */
    switch_if_branch();
    emit_assign(make_variable_lhs(b, 0), make_int_const_rhs(2));
    end_if_cond();

    start_while_loop(make_variable_rhs(a, 0));
    emit_assign(make_variable_lhs(b, 0), make_variable_rhs(b, 0));
    emit_assign(make_variable_lhs(b, 0), make_int_const_rhs(1));
    emit_assign(make_variable_lhs(b, 0), make_int_const_rhs(3));
    end_while_loop();

    dump_code(first_stmt, 0);
}

int
main (void)
{
    test_compiler();
    return 0;
}
