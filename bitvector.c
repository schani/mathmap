/*
 * bitvector.c
 *
 * MathMap
 *
 * Copyright (C) 2004 Mark Probst
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "bitvector.h"

#define LONG_SHIFT 6

#define LONG_BITS  (1 << LONG_SHIFT)
#define LONG_MASK  (LONG_BITS-1)

#define BIT_SIZE_TO_LONG_SIZE(s)          (((s)+LONG_MASK) >> LONG_SHIFT)
#define BIT_VECTOR_BYTE_SIZE(s)           (sizeof(bit_vector_t) + sizeof(bit_vector_long_t) * BIT_SIZE_TO_LONG_SIZE((s)))
#define BIT(p)                            ((bit_vector_long_t)1 << (p))

bit_vector_t*
new_bit_vector (unsigned long size, int init_bit)
{
    bit_vector_t *bitvec;

    bitvec = (bit_vector_t*)malloc(BIT_VECTOR_BYTE_SIZE(size));
    assert(bitvec != 0);

    bitvec->size = size;
    memset(bitvec->data, init_bit ? 0xff : 0x00, sizeof(bit_vector_long_t) * BIT_SIZE_TO_LONG_SIZE(size));

    return bitvec;
}

bit_vector_t*
copy_bit_vector (bit_vector_t *bitvec)
{
    bit_vector_t *copy;

    copy = (bit_vector_t*)malloc(BIT_VECTOR_BYTE_SIZE(bitvec->size));
    assert(copy != 0);

    memcpy(copy, bitvec, BIT_VECTOR_BYTE_SIZE(bitvec->size));

    return copy;
}

void
free_bit_vector (bit_vector_t *bitvec)
{
    free(bitvec);
}

void
bit_vector_set (bit_vector_t *bitvec, unsigned long which)
{
    assert(which < bitvec->size);

    bitvec->data[which >> LONG_SHIFT] |= BIT(which & LONG_MASK);
}

void
bit_vector_clear (bit_vector_t *bitvec, unsigned long which)
{
    assert(which < bitvec->size);

    bitvec->data[which >> LONG_SHIFT] &= ~BIT(which & LONG_MASK);
}

int
bit_vector_bit (bit_vector_t *bitvec, unsigned long which)
{
    assert(which < bitvec->size);

    return (bitvec->data[which >> LONG_SHIFT] & BIT(which & LONG_MASK)) != 0;
}

#ifdef TEST_BITVECTOR
#include <stdio.h>

int
is_prime (unsigned long l)
{
    unsigned long d;

    if (l < 2)
	return 0;

    for (d = 2; d <= l / 2; ++d)
	if (l % d == 0)
	    return 0;

    return 1;
}

#define NUM_BITS         10000

int
main (void)
{
    bit_vector_t *bitvec = new_bit_vector(NUM_BITS, 0);
    unsigned long l;

    assert(bitvec != 0);

    for (l = 0; l < NUM_BITS; ++l)
	if (is_prime(l))
	{
	    /* printf("%lu ", l); */
	    bit_vector_set(bitvec, l);
	}
    printf("\n");

    for (l = 0; l < NUM_BITS; ++l)
	if (is_prime(l))
	    assert(bit_vector_bit(bitvec, l));
	else
	    assert(!bit_vector_bit(bitvec, l));

    return 0;
}
#endif
