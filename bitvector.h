/*
 * bitvector.h
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

#include <glib.h>

typedef guint64 bit_vector_long_t;

typedef struct
{
    unsigned long size;		/* in bits */
    bit_vector_long_t data[];
} bit_vector_t;

bit_vector_t* new_bit_vector (unsigned long size, int init_bit);
bit_vector_t* copy_bit_vector (bit_vector_t *bitvec);
void free_bit_vector (bit_vector_t *bitvec);

void bit_vector_set (bit_vector_t *bitvec, unsigned long which);
void bit_vector_clear (bit_vector_t *bitvec, unsigned long which);

int bit_vector_bit (bit_vector_t *bitvec, unsigned long which);
