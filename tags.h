/* -*- c -*- */

/*
 * tags.h
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

#ifndef __TAGS_H__
#define __TAGS_H__

void init_tags (void);

int tag_number_for_name (const char *name);
const char* tag_name_for_number (int num);

extern int nil_tag_number;
extern int xy_tag_number;
extern int ra_tag_number;
extern int rgba_tag_number;
extern int ri_tag_number;
extern int image_tag_number;

#endif
