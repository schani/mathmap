/* -*- c -*- */

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

#endif
