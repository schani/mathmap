/* -*- c -*- */

#ifndef __NOISE_H__
#define __NOISE_H__

float noise1 (float x);
float noise3 (float x, float y, float z);
float scnoise (float x, float y, float z);
float vlnoise3 (float x, float y, float z, float distortion);

void init_noise (void);

#endif
