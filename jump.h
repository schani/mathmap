/* -*- c -*- */

/*
 * jump.h
 *
 * MathMap
 *
 * Copyright (C) 1998 Mark Probst
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


#ifndef __JUMP_H__
#define __JUMP_H__

#include <setjmp.h>

extern jmp_buf *topmostJmpBuf;

#define DO_JUMP_CODE              { \
                                      jmp_buf jmpBuf, \
                                          *lastTopmost = topmostJmpBuf; \
                                      int jumpResult; \
                                      \
                                      topmostJmpBuf = &jmpBuf; \
                                      jumpResult = setjmp(jmpBuf); \
                                      if (jumpResult != 0) \
                                          topmostJmpBuf = lastTopmost; \
                                      else

#define WITH_JUMP_HANDLER             if (jumpResult != 0)

#define END_JUMP_HANDLER              else \
                                          topmostJmpBuf = lastTopmost; \
                                  }

#define JUMP(result)              longjmp(*topmostJmpBuf, (result))
#define REJUMP                    JUMP(jumpResult)

#define JUMP_CODE_RETURN          0x01000000
#define JUMP_CODE_BREAK           0x02000000

#define JUMP_CODE_MASK            0xff000000
#define JUMP_INFO_MASK            0x00ffffff

#define JUMP_CODE                 (jumpResult & JUMP_CODE_MASK)
#define JUMP_INFO                 (jumpResult & JUMP_INFO_MASK)

#define JUMP_HANDLE_RETURN        if (JUMP_CODE == JUMP_CODE_RETURN) REJUMP

#endif
