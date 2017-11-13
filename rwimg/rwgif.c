/* -*- c -*- */

/*
 * rwgif.c
 *
 * rwimg
 *
 * Copyright (C) 2006 Xavier Martin
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

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include <gif_lib.h>
#include <string.h>

typedef struct
{
    GifFileType *file;
    int width;
    int height;
    unsigned char *rgb;
} gif_data_t;

void*
open_gif_file (const char *filename, int *width, int *height)
{
    int interlace_offset[] = { 0, 4, 2, 1 };
    int interlace_jump[] = { 8, 8, 4, 2 };    
    GifColorType *colormap;
    GifRecordType record_type;
    GifRowType *buffer = NULL;
    
    int i, j, gif_err;
    int color_index;
    unsigned char *ptr = NULL;
                
    gif_data_t *data = (gif_data_t*)malloc(sizeof(gif_data_t));
    
    assert(data != 0);
    
    data->file = DGifOpenFileName(filename, &gif_err);
    
    assert(data->file !=0);
        
    do
    {
        assert(DGifGetRecordType(data->file, &record_type) != GIF_ERROR) ;

        switch (record_type)
        {
        case IMAGE_DESC_RECORD_TYPE:
            assert(DGifGetImageDesc(data->file) != GIF_ERROR);
            
            *width = data->file->Image.Width;
            *height = data->file->Image.Height;
            data->width = *width;
            data->height = *height;
            
            buffer = malloc(*height * sizeof(GifRowType *));
            assert(buffer != NULL);
            
            for (i = 0; i < *height; i++) 
            {
                buffer[i] = malloc(*width * sizeof(GifPixelType));
                assert(buffer[i] != NULL);
            }
            
            if (data->file->Image.Interlace)
            {
                for (i = 0; i < 4; i++)
                    for (j = interlace_offset[i]; j < *height; 
                         j += interlace_jump[i])
                        DGifGetLine(data->file, buffer[j], *width);
            }
            else
            {
                for (i = 0; i < *height; i++)
                    DGifGetLine(data->file, buffer[i], *width);
            }
            break;
        case EXTENSION_RECORD_TYPE:
        {
            /* Skip extension blocks */
            int ext_code;
            GifByteType *ext;
            assert(DGifGetExtension(data->file, &ext_code, &ext) != GIF_ERROR); 
            
            while (ext != NULL) 
            {
                assert(DGifGetExtensionNext(data->file, &ext) != GIF_ERROR); 
            }
        }
        break;
        case TERMINATE_RECORD_TYPE:
            break;
        default:
            fprintf(stderr, "unknown record type in GIF file\n");
            break;
        }
    } while (record_type != TERMINATE_RECORD_TYPE);
    
    
    colormap = (data->file->Image.ColorMap ? data->file->Image.ColorMap->Colors
                : data->file->SColorMap->Colors);

    data->rgb = (unsigned char*)malloc( (data->width* data->height * 3) );
    assert(data->rgb != NULL);
    
    ptr = data->rgb;
    
    for (j = 0; j < *height; j++)
    {
        for (i = 0; i < *width; i++)
        {
            color_index = (int) buffer[j][i];
            *ptr++ = (unsigned char) colormap[color_index].Red;
            *ptr++ = (unsigned char) colormap[color_index].Green;
            *ptr++ = (unsigned char) colormap[color_index].Blue;
        }
        free(buffer[j]);
    }
    free(buffer);
    
    assert(DGifCloseFile(data->file, &gif_err) == GIF_OK);
    
    return data;
}

void
gif_read_lines (void *_data, unsigned char *lines, int num_lines)
{
    gif_data_t *data = (gif_data_t*)_data;
    
    memcpy(lines, data->rgb, data->width * num_lines * 3);
}

void
gif_free_data (void *_data)
{    
    gif_data_t *data = (gif_data_t*)_data;

    free(data->rgb);
    free(data);
}
