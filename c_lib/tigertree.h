/*
    DC++ protocl utls for GHC
    Copyright (C) 2009 Nikolay Orlyuk (virkony _at_ gmail _dot_ com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/

#ifndef __tigertree_h__
#define __tigertree_h__

#include <stdlib.h>

typedef struct tigertree_context_s tigertree_context;

#define TIGERTREE_BLOCK_SIZE 1024

#ifdef __cplusplus
extern "C" {
#endif

void tigertree_init(tigertree_context *ctx);
void tigertree_done(tigertree_context *ctx);
void tigertree_feed(tigertree_context *ctx, const void *block, size_t bytes_count);
void tigertree_finalize(tigertree_context *ctx, void *hash);
void tigertree_reset(tigertree_context *ctx);

extern const size_t tigertree_context_size;
tigertree_context *tigertree_new();
void tigertree_free(tigertree_context *ctx);

#ifdef __cplusplus
}
#endif

#endif
