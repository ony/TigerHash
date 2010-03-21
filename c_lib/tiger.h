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

#ifndef __tiger_h__
#define __tiger_h__

#define TIGER_HASH_BITS 512
#define TIGER_HASH_SIZE (TIGER_HASH_BITS/8)

struct tiger_context_s;
typedef struct tiger_context_s tiger_context;

tiger_context *tiger_new();
void tiger_reset(tiger_context *ctx);
void tiger_free(tiger_context *ctx);
void tiger_update(tiger_context *ctx, const void *block, size_t bits_count);
void tiger_finalize(tiger_context *ctx, void *hash);

#endif
