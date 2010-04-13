/*
 * Copyright (C) 2001-2006 Jacek Sieka, arnetheduck on gmail point com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
    DC++ protocl utls for GHC
    Copyright (C) 2010 Nikolay Orlyuk (virkony _at_ gmail _dot_ com)

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

#include <assert.h>
#include <string.h>

#include "tiger.h"
#include "tigertree.h"

typedef struct tt_stack_s {
    char buf[1+2*TIGER_HASH_SIZE]; // 00 - empty, 01 - half
    struct tt_stack_s *upper;
} tt_stack;

typedef struct tt_allocs_s {
    tt_stack data[64];
    struct tt_allocs_s *prev;
} tt_allocs;

struct tt_ctx_s {
    size_t left;

    tiger_context *tiger;

    size_t level;
    //tt_stack *top;
    tt_stack *bottom;
    tt_allocs *allocs;
};

// returns first allocated
static tt_stack *tt_alloc(tt_ctx *ctx)
{
    size_t i;
    tt_allocs *next = (tt_allocs*)malloc(sizeof(tt_allocs));
    tt_stack *const data = next->data;
    const size_t cnt = sizeof(next->data)/sizeof(data[0]);
    
    for(i=1;i<cnt;++i) {
        data[i-1].buf[0] = '\0'; // empty
        data[i-1].upper = data+i;
    }
    data[i-1].buf[0] = '\0';

    if (ctx->allocs != NULL) {
        assert( ctx->allocs->data[cnt-1].upper == NULL );
        ctx->allocs->data[cnt-1].upper = data;
    }

    next->prev = ctx->allocs;
    ctx->allocs = next;

    return data;
}

void tt_init(tt_ctx *ctx)
{
    ctx->tiger = tiger_new();
    tiger_update(ctx->tiger, "\0", 1);

    ctx->left = TIGERTREE_BLOCK_SIZE;

    ctx->level = 0;
    //ctx->top = NULL;
    ctx->allocs = NULL;
    ctx->bottom = tt_alloc(ctx); // atleast one level
}

void tt_done(tt_ctx *ctx)
{
    tt_allocs *f = ctx->allocs;
    while(f != NULL) {
        tt_allocs * const u = f->prev;
        free(f);
        f = u;
    }

    tiger_free(ctx->tiger);
}


static void tt_feed_leaf(tt_ctx *ctx)
{
    const size_t clevel = ctx->level;
    size_t level = 1;
    tt_stack *node = ctx->bottom;
    assert( ctx->bottom != NULL );

    while((node->buf[0] == '\1') && (level < clevel)) { // carry up
        tiger_finalize(ctx->tiger, node->buf + 1 + TIGER_HASH_SIZE);

        tiger_reset(ctx->tiger);
        tiger_update(ctx->tiger, node->buf, sizeof(node->buf));
        node->buf[0] = '\0';

        node = node->upper;
        if (node == NULL ) node = tt_alloc(ctx);
        ++level;
    }
    //assert( node->buf[0] == '\0' );

    if (level > clevel) {
        ctx->level = level;
        //ctx->top = node;
    }

    node->buf[0] = '\1';
    return tiger_finalize(ctx->tiger, node->buf + 1);
}

void tt_feed(tt_ctx *ctx, const void *buf, size_t len)
{
    const char *p = buf;

    while(1) {
        if (len < ctx->left) {
            ctx->left -= len;
            return tiger_update(ctx->tiger, p, len);
        }
        tiger_update(ctx->tiger, p, ctx->left);
        tt_feed_leaf(ctx);

        tiger_reset(ctx->tiger);
        tiger_update(ctx->tiger, "\0", 1);

        p += ctx->left;
        len -= ctx->left;
        ctx->left = TIGERTREE_BLOCK_SIZE;
    }
}

void tt_finalize(tt_ctx *ctx, void *hash)
{
    if (ctx->left < TIGERTREE_BLOCK_SIZE) tt_feed_leaf(ctx); // last block
    tiger_reset(ctx->tiger);

    {
        const size_t clevel = ctx->level;
        size_t level;
        tt_stack *node = ctx->bottom;
        const void *rhash = NULL;

        assert( ctx->level > 0 );

        for(level = 1; level <= clevel; ++level, node = node->upper) {
            assert( node != NULL );

            if (node->buf[0] == '\0') continue;
            assert( node->buf[0] == '\1' );

            if (rhash == NULL) {
                rhash = node->buf+1;
                continue;
            }
            // assert( rhash != NULL );

            tiger_update(ctx->tiger, node->buf, 1+TIGER_HASH_SIZE);
            tiger_update(ctx->tiger, rhash, TIGER_HASH_SIZE);
            tiger_finalize(ctx->tiger, hash);
            tiger_reset(ctx->tiger);
            rhash = hash;
        }

        if (rhash != hash) memcpy(hash, rhash, TIGER_HASH_SIZE);
    }
}

void tt_reset(tt_ctx *ctx)
{
    tiger_reset(ctx->tiger);
    tiger_update(ctx->tiger, "\0", 1);

    ctx->left = TIGERTREE_BLOCK_SIZE;

    ctx->level = 0; // will cut carrying
    //ctx->top = NULL;
}
