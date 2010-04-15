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

#ifndef NDEBUG
#include <stdio.h>
#define DEBUGF(fmt, args...) (fprintf(stderr, "%s(%s+%d): " fmt "\n", __FUNCTION__, __FILE__, __LINE__, args), fflush(stderr))
#else
#define DEBUGF(fmt, args...)
#endif

#include "tiger.h"
#include "tigertree.h"

typedef struct tigertree_stack_s {
    char buf[1+2*TIGER_HASH_SIZE]; // 00 - empty, 01 - half
    struct tigertree_stack_s *upper;
} tigertree_stack;

typedef struct tigertree_allocs_s {
    tigertree_stack data[32]; // 4 TB files
    struct tigertree_allocs_s *prev;
} tigertree_allocs;

struct tigertree_context_s {
    size_t left;

    tiger_context *tiger;

    unsigned int level;
    tigertree_stack *bottom;
    tigertree_allocs *allocs;
};

const size_t tigertree_context_size = sizeof(struct tigertree_context_s);

// returns first allocated
static tigertree_stack *tigertree_alloc(tigertree_context *ctx)
{
    size_t i;
    tigertree_allocs *next = (tigertree_allocs*)malloc(sizeof(tigertree_allocs));
    tigertree_stack *const data = next->data;
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

void tigertree_init(tigertree_context *ctx)
{
    ctx->tiger = tiger_new();
    tiger_feed(ctx->tiger, "\0", 1);

    ctx->left = TIGERTREE_BLOCK_SIZE;

    ctx->level = 0;
    ctx->allocs = NULL;
    ctx->bottom = tigertree_alloc(ctx); // atleast one level
}

void tigertree_done(tigertree_context *ctx)
{
    tigertree_allocs *f = ctx->allocs;
    while(f != NULL) {
        tigertree_allocs * const u = f->prev;
        free(f);
        f = u;
    }

    tiger_free(ctx->tiger);
}


static void tigertree_feed_leaf(tigertree_context *ctx)
{
    const unsigned int clevel = ctx->level;
    unsigned int level = 1;
    tigertree_stack *node = ctx->bottom;
    assert( ctx->bottom != NULL );


    //DEBUGF("node marker 0x%02x", node->buf[0]);
    while((node->buf[0] == '\1') && (level <= clevel)) { // carry up
        //DEBUGF("carry up from level %u", level);
        tiger_finalize(ctx->tiger, node->buf + 1 + TIGER_HASH_SIZE);

        tiger_reset(ctx->tiger);
        tiger_feed(ctx->tiger, node->buf, sizeof(node->buf));
        node->buf[0] = '\0';

        node = node->upper;
        if (node == NULL ) {
            node = tigertree_alloc(ctx);
            ++level;
            break;
        }
        else if ((++level) > clevel) break;
    }
    //assert( node->buf[0] == '\0' );
    //DEBUGF("put at level %u (toplevel = %u)", level, clevel);

    if (level > clevel) ctx->level = level;

    node->buf[0] = '\1';
    return tiger_finalize(ctx->tiger, node->buf + 1);
}

void tigertree_feed(tigertree_context *ctx, const void *buf, size_t len)
{
    const char *p = buf;

    while(1) {
        if (len < ctx->left) {
            ctx->left -= len;
            return tiger_feed(ctx->tiger, p, len);
        }
        tiger_feed(ctx->tiger, p, ctx->left);
        tigertree_feed_leaf(ctx);

        tiger_reset(ctx->tiger);
        tiger_feed(ctx->tiger, "\0", 1);

        p += ctx->left;
        len -= ctx->left;
        ctx->left = TIGERTREE_BLOCK_SIZE;
    }
}

void tigertree_finalize(tigertree_context *ctx, void *hash)
{
    if (ctx->left < TIGERTREE_BLOCK_SIZE) tigertree_feed_leaf(ctx); // last block
    tiger_reset(ctx->tiger);

    {
        const size_t clevel = ctx->level;
        size_t level;
        tigertree_stack *node = ctx->bottom;
        const void *rhash = NULL;

        assert( ctx->level > 0 );

        for(level = 1; level <= clevel; ++level, node = node->upper) {
            assert( node != NULL );

            DEBUGF("node marker 0x%02x", node->buf[0]);

            if (node->buf[0] == '\0') continue;
            assert( node->buf[0] == '\1' );

            if (rhash == NULL) {
                rhash = node->buf+1;
                DEBUGF("first rhash @ %p", rhash);
                continue;
            }
            // assert( rhash != NULL );

            DEBUGF("combine %p %p", node->buf+1, rhash);
            tiger_feed(ctx->tiger, node->buf, 1+TIGER_HASH_SIZE);
            tiger_feed(ctx->tiger, rhash, TIGER_HASH_SIZE);
            tiger_finalize(ctx->tiger, hash);
            tiger_reset(ctx->tiger);
            rhash = hash;
        }

        if (rhash != hash) memcpy(hash, rhash, TIGER_HASH_SIZE);
    }
}

void tigertree_reset(tigertree_context *ctx)
{
    tiger_reset(ctx->tiger);
    tiger_feed(ctx->tiger, "\0", 1);

    ctx->left = TIGERTREE_BLOCK_SIZE;

    ctx->level = 0; // will cut carrying
}

tigertree_context *tigertree_new()
{
    tigertree_context *ctx = (tigertree_context*)malloc(sizeof(tigertree_context));
    tigertree_init(ctx);
    return ctx;
}

void tigertree_free(tigertree_context *ctx)
{
    tigertree_done(ctx);
    free(ctx);
}
