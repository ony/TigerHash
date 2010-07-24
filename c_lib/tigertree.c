/*
 * Copyright (C) 2010 Nikolay Orlyuk (virkony _at_ gmail _dot_ com)
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

/* Tiger hash can handle up to 16 exbibits - 2 EB = 2**51 KB */

#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

typedef struct tigertree_stack_s {
    char buf[1+TIGER_HASH_SIZE]; /* first byte shows: 00 - empty, 01 - half */
} tigertree_stack;

struct tigertree_context_s {
    size_t left;

    tiger_context *tiger;

    unsigned int level;
    tigertree_stack stack[51]; /* levels, 51*(1+24) = 1275 bytes */
};

const size_t tigertree_context_size() { return sizeof(struct tigertree_context_s); }


void tigertree_init(tigertree_context *ctx)
{
    ctx->tiger = tiger_new();
    tiger_feed(ctx->tiger, "\0", 1);

    ctx->left = TIGERTREE_BLOCK_SIZE;

    ctx->level = 0;
}

void tigertree_done(tigertree_context *ctx)
{
    tiger_free(ctx->tiger);
}


static void tigertree_feed_leaf(tigertree_context *ctx)
{
    char chash[TIGER_HASH_SIZE]; /* temp hash */
    const unsigned int clevel = ctx->level;
    unsigned int level = 1;
    tigertree_stack *node = ctx->stack + 0;


    while((node->buf[0] == '\1') && (level <= clevel)) { /* carry up */
        tiger_finalize(ctx->tiger, chash);

        tiger_reset(ctx->tiger);
        tiger_feed(ctx->tiger, node->buf, 1 + TIGER_HASH_SIZE);
        tiger_feed(ctx->tiger, chash, TIGER_HASH_SIZE);
        node->buf[0] = '\0'; /* clear up this node */

        /* now we have new hash to finalize in our ctx */
        ++node;
        if ((++level) > clevel) break; /* we've reached the top */
    }

    if (level > clevel) {
        ctx->level = level;
        /*
        if (level > ARRAY_SIZE(ctx->stack)) {
            ...
        }
        */
    }

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
    if ((ctx->left < TIGERTREE_BLOCK_SIZE) || /* last block */
        (ctx->level == 0)) /* finalize for empty file (no leafs were added) */
    {
        tigertree_feed_leaf(ctx);
    }

    {
        const size_t clevel = ctx->level;
        size_t level;
        tigertree_stack *node = ctx->stack + 0;
        const void *rhash = NULL;

        assert( ctx->level > 0 );

        for(level = 1; level <= clevel; ++level, ++node) {
            assert( node != NULL );

            if (node->buf[0] == '\0') continue;
            assert( node->buf[0] == '\1' );

            if (rhash == NULL) {
                rhash = node->buf+1;
                continue;
            }

            tiger_reset(ctx->tiger);
            tiger_feed(ctx->tiger, node->buf, 1+TIGER_HASH_SIZE);
            tiger_feed(ctx->tiger, rhash, TIGER_HASH_SIZE);
            tiger_finalize(ctx->tiger, hash);
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

    ctx->level = 0; /* will cut carrying up and force use next node (bottom) as empty */
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
