#include <unistd.h>

#include "tiger.h"
#include "tigertree.h"

int main()
{
    char block[4096];
    ssize_t r;
    tigertree_context *ctx = tigertree_new();
    while((r = read(0, block, sizeof(block))) > 0) {
        tigertree_feed(ctx, block, r);
    }
    tigertree_finalize(ctx, block);
    write(1, block, TIGER_HASH_SIZE);
    tigertree_free(ctx);
}
