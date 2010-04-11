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

/*
#define USE_MAP
#define USE_AIO
*/

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <alloca.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <errno.h>
#include <dirent.h>

#if defined(USE_MAP) && !defined(USE_AIO)
#include <sys/mman.h>
#endif

#ifdef USE_AIO
#include <aio.h>
#endif

#include "tiger.h"

//#define BLK_SIZE 8192
#define BLK_SIZE 4096
#define BUF_COUNT 2

#if defined(USE_MAP) && !defined(USE_AIO)
//#define MMAP_SIZE (256*1024*1024)
//#define MMAP_SIZE (256*1024*BLK_SIZE)
#define MMAP_SIZE (16*4096)
//#define MMAP_FLAGS (MAP_SHARED | MAP_NORESERVE | MAP_POPULATE)
#define MMAP_FLAGS (MAP_SHARED | MAP_DENYWRITE | MAP_NORESERVE | MAP_POPULATE)
#endif

#ifdef USE_AIO
#ifndef AIO_BLOCKS
#define AIO_BLOCKS 4
#endif
#endif

#define min(a,b) (((a)>(b))?(b):(a))

void hash_fd(tiger_context *ctx, int fd, const char *desc) {
#ifndef USE_AIO
    size_t r;
#endif
    uint32_t sum[TIGER_HASH_BITS/32];
#if defined(USE_MAP) || defined(USE_AIO)
#ifdef USE_AIO
    uint8_t buf[2][AIO_BLOCKS*BLK_SIZE];
    int ib;
    struct aiocb cb[2];
    const struct aiocb * cbp[] = {cb+0, cb+1};
#else
    uint8_t *addr1, *addr2;
    off_t pos;
#endif
    off_t size_left;
    struct stat st;
#endif

    tiger_reset(ctx);


#if defined(USE_MAP) || defined(USE_AIO)
    if (fstat(fd, &st) != 0) {
        perror("fstat");
    }
#endif

#ifdef USE_AIO
    size_left = st.st_size;
    if (size_left > 0) {
        cb[1].aio_fildes = cb[0].aio_fildes = fd;
        cb[0].aio_buf = buf[0];
        cb[1].aio_buf = buf[1];
#ifdef _POSIX_PRIORITIZED_IO
        cb[1].aio_reqprio = cb[0].aio_reqprio = 0;
#endif
        ib = 0;

        cb[ib].aio_offset = 0;
        cb[ib].aio_nbytes = min(BLK_SIZE, size_left);
        if (aio_read(cb+ib) != 0) {
            perror("aio_read");
            return;
        }
        size_left -= cb[ib].aio_nbytes;

        while(size_left > 0) {
            const int ia = ib;
            ib ^= 1;
            cb[ib].aio_offset = cb[ia].aio_offset + cb[ia].aio_nbytes;
            cb[ib].aio_nbytes = min(AIO_BLOCKS*BLK_SIZE, size_left);
            if (aio_read(cb+ib) != 0) {
                perror("aio_read");
                return;
            }
            size_left -= cb[ib].aio_nbytes;

            if( aio_suspend(cbp+ia, 1, NULL) != 0 ) {
                perror("aio_suspend");
                return;
            }
            aio_return(cb+ia);
            tiger_update(ctx, buf[ia], cb[ia].aio_nbytes);
        }

        if( aio_suspend(cbp+ib, 1, NULL) != 0 ) {
            perror("aio_suspend");
            return;
        }
        aio_return(cb+ib);
        tiger_update(ctx, buf[ib], cb[ib].aio_nbytes);
    }
#else
#ifdef USE_MAP

    pos = 0;
    if (S_ISREG(st.st_mode)) {
        size_left = st.st_size;

        if (size_left > 0) {
            r = min(MMAP_SIZE, size_left);
            addr1 = mmap(NULL, r,
                PROT_READ, MMAP_FLAGS,
                0, pos);
            size_left -= r;
            pos += r;

            while( size_left > 0 ) {
                r = min(MMAP_SIZE, size_left);
                addr2 = mmap(NULL, r,
                    PROT_READ, MMAP_FLAGS,
                    0, pos);
                tiger_update(ctx, addr1, r);
                munmap(addr1, r);
                addr1 = addr2;
                size_left -= r;
                pos += r;
            }

            tiger_update(ctx, addr1, r);
            munmap(addr1, r);
            /*
            size_left -= r;
            pos += r;
            */
        }
    } else
#endif
    {
        char buf[BLK_SIZE];
        while( (r=read(fd,buf,sizeof(buf))) > 0 ) {
            tiger_update(ctx, buf, r);
        }
    }
#endif
    tiger_finalize(ctx, sum);

    printf("%08x%08x%08x%08x%08x%08x %s\n", ntohl(sum[0]), ntohl(sum[1]), ntohl(sum[2]), ntohl(sum[3]), ntohl(sum[4]), ntohl(sum[5]), desc);
}

#ifdef __USE_LARGEFILE64
#define OF_LARGE (O_LARGEFILE)
#else
#define OF_LARGE 0
#endif

#ifdef __USE_GNU
/* #define OF_EXT (O_DIRECT|O_NOATIME) */
#define OF_EXT (O_NOATIME)
#else
#define OF_EXT 0
#endif

void hash_file(tiger_context *ctx, const char *fn) {
    int fd;
    if ((fd = open(fn, O_RDONLY|OF_LARGE|OF_EXT)) == -1) {
        perror("open");
        return;
    }
    hash_fd(ctx, fd, fn);
    close(fd);
}

static inline int is_special(const char *s) {
    switch(s[0]) {
    case '.': break;
    default: return 0;
    }
    switch(s[1]) {
    case '\0': return 1;
    case '.': break;
    default: return 0;
    }
    switch(s[2]) {
    case '\0': return 1;
    default: return 0;
    }
}

void hash_folder(tiger_context *ctx, const char *pn) {
    DIR *dir = opendir(pn);
    struct dirent *ent;
    if (dir == NULL) {
        perror("opendir");
        return;
    }
    if (strcmp(pn,".") == 0) {
        while((ent = readdir(dir)) != NULL) {
            switch(ent->d_type) {
            case DT_DIR:
                if (!is_special(ent->d_name)) {
                    hash_folder(ctx, ent->d_name);
                }
                break;
            case DT_REG:
                hash_file(ctx, ent->d_name);
                break;
            default: ;
            }
        }
    } else {
        const size_t pn_len = strlen(pn);
        char *prefix = alloca(pn_len+1+NAME_MAX+1);
        char *suffix = prefix+pn_len;
        strcpy(prefix, pn);
        strcpy(suffix++, "/");
        while((ent = readdir(dir)) != NULL) {
            switch(ent->d_type) {
            case DT_DIR:
                if (!is_special(ent->d_name)) {
                    strcpy(suffix, ent->d_name);
                    hash_folder(ctx, prefix);
                }
                break;
            case DT_REG:
                strcpy(suffix, ent->d_name);
                hash_file(ctx, prefix);
                break;
            default: ;
            }
        }
    }
}

int main(int argc, char *argv[])
{
    tiger_context *ctx = tiger_new();
    int i;

    if (argc < 2) {
        hash_fd(ctx, 0, "-");
    } else {
        struct stat st;
        for(i=1;i<argc;++i) {
            lstat(argv[i], &st);
            if (S_ISREG(st.st_mode)) hash_file(ctx, argv[i]);
            else if (S_ISDIR(st.st_mode)) hash_folder(ctx, argv[i]);
        }
    }
    
    tiger_free(ctx);

    return 0;
}
