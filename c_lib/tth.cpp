#include <string>
#include <iostream>
#include <vector>
#include <memory>

#include <cassert>

#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <cctype>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>


#include "tiger.h"
#include "tigertree.h"

#include "Tiger.hpp"

//#define TIGERTREE_CXX 1

class HexDump {
    const char * const blockData;
    const size_t blockSize;
public:
    HexDump(const char *data, size_t size) : blockData(data), blockSize(size) {}

    friend std::ostream &operator<<(std::ostream &s, const HexDump& dump);
};

std::ostream &operator<<(std::ostream &s, const HexDump& dump) {
    static char digits[] = "0123456789abcdef";
    const char *p = dump.blockData;
    size_t i = dump.blockSize, j;
    for(; i > 0; i-=j, p+=j) {
        for (j = 0; (j < 16) && (j < i); ++j) {
            s << digits[p[j] >> 4] << digits[p[j] & 0xf] << ' ';
        }
        for (; (j < 16) ; ++j) {
            s << "   ";
        }
        s << "| ";
        for (j = 0; (j < 16) && (j < i); ++j) {
            if (isprint(p[j])) {
                s << p[j];
            } else {
                s << '.';
            }
        }
        s << std::endl;
    }
    return s;
}

#define final (static_cast<final_type*>(this))

template<class _Impl>
class LTH {
    Tiger *tiger;

public:
    enum { BLOCK_SIZE = 1024 };

    typedef _Impl final_type;

    LTH(Tiger *t) : tiger(t) {}

    void *nextBlock(size_t &len); /* len => unread */

    bool next(Tiger::hash_type &hash)
    {
        static const char magic = 0x00;

        size_t len = BLOCK_SIZE;
        const void *block = final->nextBlock(len);
        if (block == NULL) return false;

        size_t plen = BLOCK_SIZE;

        tiger->feed(&magic, sizeof(magic));

        while(block != NULL) {
            // std::cerr << plen - len << std::endl;
            //std::cerr << HexDump((const char*)block, plen-len) << std::endl;
            tiger->feed(block, plen-len);
            if (len == 0) break;
            plen = len;
            block = final->nextBlock(len);
        }

        tiger->finalize(hash);
        tiger->reset();
        return true;
    }
};

class FileLTH: public LTH<FileLTH> {
    int fd;
    char *pageA, *pageB;
    off_t offset, length;
    size_t pgoffs;

    static char *mmap(void *addr, size_t length, int fd, off_t offset)
    {
        //std::cerr << "mmap(" << addr << ", " << length << ", " << fd << ", " << offset << ");" << std::endl;
        return (char*)::mmap(addr, length, PROT_READ, MAP_SHARED | MAP_NONBLOCK | MAP_NORESERVE, fd, offset);
    }

    void swapPages() {
        register char *page = pageA;
        pageA = pageB;
        pageB = page;
    }
public:
    enum { PAGE_SIZE = 4096 };

    typedef LTH< FileLTH > base_type;

    FileLTH(Tiger *t) : base_type(t),
        fd(-1),
        pageA(NULL), pageB(NULL),
        offset(0),
        pgoffs(0)
    { }

    ~FileLTH()
    {
        if (fd == -1) return;
        close(fd);

        if (length == (off_t)-1) return; // ill-formed object

        if (pageB != NULL) {
            munmap(pageA, PAGE_SIZE);
            munmap(pageB, std::min<size_t>(PAGE_SIZE, length-offset-PAGE_SIZE));
        } else if (pageA != NULL) {
            munmap(pageA, length-offset);
        }
    }

    bool select(const char *filename)
    {
        if (fd != -1) {
            close(fd);

            if (pageB != NULL) {
                munmap(pageA, PAGE_SIZE);
                munmap(pageB, std::min<size_t>(PAGE_SIZE, length-offset-PAGE_SIZE));
            } else if (pageA != NULL) {
                munmap(pageA, length-offset);
            }
        }

        fd = ::open(filename, O_RDONLY | O_DIRECT | O_NOATIME);
        if (fd == -1) return false;

        offset = 0;
        pgoffs = 0;
        length = lseek(fd, 0, SEEK_END);
        if (length == (off_t)-1) return false; // ill-formed object

        if (length <= PAGE_SIZE) {
            pageA = mmap(pageA, length, fd, 0);
            pageB = NULL;
        } else {
            pageA = mmap(pageA, PAGE_SIZE, fd, 0);
            pageB = mmap(pageB, std::min<size_t>(PAGE_SIZE, length-PAGE_SIZE), fd, PAGE_SIZE);
        }
        return true;
    }

    const size_t layers() const {
        off_t len = length >> 10;
        size_t i = 0;
        for(;len > 0;++i) {
            len >>= 1;
        }
        return i;
    }

    const void *nextBlock(size_t &len)
    {
        if (pageA == NULL) return NULL;
        else if (pageB == NULL) { // last page
            assert( (offset + PAGE_SIZE) >= length );
            const size_t pgsz = length - offset;
            const size_t pgleft = pgsz - pgoffs;
            if (pgleft == 0) {
                munmap(pageA, pgsz);
                pageA = NULL;
                return NULL;
            }

            const size_t blksz = std::min(len, pgleft);
            const void *ptr = pageA + pgoffs;
            pgoffs += blksz;
            len -= blksz;
            return ptr;
        } else {
            assert( (offset + PAGE_SIZE) < length );
            const size_t pgleft = PAGE_SIZE - pgoffs;
            if (pgleft == 0) {
                swapPages();
                offset += PAGE_SIZE;
                pgoffs = 0;
                munmap(pageB, PAGE_SIZE);
                if ((offset+PAGE_SIZE) < length) {
                    pageB = mmap(pageB, std::min<size_t>(PAGE_SIZE, length-offset-PAGE_SIZE), fd, offset+PAGE_SIZE);
                } else {
                    pageB = NULL;
                }
                return nextBlock(len);
            }

            const size_t blksz = std::min(len, pgleft);
            const char *ptr = pageA + pgoffs;
            pgoffs += blksz;
            len -= blksz;
            return ptr;
        }
    }
};

#ifdef TIGERTREE_CXX
class ITH {
    Tiger *tiger;
public:
    ITH(Tiger *t) : tiger(t) {}
    virtual bool nextHash(Tiger::hash_type &hash) = 0;

    bool next(Tiger::hash_type &hash)
    {
        static const char magic = 0x01;
        
        bool f = nextHash(hash);
        if (f == false) return false;

        Tiger::hash_type hashB;
        f = nextHash(hashB);
        if (f == false) return true;

        tiger->feed(&magic, sizeof(magic));
        tiger->feed(hash.data, Tiger::hash_type::BYTES);
        tiger->feed(hashB.data, Tiger::hash_type::BYTES);
        
        tiger->finalize(hash);
        tiger->reset();

        return true;
    }

    friend class SuperITH;
};

class FileITH : public ITH {
    FileLTH lth;
public:
    FileITH(Tiger *t) : ITH(t), lth(t) {}

    bool nextHash(Tiger::hash_type &hash)
    { return lth.next(hash); }

    size_t layers() const
    { return lth.layers(); }

    bool select(const char *fn)
    { return lth.select(fn); }
};

class SuperITH : public ITH {
    ITH *ith;
public:
    SuperITH(ITH *base) : ITH(base->tiger), ith(base) {}

    bool nextHash(Tiger::hash_type &hash)
    { return ith->next(hash); }
};

class TTR {
    Tiger tiger;
    FileITH lth;
    std::vector<std::auto_ptr<SuperITH> > ith;
    size_t layer;
public:
    TTR() : lth(&tiger), layer(0) {
        ith.push_back(std::auto_ptr<SuperITH>(new SuperITH(&lth)));
    }

    bool select(const char *fn)
    {
        //std::cerr << fn << std::endl;
        if (lth.select(fn) == false) return false;

        layer = std::max<size_t>(1, lth.layers());

        if (layer >= ith.size()) {
            //std::cerr << ith.size() << " => " << (layer+1) << std::endl;
            ith.reserve(layer+1);
            while(layer >= ith.size()) {
                ith.push_back(std::auto_ptr<SuperITH>(new SuperITH(ith.back().get())));
            }
        }
        return true;
    }

    bool finalize(Tiger::hash_type &hash)
    {
#ifdef NDEBUG
        return (ith[layer]->next(hash));
#else
        bool r = ith[layer]->next(hash);
        assert(ith[layer]->next(hash) == false);
        return r;
#endif
    }
};

#else
class TTR {
    Tiger tiger;
    TigerTree tigert;
    FileLTH lth;
public:
    TTR() : lth(&tiger) {
    }

    bool select(const char *fn)
    {
        //std::cerr << fn << std::endl;
        if (lth.select(fn) == false) return false;

        return true;
    }

    bool finalize(Tiger::hash_type &hash)
    {
        size_t len;
        const void *block = NULL;
        while(true) {
            len = 4096;
            const size_t plen = len;
            block = lth.nextBlock(len);
            if (plen == len) break;
            if (block == NULL) break;
            tigert.feed(block, plen-len);
        }
        tigert.finalize(hash);
        tigert.reset();
        return true;
    }
};
#endif

/* Leaf Tiger Hash = LTH = Tiger(00 + 1024-bytes block)
 * Internal Tiger Hash = ITH = Tiger(01 + hash1 + hash2)
 * Tiger Tree Root = TTR = top ITH/LTH
 * http://ru.wikipedia.org/wiki/TTH
*/

void hash_file(TTR &ttr, const char *fn) {
    if (ttr.select(fn) == false) {
        perror("open/lseek/map/unmap");
        return;
    }
    Tiger::hash_type h;
    if (ttr.finalize(h)) {
        std::cout << h << " " << fn << std::endl;
    }
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
void hash_folder(TTR &ttr, const char *pn) {
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
                    hash_folder(ttr, ent->d_name);
                }
                break;
            case DT_REG:
                hash_file(ttr, ent->d_name);
                break;
            default: ;
            }
        }
    } else {
        const size_t pn_len = strlen(pn);
        char *prefix = (char*)alloca(pn_len+1+NAME_MAX+1);
        char *suffix = prefix+pn_len;
        strcpy(prefix, pn);
        strcpy(suffix++, "/");
        while((ent = readdir(dir)) != NULL) {
            switch(ent->d_type) {
            case DT_DIR:
                if (!is_special(ent->d_name)) {
                    strcpy(suffix, ent->d_name);
                    hash_folder(ttr, prefix);
                }
                break;
            case DT_REG:
                strcpy(suffix, ent->d_name);
                hash_file(ttr, prefix);
                break;
            default: ;
            }
        }
    }
}
int main(int argc, char *argv[])
{
    TTR ttr;

    struct stat st;
    if (argc < 2) {
        ssize_t r;
        char pn[4096];
        char *p = pn, *q;
        while(true) {
            assert(p < (pn+sizeof(pn)));
            r = read(0, p, pn+sizeof(pn)-p);
            if (r < 1) break;

            while (r > 0) {
                q = (char*)memchr(p, '\n', r);
                if (q == NULL) {
                    p += r;
                    break;
                }
                *q++ = '\0';
                r = p+r - q;
                std::cerr << pn << std::endl;

                lstat(pn, &st);
                if (S_ISREG(st.st_mode)) hash_file(ttr, pn);
                else if (S_ISDIR(st.st_mode)) hash_folder(ttr, pn);

                memmove(pn, q, r);
                p = pn;
            }
        }
    } else {
        size_t i;
        for(i=1;i<argc;++i) {
            lstat(argv[i], &st);
            if (S_ISREG(st.st_mode)) hash_file(ttr, argv[i]);
            else if (S_ISDIR(st.st_mode)) hash_folder(ttr, argv[i]);
        }
    }

    return 0;
}
