#ifndef __Tiger_hpp__
#define __Tiger_hpp__

#include <stdint.h>
#include <string>
#include <cassert>
#include <cstring>

#include "tiger.h"
#include "tigertree.h"

template<size_t _bits>
class BitsIter {
    const uint8_t *p;
    unsigned short offs;
public:
    enum { BITS = _bits };
    BitsIter(const uint8_t *data) : p(data), offs(0) {}

    uint32_t next() {
        uint32_t r = p[0] & (0xff >> offs);
        if (BITS < (8-offs)) {
            r >>= 8-offs-BITS;
            offs += BITS;
            return r;
        }

        ++p;
        size_t i = 8-offs;

        for(;(i+8)<BITS;i+=8) {
            r <<= 8;
            r |= *p++;
        }

        offs = BITS - i;
        if (offs > 0) {
            r <<= offs;
            r |= p[0] >> (8-offs);
        }

        return r;
    }

    uint32_t pad() const {
        uint32_t r = p[0] & (0xff >> offs);
        r <<= BITS - (8-offs);
        return r;
    }
};



template<int _bits>
struct Hash {
    enum { BITS = _bits, BYTES = _bits / 8 + ((_bits % 8)?1:0), NIBBLES = _bits / 4 + ((_bits % 4)?1:0) };

    uint8_t data[BYTES];

    std::basic_string<uint8_t> binary() const {
        return std::string(data, BYTES);
    }

    std::string hex() const {
        assert((BITS % 4) == 0);

        static const char digits[] = "0123456789ABCDEF";
        std::string digest;

        digest.reserve(NIBBLES);

        int i,j;
        for(i=j=0; (i+2) <= NIBBLES; i+=2, ++j) {
            digest.push_back(digits[(data[j] >> 4) & 0xf]);
            digest.push_back(digits[data[j] & 0xf]);
        }
        if (i < NIBBLES) {
            digest.push_back(digits[(data[j] >> 4) & 0xf]);
        }
        return digest;
    }
    std::string base32() const {
        static const char digits[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";

        assert(((BITS % 5) == 0) || ((BITS % 8) == 0));

        std::string digest;

        digest.reserve((BITS/5)+1);

        size_t i;
        BitsIter<5> g(data);
        for(i=0; (i+5) < BITS; i+= 5) {
            digest.push_back(digits[g.next()]);
        }

        if (i < BITS) {
            digest.push_back(digits[g.pad()]);
            digest.push_back('=');

        }
        return digest;
    }
};

template<int _bits>
std::ostream &operator<<(std::ostream &s, const Hash<_bits> &hash)
{
    return (s << hash.base32());
}

class Tiger {
    tiger_context *ctx;
private:
    Tiger(const Tiger &);
    Tiger &operator=(const Tiger &base);
public:
    enum { HASH_BITS = TIGER_HASH_BITS, HASH_SIZE = TIGER_HASH_SIZE };

    typedef Hash<HASH_BITS> hash_type;

    Tiger() : ctx(tiger_new()) {};
    ~Tiger() { tiger_free(ctx); }

    void reset() { return tiger_reset(ctx); }
    void feed(const void *block, size_t bytes_count) { return tiger_feed(ctx, block, bytes_count); }
    void finalize(void *hash) { return tiger_finalize(ctx, hash); }

    void feed(const std::string &block) { return feed(block.data(), block.size()); }
    void finalize(hash_type &hash) { return finalize(hash.data); }

    template<int _bits>
    void feed(const Hash<_bits> &hash) { return feed(hash.data, Hash<_bits>::BYTES); }
};

class TigerTree {
    tigertree_context *ctx;
private:
    TigerTree(const Tiger &);
    Tiger &operator=(const Tiger &);
public:
    enum { HASH_BITS = TIGER_HASH_BITS, HASH_SIZE = TIGER_HASH_SIZE };

    typedef Hash<HASH_BITS> hash_type;

    TigerTree() : ctx(tigertree_new()) {};
    ~TigerTree() { tigertree_free(ctx); }

    void reset() { return tigertree_reset(ctx); }
    void feed(const void *block, size_t bytes_count) { return tigertree_feed(ctx, block, bytes_count); }
    void finalize(void *hash) { return tigertree_finalize(ctx, hash); }

    void feed(const std::string &block) { return feed(block.data(), block.size()); }
    void finalize(hash_type &hash) { return finalize(hash.data); }
};

#endif
