#include <unistd.h>

#include <vector>
#include <iostream>

#include "Tiger.hpp"

class TTH {
    Tiger tiger;
    std::vector<Tiger::hash_type> leafs;
public:
    typedef Tiger::hash_type hash_type;

    void readLeafs(int fd)
    {
        char block[4096];
        char *p;
        ssize_t r;
        size_t left = 1024;

        leafs.clear();
        tiger.reset();
        tiger.feed("\0",1);
        while((r = read(fd, block, sizeof(block))) > 0) {
            p = block;
            while (1) {
                if (((size_t)r) < left) {
                    left -= r;
                    tiger.feed(p, r);
                    break;
                }

                tiger.feed(p, left);
                p += left;
                r -= left;

                leafs.resize(leafs.size()+1);
                tiger.finalize(leafs.back());
                tiger.reset();
                tiger.feed("\0",1);
                left = 1024;
            }
        }
        if (leafs.empty() || (left < 1024)) {
            leafs.resize(leafs.size()+1);
            tiger.finalize(leafs.back());
        }
    }

    void getHash(size_t l, size_t n, hash_type &hash)
    {
        if (n == 1) {
            return ((void)(hash = leafs[l]));
        }
        else if (n == 2) {
            tiger.reset();
            tiger.feed("\1", 1);
            tiger.feed(leafs[l]);
            tiger.feed(leafs[l+1]);
            return tiger.finalize(hash);
        }

        assert(n > 0);

        size_t m = 1;
        while((m*2) < n) m *= 2;

        hash_type a, b;
        getHash(l, m, a);
        getHash(l+m, n-m, b);
        
        tiger.reset();
        tiger.feed("\1", 1);
        tiger.feed(a);
        tiger.feed(b);
        return tiger.finalize(hash);
    }

    void finalize(hash_type &hash)
    {
        std::cerr << leafs.size() << std::endl;
        return getHash(0, leafs.size(), hash);
    }
};


int main()
{
    TTH tth;
    TTH::hash_type hash;
    
    tth.readLeafs(0);
    tth.finalize(hash);
    std::cout << hash.hex() << std::endl;
    return 0;
}
