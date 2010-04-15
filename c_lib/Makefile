MODE?=release
TARGET_CPU?=core2duo

# aio/mmap/read
READ_ENGINE?=aio

CFLAGS+=$(CDEFS)

CDEFS+=-D__USE_LARGEFILE64
ifeq ($(CC),icc)
CXX = $(CC) -x c++

CFLAGS+=-O3 -ipo
ifeq ($(TARGET_CPU),core2duo)
#CFLAGS+=-xT -msse3 -march=core2 -mtune=core2
CFLAGS+=-xT
endif
LDFLAGS+=-fast

else

CXXFLAGS+=$(CFLAGS)
ifeq ($(MODE),debug)
#9CFLAGS+=-std=c99 -pedantic-errors -Wall -g
CFLAGS+=-Wall -g
CFLAGS+=-Wall -g
else
CDEFS+=-DNDEBUG
endif
CFLAGS+=-fhosted
CFLAGS+=-O3 -maccumulate-outgoing-args -minline-all-stringops -pipe
ifeq ($(TARGET_CPU),core2duo)
CFLAGS+=-march=core2 -mtune=core2 -msse -msse2 -msse3
endif
LDFLAGS+=-O3

endif

ifeq ($(READ_ENGINE),aio)
#CFLAGS+=-DUSE_AIO
#LDFLAGS+=-lrt -pthread
else ifeq ($(READ_ENGINE),mmap)
#CFLAGS+=-DUSE_MMAP
else ifeq ($(READ_ENGINE),read)
#CFLAGS+=-DUSE_READ
endif



all: testtiger hasher_aio hasher_mmap hasher_read testtiger.llvm tth tigertree.o pipehash dhash

clean:
	rm -f *.o *.a *.bc testtiger testtiger.llvm  hasher_aio hasher_mmap hasher_read dhash pipehash tth

pipehash: pipehash.o libtigerhash.a
dhash: dhash.o libtigerhash.a
	$(CXX) $(LDFLAGS) -o $(@) $(^)

dhash.o: Tiger.hpp tiger.h tigertree.h

testtiger: testtiger.o libtigerhash.a
tth.o: tiger.h
tth: tth.o libtigerhash.a
	$(CXX) $(LDFLAGS) -o $(@) $(^)


tth.o: CXXFLAGS+=-std=c++0x


hasher_aio.o: CDEFS+=-DUSE_AIO
hasher_mmap.o: CDEFS+=-DUSE_MMAP
hasher_read.o: CDEFS+=-DUSE_READ
hasher_aio.o hasher_mmap.o hasher_read.o: hasher.c tiger.h
	$(CC) $(CFLAGS) -c -o $(@) $(<)

hasher_aio hasher_mmap hasher_read: tiger.o
hasher_aio: LDFLAGS+=-lrt -pthread

libtigerhash.a: tiger.o tigertree.o
	$(AR) -cru $(@) $(^)
	ranlib $(@)

.PHONY: all clean

testtiger.llvm: testtiger.bc tiger.bc
	llvm-ld -o $(@) $^


.SUFFIXES: .bc .ll .s .o .c

.ll.bc:
	llvm-as -f -o="$(@)" "$(<)" && chmod +x "$@"
#	m4 "$<" | llvm-as -f -o="$(@)" && chmod +x "$@"

.bc.o:
	llc -f -O=3 -filetype=obj -stats -tailcallopt -time-passes -verify-dom-info "$<"

.bc.s:
	llc -f -O=3 -asm-verbose -filetype=asm -stats -tailcallopt -time-passes -verify-dom-info "$<"

.c.ll:
	clang $(CDEFS) -O4 -S -emit-llvm -o $(@) $(<)

.c.bc:
	clang $(CDEFS) -O4 -c -emit-llvm -o $(@) $(<)
