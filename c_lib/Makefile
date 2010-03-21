MODE?=release
TARGET_CPU?=core2duo

# aio/mmap/read
READ_ENGINE?=aio

CFLAGS+=-D__USE_LARGEFILE64
ifeq ($(CC),icc)

CFLAGS+=-O3 -ipo
ifeq ($(TARGET_CPU),core2duo)
#CFLAGS+=-xT -msse3 -march=core2 -mtune=core2
CFLAGS+=-march=core2 -mtune=core2
endif
LDFLAGS+=-fast

else

ifeq ($(MODE),debug)
#9CFLAGS+=-std=c99 -pedantic-errors -Wall -g
CFLAGS+=-Wall -g
else
CFLAGS+=-DNDEBUG
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



all: testtiger hasher_aio hasher_mmap hasher_read

clean:
	rm *.o testtiger hasher_aio hasher_mmap hasher_read

testtiger: testtiger.o tiger.o

hasher_aio.o: CFLAGS+=-DUSE_AIO
hasher_mmap.o: CFLAGS+=-DUSE_MMAP
hasher_read.o: CFLAGS+=-DUSE_READ
hasher_aio.o hasher_mmap.o hasher_read.o: hasher.c tiger.h
	$(CC) $(CFLAGS) -c -o $(@) $(<)

hasher_aio hasher_mmap hasher_read: tiger.o
hasher_aio: LDFLAGS+=-lrt -pthread

.PHONY: all clean
