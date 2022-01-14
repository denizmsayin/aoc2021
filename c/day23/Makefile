LIBDIR=../lib
VPATH=../lib/dheap ../lib/dhashtable

INCLUDE_FLAGS=-I$(LIBDIR)

ifdef DEBUG 
CFLAGS=-Wall -Wshadow -Wextra -O0 -g
else
CFLAGS=-Wall -O3 -DRELEASE
endif

CFLAGS += $(INCLUDE_FLAGS)

all: part1 part2

part1: part1.o dheap.o dhashtable.o

part2: part2.o dheap.o dhashtable.o

clean:
	rm -f *.o
