LIBDIR=../lib
VPATH=../lib/dhashtable

INCLUDE_FLAGS=-I$(LIBDIR)

CFLAGS=-Wall -O3 $(INCLUDE_FLAGS)

all: part1 part2

part1: part1.o dhashtable.o

part2: part2.o dhashtable.o

clean:
	rm -f *.o
