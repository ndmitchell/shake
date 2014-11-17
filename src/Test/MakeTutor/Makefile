# From http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/, Makefile 4

CC=gcc
CFLAGS=-I.
DEPS = hellomake.h
OBJ = hellomake.o hellofunc.o 

hellomake$(EXE): $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

.PHONY: clean
clean:
	rm hellomake$(EXE)
	rm *.o
