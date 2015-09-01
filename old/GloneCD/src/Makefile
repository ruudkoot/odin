IDIR =../include
CC=gcc
CFLAGS=-O3 -I$(IDIR)

ODIR=.
LDIR =../lib

LIBS=-lm

_DEPS = *.h io/*.c
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = glone.o io.o sg_lib.o sg_io_linux.o sg_cmds.o sg_pt_linux.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

glone: $(OBJ)
	gcc -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o *~ core $(INCDIR)/*~ 
