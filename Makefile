# if you do not want to use the c code generator,
# comment the following line
CGEN = YES

# if you are building on linux/alpha and have libffm, uncomment the following line
LIBFFM = -lffm

# you should not need to change anything beyond this line
# -------------------------------------------------------

ifeq ($(CGEN),YES)
CGEN_CC=-DCGEN_CC="\"gcc -O -g -c -fPIC -o\""
CGEN_LD=-DCGEN_LD="\"gcc -shared -o\""

CGEN_CFLAGS=-DUSE_CGEN $(CGEN_CC) $(CGEN_LD)
CGEN_LDFLAGS=-Wl,--export-dynamic
endif

GIMPDIR := .gimp-$(notdir $(shell gimptool --gimpdatadir))

CFLAGS = -I. $(CGEN_CFLAGS) -Wall -g `gtk-config --cflags`
CC = gcc

OBJECTS = mathmap.o builtins.o exprtree.o parser.o scanner.o postfix.o vars.o tags.o tuples.o internals.o macros.o userval.o overload.o jump.o cgen.o builtins_compiler.o colorwell.o noise.o lispreader.o

mathmap : $(OBJECTS)
	$(CC) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(LIBFFM) `gtk-config --libs` -lgimp -lgimpui

%.o : %.c
	$(CC) $(CFLAGS) -c $<

parser.c parser.h : parser.y
	bison -p mm -d parser.y
	mv parser.tab.c parser.c
	mv parser.tab.h parser.h

scanner.c : scanner.fl parser.h
	flex -Pmm scanner.fl
	mv lex.mm.c scanner.c

builtins.o : builtins.c builtins.h builtins_interpreter.c

builtins_interpreter.c : builtins_interpreter.chc builtins.chc
	chpp builtins_interpreter.chc >builtins_interpreter.c

builtins_compiler.c : builtins_compiler.chc builtins.chc
	chpp builtins_compiler.chc >builtins_compiler.c

install : mathmap
	gimptool --install-bin mathmap
	if [ ! -f $(HOME)/$(GIMPDIR)/mathmaprc ] ; then cp mathmaprc $(HOME)/$(GIMPDIR)/ ; fi

clean :
	rm -f *~ *.o mathmap scanner.c parser.[ch] parser.output core

realclean : clean
	rm -f builtins_interpreter.c builtins_compiler.c
