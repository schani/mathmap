# uncomment either the first or the second of the two following lines, depending
# on whether you have gimp 1.1 or gimp 1.0
GIMP11 = YES
#GIMP11 = NO

# if you are building on linux/alpha and have libffm, uncomment the following line
LIBFFM = -lffm
#LIBM = -lm

# if you want to use the c code generator, uncomment the following line (only gimp >=1.1)
CGEN = YES

ifeq ($(GIMP11),YES)
GIMPDIR = .gimp-1.1
else
GIMPDIR = .gimp
endif

ifeq ($(CGEN),YES)
CGEN_OBJS=cgen.o builtins_compiler.o
CGEN_CFLAGS=-DUSE_CGEN
CGEN_CHPPFLAGS=-DUSE_CGEN
CGEN_LDFLAGS=-Wl,--export-dynamic
endif

CFLAGS = -D_GIMP -D_GIMPDIR=\"$(GIMPDIR)\" $(CGEN_CFLAGS) -Wall -g -O3 `glib-config --cflags`
CC = gcc

OBJECTS = mathmap.o builtins.o exprtree.o parser.o scanner.o postfix.o vars.o tags.o tuples.o internals.o macros.o argparser.o argscanner.o overload.o jump.o $(CGEN_OBJS)

mathmap : $(OBJECTS)
	$(CC) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(LIBFFM) `gtk-config --libs` -lgimp -lPropList $(LIBM)

%.o : %.c
	$(CC) $(CFLAGS) -c $<

parser.c parser.h : parser.y
	bison -p mm -d parser.y
	mv parser.tab.c parser.c
	mv parser.tab.h parser.h

argparser.c argparser.h : argparser.y
	bison -p oa -d argparser.y
	mv argparser.tab.c argparser.c
	mv argparser.tab.h argparser.h

scanner.c : scanner.fl parser.h
	flex -Pmm scanner.fl
	mv lex.mm.c scanner.c

argscanner.c : argscanner.fl argparser.h
	flex -Poa argscanner.fl
	mv lex.oa.c argscanner.c

builtins.o : builtins.c builtins.h builtins_interpreter.c

builtins_interpreter.c : builtins_interpreter.chc builtins.chc
	chpp $(CGEN_CHPPFLAGS) builtins_interpreter.chc >builtins_interpreter.c

builtins_compiler.c : builtins_compiler.chc builtins.chc
	chpp builtins_compiler.chc >builtins_compiler.c

install : mathmap
	cp mathmap $(HOME)/$(GIMPDIR)/plug-ins/
	if [ ! -f $(HOME)/$(GIMPDIR)/mathmaprc ] ; then cp mathmaprc $(HOME)/$(GIMPDIR)/ ; fi

clean :
	rm -f *~ *.o mathmap scanner.c parser.[ch] argparser.[ch] argscanner.c builtins_interpreter.c builtins_compiler.c
