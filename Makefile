# uncomment the two following line if you are using the development version of gimp (1.1)
GIMP11 = YES

# if you are building on linux/alpha and have libffm, uncomment the following line
LIBFFM = -lffm

# if you do not want to use the c code generator,
# comment the following line (cgen only works with gimp >= 1.0.4)
CGEN = YES

# you should not need to change anything beyond this line
# -------------------------------------------------------

ifeq ($(GIMP11),YES)
GIMPDIR = .gimp-1.1
else
GIMPDIR = .gimp
endif

ifeq ($(CGEN),YES)
CGEN_CFLAGS=-DUSE_CGEN
CGEN_LDFLAGS=-Wl,--export-dynamic
endif

CFLAGS = -D_GIMP -D_GIMPDIR=\"$(GIMPDIR)\" $(CGEN_CFLAGS) -Wall -g -O3 `gtk-config --cflags` -IlibPropList
CC = gcc

OBJECTS = mathmap.o builtins.o exprtree.o parser.o scanner.o postfix.o vars.o tags.o tuples.o internals.o macros.o userval.o argparser.o argscanner.o overload.o jump.o cgen.o builtins_compiler.o colorwell.o noise.o

mathmap : $(OBJECTS)
	$(CC) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(LIBFFM) `gtk-config --libs` -lgimp -LlibPropList -lPropList

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
	chpp builtins_interpreter.chc >builtins_interpreter.c

builtins_compiler.c : builtins_compiler.chc builtins.chc
	chpp builtins_compiler.chc >builtins_compiler.c

install : mathmap
	cp mathmap $(HOME)/$(GIMPDIR)/plug-ins/
	if [ ! -f $(HOME)/$(GIMPDIR)/mathmaprc ] ; then cp mathmaprc $(HOME)/$(GIMPDIR)/ ; fi

clean :
	rm -f *~ *.o mathmap scanner.c parser.[ch] parser.output argparser.[ch] argscanner.c core

realclean : clean
	rm -f builtins_interpreter.c builtins_compiler.c
