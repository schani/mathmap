# if you want to build the command line version instead of the GIMP
# plug-in, uncomment the following line
#CMDLINE = YES

# if you want to build the GIMP plug-in, but want to build it for the old GIMP
# (version 1.2), or you want to compile the command line version, but only
# have the old glib, comment the following line
#GIMP2 = YES

# if you build the command line version and want to have movie
# (quicktime) support, uncomment the following line
#MOVIES = YES

# if you are building on MacOS X, uncomment the following line
MACOSX = YES

# if you are building on linux/alpha and have libffm, uncomment the
# following line
#LIBFFM = -lffm

# if you do not want localization (relevant for plug-in version only),
# comment the following line
ENABLE_NLS = YES

# directory where the localization files should be installed in
LOCALEDIR = /usr/local/share/locale

# you should not need to change anything beyond this line
# -------------------------------------------------------

VERSION = 0.14

#OPT_CFLAGS := -O2
OPT_CFLAGS := -g

ifeq ($(MACOSX),YES)
CGEN_CC=-DCGEN_CC="\"cc -c -fPIC -faltivec -o\""
CGEN_LD=-DCGEN_LD="\"cc -bundle -flat_namespace -undefined suppress -o\""
MACOSX_LIBS=-lmx
MACOSX_CFLAGS=-I/sw/include
else
CGEN_CC=-DCGEN_CC="\"gcc -O2 -c -fPIC -o\""
CGEN_LD=-DCGEN_LD="\"gcc -shared -o\""
endif

CGEN_CFLAGS=$(CGEN_CC) $(CGEN_LD)
#CGEN_LDFLAGS=-Wl,--export-dynamic

ifeq ($(CMDLINE),YES)
ifeq ($(GIMP2),YES)
GLIB_CFLAGS = `pkg-config --cflags glib-2.0`
GLIB_LDFLAGS = `pkg-config --libs glib-2.0 gmodule-2.0`
else
GLIB_CFLAGS = `glib-config --cflags`
GLIB_LDFLAGS = `glib-config --libs gmodule`
endif

CFLAGS = -I. $(CGEN_CFLAGS) -Wall $(OPT_CFLAGS) $(GLIB_CFLAGS) -DCMDLINE $(MACOSX_CFLAGS)
LDFLAGS = $(LIBFFM) $(GLIB_LDFLAGS) $(MACOSX_LIBS) -lm -ljpeg -lpng -lz
ifeq ($(MOVIES),YES)
CFLAGS += -I/usr/local/include/quicktime -DMOVIES
LDFLAGS += -lquicktime -lpthread -lz
endif

else

ifeq ($(GIMP2),YES)
GIMPTOOL = gimptool-2.0
GIMPDIR := .gimp-$(notdir $(shell $(GIMPTOOL) --gimpdatadir))
GIMP_CFLAGS = `gimptool-2.0 --cflags` -DGIMP2
GIMP_LDFLAGS = `gimptool-2.0 --libs`
else
GIMPTOOL = gimptool
GIMPDIR := .gimp-$(notdir $(shell $(GIMPTOOL) --gimpdatadir))
GIMP_CFLAGS = `gimp-config --cflags`
GIMP_LDFLAGS = `gimp-config --libs`
endif

CFLAGS = -I. $(CGEN_CFLAGS) $(OPT_CFLAGS) -Wall $(GIMP_CFLAGS) -DGIMP -DLOCALEDIR=\"$(LOCALEDIR)\" $(NLS_CFLAGS) $(MACOSX_CFLAGS)
LDFLAGS = $(LIBFFM) $(GIMP_LDFLAGS) $(MACOSX_LIBS)

endif

ifeq ($(ENABLE_NLS),YES)
NLS_CFLAGS = -DENABLE_NLS
MOS = fr.mo
endif

CFLAGS += -DMATHMAP_VERSION=\"$(VERSION)\"

CC = gcc

COMMON_OBJECTS = mathmap_common.o builtins.o exprtree.o parser.o scanner.o postfix.o vars.o tags.o tuples.o internals.o macros.o userval.o overload.o jump.o noise.o lispreader.o spec_func.o compiler.o bitvector.o

ifeq ($(CMDLINE),YES)
OBJECTS = $(COMMON_OBJECTS) mathmap_cmdline.o readimage.o writeimage.o rwjpeg.o rwpng.o getopt.o getopt1.o
else
OBJECTS = $(COMMON_OBJECTS) mathmap.o
endif

mathmap : $(OBJECTS)
	$(CC) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(LDFLAGS) -lgsl -lgslcblas

#compiler_test : $(COMMON_OBJECTS) compiler_test.o
#	$(CC) $(CGEN_LDFLAGS) -o compiler_test $(COMMON_OBJECTS) compiler_test.o $(LDFLAGS) -lgsl -lgslcblas

%.o : %.c
	$(CC) $(CFLAGS) -c $<

%.mo : %.po
	msgfmt -o $@ $<

parser.c parser.h : parser.y
	bison -d parser.y
	mv parser.tab.c parser.c
	mv parser.tab.h parser.h

scanner.c : scanner.fl parser.h
	flex scanner.fl
	mv lex.yy.c scanner.c

compiler.o : new_builtins.c

new_builtins.c : builtins.lisp
	clisp builtins.lisp

install : mathmap
ifneq ($(CMDLINE),YES)
	$(GIMPTOOL) --install-bin mathmap
	if [ ! -d $(HOME)/$(GIMPDIR)/mathmap ] ; then mkdir $(HOME)/$(GIMPDIR)/mathmap ; fi
	if [ ! -f $(HOME)/$(GIMPDIR)/mathmap/mathmaprc ] ; then cp mathmaprc $(HOME)/$(GIMPDIR)/mathmap/ ; fi
	cp new_template.c $(HOME)/$(GIMPDIR)/mathmap/
endif

install-mos : $(MOS)
	if [ ! -d $(LOCALEDIR)/fr ] ; then mkdir $(LOCALEDIR)/fr ; fi
	if [ ! -d $(LOCALEDIR)/fr/LC_MESSAGES ] ; then mkdir $(LOCALEDIR)/fr/LC_MESSAGES ; fi
	cp fr.mo $(LOCALEDIR)/fr/LC_MESSAGES/mathmap.mo

clean :
	rm -f *~ *.o mathmap compiler scanner.c parser.[ch] parser.output core

realclean : clean
	rm -f new_builtins.c .nfs* mathmap-*.tar.gz

dist : new_builtins.c clean
	rm -rf mathmap-$(VERSION)
	mkdir mathmap-$(VERSION)
	cp Makefile README ANNOUNCEMENT COPYING INSTALL HACKING *.[ch] builtins.lisp parser.y scanner.fl mathmaprc *.po mathmap-$(VERSION)
	mkdir mathmap-$(VERSION)/doc
	cp html/language.html html/reference.html html/cartesian.png html/graygradient.png html/clown.jpg html/sinegraph.png html/sineclown.jpg html/polar.png html/clownpond.jpg html/target.png html/rmod.jpg html/clownhole.jpg html/redgreengradient.png html/noise.jpg mathmap-$(VERSION)/doc
	tar -zcvf mathmap-$(VERSION).tar.gz mathmap-$(VERSION)
	rm -rf mathmap-$(VERSION)
