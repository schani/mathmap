# If you want to build the command line version (which you can use to
# generate Blender sequence plug-ins) instead of the GIMP plug-in,
# uncomment the following line
#CMDLINE = YES

# If you build the command line version and want to have movie
# (quicktime) support, uncomment the following line.
#MOVIES = YES

# If you are building on MacOS X, uncomment the following line
#MACOSX = YES

# If you do not want localization (relevant for plug-in version only),
# comment the following line
ENABLE_NLS = YES

# Prefix of your GIMP binaries.  Usually you can leave this line
# commented.  If you have more than one GIMP versions installed, you
# should give the prefix for the one which you want to build MathMap
# for.  Only relevant for the plug-in.
GIMP_BIN = /usr/bin/

# Prefix for the software installation
PREFIX = /usr/local

# Directory where the command line binary should be installed in
BINDIR = $(PREFIX)/bin

# Directory where the localization files should be installed in
LOCALEDIR = $(PREFIX)/share/locale

# Directory where the template files (only for the command line version)
# should be installed in
TEMPLATE_DIR = $(PREFIX)/share/mathmap

# You should not need to change anything beyond this line.
# -------------------------------------------------------

VERSION = 1.1.1

#OPT_CFLAGS := -O2
OPT_CFLAGS := -g

ifeq ($(MACOSX),YES)
CGEN_CC=-DCGEN_CC="\"cc -O2 -c -fPIC -faltivec -o\""
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

GLIB_CFLAGS = `pkg-config --cflags glib-2.0`
GLIB_LDFLAGS = `pkg-config --libs glib-2.0 gmodule-2.0`

CFLAGS = -I. $(CGEN_CFLAGS) -Wall $(OPT_CFLAGS) $(GLIB_CFLAGS) -DCMDLINE $(MACOSX_CFLAGS) -DTEMPLATE_DIR=\"$(TEMPLATE_DIR)\"
LDFLAGS = $(GLIB_LDFLAGS) $(MACOSX_LIBS) -lm -ljpeg -lpng -lz
ifeq ($(MOVIES),YES)
CFLAGS += -I/usr/local/include/quicktime -DMOVIES
LDFLAGS += -lquicktime -lpthread -lz
endif

else

GIMPTOOL = $(GIMP_BIN)gimptool-2.0
GIMPDIR := .gimp-$(basename $(shell $(GIMPTOOL) --version))
GIMP_CFLAGS = `$(GIMPTOOL) --cflags` `pkg-config --cflags gmodule-2.0` -DGIMP2
GIMP_LDFLAGS = `$(GIMPTOOL) --libs` `pkg-config --libs gmodule-2.0`

CFLAGS = -I. $(CGEN_CFLAGS) $(OPT_CFLAGS) -Wall $(GIMP_CFLAGS) -DGIMP -DLOCALEDIR=\"$(LOCALEDIR)\" $(NLS_CFLAGS) $(MACOSX_CFLAGS)
LDFLAGS = $(GIMP_LDFLAGS) $(MACOSX_LIBS)

endif

ifeq ($(ENABLE_NLS),YES)
NLS_CFLAGS = -DENABLE_NLS
MOS = fr.mo
endif

CFLAGS += -DMATHMAP_VERSION=\"$(VERSION)\"

CC = gcc

COMMON_OBJECTS = mathmap_common.o builtins.o exprtree.o parser.o scanner.o postfix.o vars.o tags.o tuples.o internals.o macros.o userval.o overload.o jump.o noise.o lispreader.o spec_func.o compiler.o bitvector.o pools.o

ifeq ($(CMDLINE),YES)
OBJECTS = $(COMMON_OBJECTS) mathmap_cmdline.o readimage.o writeimage.o rwjpeg.o rwpng.o getopt.o getopt1.o generators/blender/blender.o generators/pixeltree/pixeltree.o
else
OBJECTS = $(COMMON_OBJECTS) mathmap.o
endif

mathmap : $(OBJECTS)
	$(CC) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(LDFLAGS) -lgsl -lgslcblas

#compiler_test : $(COMMON_OBJECTS) compiler_test.o
#	$(CC) $(CGEN_LDFLAGS) -o compiler_test $(COMMON_OBJECTS) compiler_test.o $(LDFLAGS) -lgsl -lgslcblas

%.o : %.c
	$(CC) $(CFLAGS) -o $@ -c $<

%.mo : %.po
	msgfmt -o $@ $<

parser.c parser.h : parser.y
	bison -d parser.y
	mv parser.tab.c parser.c
	mv parser.tab.h parser.h

scanner.c : scanner.fl parser.h
	flex scanner.fl
	mv lex.yy.c scanner.c

compiler.o : new_builtins.c opdefs.h

new_builtins.c opdefs.h : builtins.lisp ops.lisp
	clisp builtins.lisp

blender.o : generators/blender/blender.c

install : mathmap
ifneq ($(CMDLINE),YES)
	$(GIMPTOOL) --install-bin mathmap
	if [ ! -d $(HOME)/$(GIMPDIR)/mathmap ] ; then mkdir $(HOME)/$(GIMPDIR)/mathmap ; fi
	if [ ! -f $(HOME)/$(GIMPDIR)/mathmap/mathmaprc ] ; then cp mathmaprc $(HOME)/$(GIMPDIR)/mathmap/ ; fi
	cp new_template.c $(HOME)/$(GIMPDIR)/mathmap/
	cp opmacros.h $(HOME)/$(GIMPDIR)/mathmap/
else
	cp mathmap $(BINDIR)
	if [ ! -d $(TEMPLATE_DIR) ] ; then mkdir $(TEMPLATE_DIR) ; fi
	cp generators/blender/blender_template.c generators/blender/blender_opmacros.h $(TEMPLATE_DIR)
endif

install-mos : $(MOS)
	if [ ! -d $(LOCALEDIR)/fr ] ; then mkdir $(LOCALEDIR)/fr ; fi
	if [ ! -d $(LOCALEDIR)/fr/LC_MESSAGES ] ; then mkdir $(LOCALEDIR)/fr/LC_MESSAGES ; fi
	cp fr.mo $(LOCALEDIR)/fr/LC_MESSAGES/mathmap.mo

clean :
	rm -f *~ *.o generators/blender/*~ generators/blender/*.o mathmap compiler scanner.c parser.[ch] parser.output core

realclean : clean
	rm -f new_builtins.c opdefs.h .nfs* mathmap-*.tar.gz

dist : new_builtins.c clean
	rm -rf mathmap-$(VERSION)
	mkdir mathmap-$(VERSION)
	cp Makefile README README.blender BUGS ANNOUNCEMENT COPYING INSTALL *.[ch] *.lisp parser.y scanner.fl mathmaprc *.po mathmap-$(VERSION)
	mkdir mathmap-$(VERSION)/generators
	mkdir mathmap-$(VERSION)/generators/blender
	cp generators/blender/blender.[ch] generators/blender/blender_template.c generators/blender/blender_opmacros.h generators/blender/make_some_plugins mathmap-$(VERSION)/generators/blender
	mkdir mathmap-$(VERSION)/doc
	cp html/language.html html/reference.html html/cartesian.png html/graygradient.png html/clown.jpg html/sinegraph.png html/sineclown.jpg html/polar.png html/clownpond.jpg html/target.png html/rmod.jpg html/clownhole.jpg html/redgreengradient.png html/noise.jpg mathmap-$(VERSION)/doc
	tar -zcvf mathmap-$(VERSION).tar.gz mathmap-$(VERSION)
	rm -rf mathmap-$(VERSION)
