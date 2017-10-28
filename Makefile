# If you're building on MinGW32, uncomment the following line
#MINGW32 = YES

# Uncomment this line if you want to use the LLVM backend.  This is
# compulsory for MinGW32!
#USE_LLVM = YES

# Prefix of your GIMP binaries.  Usually you can leave this line
# commented.  If you have more than one GIMP versions installed, you
# should give the prefix for the one which you want to build MathMap
# for.
#GIMP_BIN = /usr/bin/

# Choose which gif library you have.
GIFLIB = -lgif
#GIFLIB = -lungif

# If you are building on MacOS X, uncomment the following line
#MACOSX = YES

# If you want the final GIMP rendering to be multi-threaded then
# uncomment the following line
THREADED = -DTHREADED_FINAL_RENDER

# If want to have movie (Quicktime) support in the command line,
# uncomment the following line.  Please not that this feature hasn't
# been maintained for quite some time and probably doesn't work.
#MOVIES = YES

# If you want to install in your home directory instead of globally,
# uncomment the following line.
#INSTALL_LOCAL = YES

# Prefix for the software installation
ifeq ($(INSTALL_LOCAL),YES)
PREFIX = ~/.local
else
PREFIX = /usr
endif

# You should not need to change anything beyond this line.
# -------------------------------------------------------

VERSION = 1.3.5

CFLAGS = -O2 -Wall
#CFLAGS = -O0 -g -Wall #-fgnu89-inline
#DEBUG_CFLAGS := -DPRINT_FPS -DDEBUG_OUTPUT -DDONT_UNLINK_C

#PROF_FLAGS := -pg

ifeq ($(MACOSX),YES)
CGEN_CC=-DCGEN_CC="\"cc -O2 -c -fPIC -faltivec -o\""
CGEN_LD=-DCGEN_LD="\"cc -bundle -flat_namespace -undefined suppress -o\""
MACOSX_LIBS=-lmx
MACOSX_CFLAGS=-I/sw/include
else
CGEN_CC=-DCGEN_CC="\"gcc -O2 -c -fPIC -o\""
#CGEN_CC=-DCGEN_CC="\"gcc -O0 -g -c -fPIC -o\""
CGEN_LD=-DCGEN_LD="\"gcc -shared -o\""
endif

ifeq ($(MINGW32),YES)
MINGW_CFLAGS = -mms-bitfields -I/include
MINGW_LDFLAGS = -lpsapi -limagehlp -mwindows
LLVM_GCC = /local/llvm-gcc-4.2/bin/llvm-gcc
FORMATDEFS = -DRWIMG_PNG
FORMAT_LDFLAGS = -lpng12
else
FORMATDEFS = -DRWIMG_JPEG -DRWIMG_PNG -DRWIMG_GIF
FORMAT_LDFLAGS = -ljpeg -lpng $(GIFLIB)
LLVM_GCC = llvm-gcc
endif

ifeq ($(USE_LLVM),YES)
LLVM_CFLAGS = -DUSE_LLVM
LLVM_LDFLAGS = $(shell llvm-config --ldflags --libs engine bitreader ipo)
LLVM_CXXFLAGS = `llvm-config --cxxflags`
LLVM_OBJECTS = backends/llvm.o
LLVM_TARGETS = llvm_template.o
endif

GTKSOURCEVIEW_CFLAGS = -DUSE_GTKSOURCEVIEW $(shell pkg-config --cflags gtksourceview-2.0)
GTKSOURCEVIEW_LDFLAGS = $(shell pkg-config --libs gtksourceview-2.0)

FFTW = fftw3
FFTW_OBJECTS = native-filters/convolve.o
FFTW_CFLAGS = -DHAVE_FFTW

PTHREADS = -DUSE_GTHREADS

CGEN_CFLAGS=$(CGEN_CC) $(CGEN_LD)
#CGEN_LDFLAGS=-Wl,--export-dynamic

GIMPTOOL := $(GIMP_BIN)gimptool-2.0
GIMPDIR := .gimp-$(basename $(shell $(GIMPTOOL) --version))
ifeq ($(INSTALL_LOCAL),YES)
GIMPDATADIR := ~/.gimp-2.8
PLUGIN_DIR = $(GIMPDATADIR)/plug-ins
else
GIMPDATADIR := $(PREFIX)/share/gimp/2.0
#FIXME: does not honor PREFIX
PLUGIN_DIR = $(shell $(GIMPTOOL) --libdir)/gimp/2.0/plug-ins
#PLUGIN_DIR = $(PREFIX)/lib/gimp/2.0/plug-ins
endif
GIMP_CFLAGS := $(shell $(GIMPTOOL) --cflags) $(shell pkg-config --cflags gmodule-2.0 gthread-2.0 gobject-2.0 $(FFTW))
GIMP_LDFLAGS := $(shell $(GIMPTOOL) --libs) $(shell pkg-config --libs gmodule-2.0 gthread-2.0 gobject-2.0 $(FFTW))

TEMPLATE_DIR = $(GIMPDATADIR)/mathmap
PIXMAP_DIR = $(GIMPDATADIR)/mathmap
LOCALEDIR = $(PREFIX)/share/locale

C_CXX_FLAGS = -I. -I/usr/local/include -D_GNU_SOURCE $(CFLAGS) $(CGEN_CFLAGS) $(GIMP_CFLAGS) -DLOCALEDIR=\"$(LOCALEDIR)\" -DTEMPLATE_DIR=\"$(TEMPLATE_DIR)\" -DPIXMAP_DIR=\"$(PIXMAP_DIR)\" $(NLS_CFLAGS) $(MACOSX_CFLAGS) $(THREADED) $(PROF_FLAGS) $(MINGW_CFLAGS) $(LLVM_CFLAGS) $(FFTW_CFLAGS) $(PTHREADS) $(DEBUG_CFLAGS) $(GTKSOURCEVIEW_CFLAGS)
MATHMAP_CFLAGS = $(C_CXX_FLAGS) -std=gnu99
MATHMAP_CXXFLAGS = $(C_CXX_FLAGS) $(LLVM_CXXFLAGS) $(CXXFLAGS)
MATHMAP_LDFLAGS = $(LDFLAGS) $(GIMP_LDFLAGS) $(MACOSX_LIBS) -lm -lgsl -lgslcblas libnoise/noise/lib/libnoise.a $(PROF_FLAGS) $(MINGW_LDFLAGS) $(GTKSOURCEVIEW_LDFLAGS)

ifeq ($(MOVIES),YES)
MATHMAP_CFLAGS += -I/usr/local/include/quicktime -DMOVIES
MATHMAP_LDFLAGS += -lquicktime -lpthread
endif

CMDLINE_OBJECTS = mathmap_cmdline.o getopt.o getopt1.o generators/blender/blender.o
CMDLINE_LIBS = rwimg/librwimg.a
CMDLINE_TARGETS = librwimg
MATHMAP_CFLAGS += -DGIMPDATADIR=\"$(GIMPDATADIR)\"
MATHMAP_LDFLAGS += $(FORMAT_LDFLAGS)

NLS_CFLAGS = -DENABLE_NLS
MOS = fr.mo ru.mo

MATHMAP_CFLAGS += -DMATHMAP_VERSION=\"$(VERSION)\"

CC = gcc
CXX = g++

export CFLAGS CC

CURVE_OBJECTS = \
	curve/curve_widget.o \
	curve/gegl-curve.o


COMMON_OBJECTS = mathmap_common.o builtins/builtins.o exprtree.o parser.o scanner.o vars.o tags.o tuples.o internals.o macros.o userval.o overload.o jump.o builtins/libnoise.o builtins/spec_func.o compiler.o bitvector.o expression_db.o drawable.o floatmap.o tree_vectors.o mmpools.o designer/designer.o designer/cycles.o designer/loadsave.o designer_filter.o native-filters/gauss.o native-filters/cache.o compopt/dce.o compopt/resize.o compopt/licm.o compopt/simplify.o backends/cc.o backends/lazy_creator.o $(FFTW_OBJECTS) $(LLVM_OBJECTS) $(CURVE_OBJECTS)
#COMMON_OBJECTS += designer/widget.o
COMMON_OBJECTS += designer/cairo_widget.o

GIMP_OBJECTS = mathmap.o

OBJECTS = $(COMMON_OBJECTS) $(CMDLINE_OBJECTS) $(GIMP_OBJECTS)

TEMPLATE_INPUTS = tuples.h mathmap.h userval.h drawable.h compiler.h mmpools.h builtins/builtins.h builtins/libnoise.h tree_vectors.h native-filters/native-filters.h

mathmap : libnoise/noise/lib/libnoise.a compiler_types.h $(OBJECTS) $(CMDLINE_TARGETS) liblispreader new_template.c $(LLVM_TARGETS)
	$(CXX) $(CGEN_LDFLAGS) -o mathmap $(OBJECTS) $(CMDLINE_LIBS) $(LLVM_LDFLAGS) lispreader/liblispreader.a $(MATHMAP_LDFLAGS)

librwimg :
	$(MAKE) -C rwimg "FORMATDEFS=$(FORMATDEFS)" "CFLAGS=$(MINGW_CFLAGS)"

liblispreader :
	$(MAKE) -C lispreader -f Makefile.dist

libnoise :
	mkdir libnoise
	cd libnoise ; unzip ../libnoisesrc-1.0.0.zip
	cd libnoise ; patch -p1 <../libnoise-static.diff
	cd libnoise ; patch -p1 <../libnoise-bestest.diff
	cd libnoise ; patch -p1 <../libnoise-libtool-tags.diff

libnoise/noise/lib/libnoise.a : libnoise
	cd libnoise/noise ; make CFLAGS=-O3 CXXFLAGS=-O3 src include lib

#compiler_test : $(COMMON_OBJECTS) compiler_test.o
#	$(CC) $(CGEN_LDFLAGS) -o compiler_test $(COMMON_OBJECTS) compiler_test.o $(MATHMAP_LDFLAGS) -lgsl -lgslcblas

%.o : %.c
	$(CC) $(MATHMAP_CFLAGS) $(FORMATDEFS) -o $@ -c $<

%.mo : %.po
	msgfmt -o $@ $<

parser.c parser.h : parser.y
	bison -d parser.y
	mv parser.tab.c parser.c
	mv parser.tab.h parser.h

compiler.o : compiler.c new_builtins.c opdefs.h opfuncs.h compiler_types.h
	$(CC) $(MATHMAP_CFLAGS) $(FORMATDEFS) -o $@ -c compiler.c

compopt/simplify.o : compopt/simplify_func.c

backends/cc.o : compiler_types.h

backends/llvm.o : backends/llvm.cpp compiler_types.h
	$(CXX) $(MATHMAP_CXXFLAGS) $(FORMATDEFS) -o $@ -c backends/llvm.cpp

backends/lazy_creator.cpp : exported_symbols
	perl -- make_lazy_creator.pl exported_symbols >$@

backends/lazy_creator.o : backends/lazy_creator.cpp
	$(CXX) $(MATHMAP_CXXFLAGS) $(FORMATDEFS) -o $@ -c backends/lazy_creator.cpp

builtins/libnoise.o : builtins/libnoise.cpp builtins/libnoise.h
	$(CXX) $(MATHMAP_CXXFLAGS) -Ilibnoise/noise/include -o $@ -c builtins/libnoise.cpp

new_builtins.c opdefs.h opfuncs.h compiler_types.h llvm-ops.h compopt/simplify_func.c : builtins.lisp ops.lisp simplify.lisp
	clisp builtins.lisp

new_template.c : make_template.pl new_template.c.in $(TEMPLATE_INPUTS)
	perl -- make_template.pl $(TEMPLATE_INPUTS) new_template.c.in >new_template.c

llvm_template.c : make_template.pl llvm_template.c.in $(TEMPLATE_INPUTS)
	perl -- make_template.pl $(TEMPLATE_INPUTS) llvm_template.c.in >llvm_template.c

llvm_template.o : llvm_template.c opmacros.h
	$(LLVM_GCC) -emit-llvm -Wall -O3 -c llvm_template.c

blender.o : generators/blender/blender.c

install : mathmap new_template.c $(MOS)
	install -d $(DESTDIR)$(PREFIX)/bin
	install -d $(DESTDIR)$(PLUGIN_DIR)
	install -d $(DESTDIR)$(TEMPLATE_DIR)
	install -d $(DESTDIR)$(PIXMAP_DIR)
	install -d $(DESTDIR)$(PREFIX)/share/gtksourceview-2.0/language-specs
	install mathmap $(DESTDIR)$(PREFIX)/bin/mathmap
	ln -s -f $(PREFIX)/bin/mathmap $(DESTDIR)$(PLUGIN_DIR)
	cp new_template.c opmacros.h lispreader/pools.h $(DESTDIR)$(TEMPLATE_DIR)
	cp pixmaps/*.png $(DESTDIR)$(PIXMAP_DIR)
	cp mathmap.lang $(DESTDIR)$(PREFIX)/share/gtksourceview-2.0/language-specs
	cp -Tr examples $(DESTDIR)$(TEMPLATE_DIR)/expressions
	for i in $(MOS); do	\
		lng=`echo $$i | sed "s/\.mo//"`;	\
		install -d $(DESTDIR)$(LOCALEDIR)/$$lng/LC_MESSAGES;	\
		cp $$lng.mo $(DESTDIR)$(LOCALEDIR)/$$lng/LC_MESSAGES/mathmap.mo; \
	done

clean :
	rm -f *.o builtins/*.o designer/*.o native-filters/*.o compopt/*.o backends/*.o generators/blender/*.o mathmap compiler parser.output core
	find . -name '*~' -exec rm {} ';'
	$(MAKE) -C rwimg clean
	$(MAKE) -C lispreader clean
	rm -rf debian/mathmap debian/mathmap.substvars libnoise

realclean : clean
	rm -f new_builtins.c opdefs.h opfuncs.h llvm-ops.h new_template.c llvm_template.c backends/lazy_creator.cpp compiler_types.h parser.[ch] .nfs* mathmap-*.tar.gz

TAGS :
	etags `find . -name '*.c' -o -name '*.h' -o -name '*.lisp' -o -name '*.cpp'`

dist : new_builtins.c parser.c new_template.c backends/lazy_creator.cpp clean
	rm -rf mathmap-$(VERSION)
	mkdir mathmap-$(VERSION)
	cp Makefile README README.blender README.filters README.git ANNOUNCEMENT COPYING INSTALL new_template.c.in *.[ch] builtins.lisp ops.lisp parser.y make_template.pl *.po exported_symbols mathmap.lang libnoisesrc-1.0.0.zip libnoise-*.diff mathmap-$(VERSION)
	chpp -Dversion=$(VERSION) --meta-char=\\ <mathmap.spec.in >mathmap-$(VERSION)/mathmap.spec
	mkdir mathmap-$(VERSION)/debian
	cp debian/compat debian/control debian/copyright debian/dirs debian/docs debian/rules mathmap-$(VERSION)/debian
	chpp -Dversion=$(VERSION) <debian/files.in >mathmap-$(VERSION)/debian/files
	chpp -Dversion=$(VERSION) -Ddate="`date -R`" <debian/changelog.in >mathmap-$(VERSION)/debian/changelog
	mkdir mathmap-$(VERSION)/lisp-utils
	cp lisp-utils/*.lisp mathmap-$(VERSION)/lisp-utils
	mkdir mathmap-$(VERSION)/generators
	mkdir mathmap-$(VERSION)/generators/blender
	cp generators/blender/blender.[ch] generators/blender/blender_template.c generators/blender/blender_opmacros.h generators/blender/make_some_plugins mathmap-$(VERSION)/generators/blender
	mkdir mathmap-$(VERSION)/designer
	cp designer/*.[ch] mathmap-$(VERSION)/designer
	mkdir mathmap-$(VERSION)/native-filters
	cp native-filters/*.[ch] mathmap-$(VERSION)/native-filters
	mkdir mathmap-$(VERSION)/compopt
	cp compopt/*.[ch] mathmap-$(VERSION)/compopt
	mkdir mathmap-$(VERSION)/backends
	cp backends/*.[ch] mathmap-$(VERSION)/backends
	cp backends/*.cpp mathmap-$(VERSION)/backends
	mkdir mathmap-$(VERSION)/builtins
	cp builtins/*.[ch] builtins/*.cpp mathmap-$(VERSION)/builtins
	mkdir mathmap-$(VERSION)/doc
	cp html/language.html html/reference.html html/cartesian.png html/gray_gradient.jpg html/finn.jpg html/sinegraph.png html/sine_finn.jpg html/polar.png html/finn_pond.jpg html/target.jpg html/rmod.jpg html/finn_vignette.jpg html/redgreengradient.jpg html/noise_demo.jpg mathmap-$(VERSION)/doc
	mkdir mathmap-$(VERSION)/pixmaps
	cp pixmaps/*.png mathmap-$(VERSION)/pixmaps
	cp -rL examples lispreader rwimg mathmap-$(VERSION)/
	rm -rf mathmap-$(VERSION)/examples/Test
	rm -f mathmap-$(VERSION)/examples/*.mm
	rm -rf `find mathmap-$(VERSION) -name '.git'`
	touch mathmap-$(VERSION)/parser.[ch] mathmap-$(VERSION)/new_builtins.c mathmap-$(VERSION)/opdefs.h mathmap-$(VERSION)/opfuncs.h mathmap-$(VERSION)/compiler_types.h
	tar -zcvf mathmap-$(VERSION).tar.gz mathmap-$(VERSION)
	rm -rf mathmap-$(VERSION)

mingw-dist : mathmap llvm_template.o
	rm -rf mathmap-$(VERSION)-mingw32
	mkdir mathmap-$(VERSION)-mingw32
	mkdir mathmap-$(VERSION)-mingw32/plug-ins
	mkdir mathmap-$(VERSION)-mingw32/mathmap
	mkdir mathmap-$(VERSION)-mingw32/plug-ins/share
	mkdir mathmap-$(VERSION)-mingw32/plug-ins/share/gtksourceview-2.0
	mkdir mathmap-$(VERSION)-mingw32/plug-ins/share/gtksourceview-2.0/language-specs
	cp README.windows mathmap-$(VERSION)-mingw32/README.txt
	cp COPYING mathmap-$(VERSION)-mingw32/
	sed "s/\$$version/$(VERSION)/g" <mathmap.iss.in >mathmap-$(VERSION)-mingw32/mathmap.iss
	strip mathmap.exe
	cp mathmap.exe mathmap-$(VERSION)-mingw32/plug-ins/
	cp /bin/intl.dll /bin/libgsl.dll /bin/libgslcblas.dll /bin/libgtksourceview-2.0-0.dll /bin/libfftw3-3.dll mathmap-$(VERSION)-mingw32/plug-ins/
	cp llvm_template.o pixmaps/*.png mathmap-$(VERSION)-mingw32/mathmap/
	cp mathmap.lang mathmap-$(VERSION)-mingw32/plug-ins/share/gtksourceview-2.0/language-specs/
	cp -a /share/gtksourceview-2.0/styles mathmap-$(VERSION)-mingw32/plug-ins/share/gtksourceview-2.0/
cp -a examples mathmap-$(VERSION)-mingw32/mathmap/expressions
