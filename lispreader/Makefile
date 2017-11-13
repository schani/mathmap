# $Id: Makefile 933 2007-03-18 14:22:54Z schani $

VERSION = 0.5

all : liblispreader.a

liblispreader.a :
	$(MAKE) -f Makefile.dist

dist :
	rm -rf lispreader-$(VERSION)
	mkdir lispreader-$(VERSION)
	mkdir lispreader-$(VERSION)/doc
	cp README COPYING NEWS lispreader-$(VERSION)/
	cp -pr lispreader.[ch] lispscan.h allocator.[ch] pools.[ch] docexample.c lispcat.c lispreader-$(VERSION)/
	cp Makefile.dist lispreader-$(VERSION)/Makefile
	cp doc/{lispreader,version}.texi lispreader-$(VERSION)/doc/
	cp doc/Makefile lispreader-$(VERSION)/doc/
	make -C lispreader-$(VERSION)/doc/
	tar -zcvf lispreader-$(VERSION).tar.gz lispreader-$(VERSION)
	rm -rf lispreader-$(VERSION)

clean :
	rm -f *~
	$(MAKE) -f Makefile.dist clean
	$(MAKE) -C doc clean
