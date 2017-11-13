# $Id: Makefile 182 1999-12-21 16:55:25Z schani $

all : html info

html : lispreader_toc.html

info : lispreader.info

lispreader_toc.html : lispreader.texi
	texi2html -expandinfo lispreader.texi

lispreader.info : lispreader.texi
	makeinfo lispreader.texi

clean :
	rm -f lispreader*.html lispreader.info *~
