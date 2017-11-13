OBJS = readimage.o writeimage.o rwpng.o rwjpeg.o rwgif.o

FORMATDEFS = -DRWIMG_PNG -DRWIMG_JPEG -DRWIMG_GIF

all : librwimg.a

librwimg.a : $(OBJS)
	ar rcu librwimg.a $(OBJS)

testrwimg : testrwimg.o
	$(CC) -o testrwimg testrwimg.o librwimg.a -lpng -ljpeg -lgif

%.o : %.c
	$(CC) $(CFLAGS) $(FORMATDEFS) -g -c $<

clean :
	rm -f *~ $(OBJS) librwimg.a
