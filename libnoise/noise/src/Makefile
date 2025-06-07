LIBTOOL=libtool

# defines source files and vpaths
include Sources

DEPENDS=$(SOURCES:.cpp=.d)

# What source objects are we building?
OBJECTS=$(SOURCES:.cpp=.lo)

.PHONY: all clean cleandeps cleanobjs cleanlib libnoise libnoise.so libnoise.so.0

# hooks for future makefiles being able to make multiple SOs, or older SOs
libnoise: libnoise.la

# Real build targets
libnoise.la: $(OBJECTS)
	$(LIBTOOL) --tag=CXX --mode=link $(CXX) -static $(LDFLAGS) -o $@ $(OBJECTS:.o=.lo)

clean:	cleandeps cleanobjs cleanlib
cleandeps:
	-rm $(DEPENDS)
cleanobjs:
	-rm $(OBJECTS)
	-rm $(OBJECTS:.o=.lo) #clean up after libtool
	-rm -rf .libs model/.libs module/.libs
cleanlib:
	-rm libnoise.so.0.3
	-rm libnoise.a
	-rm libnoise.la

# Utility rules
# Generates dependancy files:
%.d: %.cpp
	@set -e; rm -f $@; \
         $(CXX) -MM $(CPPFLAGS) $< > $@.$$$$; \
         sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
         rm -f $@.$$$$

# C and C++ libtool (rather than raw CXX/CC) use
%.lo: %.cpp
	( cd $(dir $<) ; $(LIBTOOL) --tag=CXX --mode=compile $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $(notdir $<) -o $(notdir $@) )

%.lo: %.c
	( cd $(dir $<) ; $(LIBTOOL) --tag=CC --mode=compile $(CC) $(CPPFLAGS) $(CFLAGS) -c $(notdir $<) -o $(notdir $@) )

# If dependancies have never been built this will produce a horde of
# "file not found" warnings and *then* build the deps.  Very odd.
include $(DEPENDS)
