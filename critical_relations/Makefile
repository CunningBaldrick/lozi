CXXFLAGS=-DBOOST_UBLAS_TYPE_CHECK=0 -g -O3 -DNDEBUG
OBJECTS=affine_maps.o critical.o symbol_sequences.o

default: m

m: m.o $(OBJECTS) Makefile
	$(CXX) -o m m.o $(OBJECTS)

%.o : %.cpp Makefile
	$(CXX) -c $(CXXFLAGS) $<

clean::
	rm -f *.o m
