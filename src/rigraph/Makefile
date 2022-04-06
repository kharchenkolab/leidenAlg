CXX ?= g++
PKG_CXXFLAGS = -O3 -std=c++11 -pthread -fPIC $(SHLIB_OPENMP_CXXFLAGS)
PKG_CXXFLAGS += -I"./include" -I"../rigraph/include"
LDFLAGS += -lpthread


LIB = ../libigraph.a
lib: $(LIB)

$(LIB): rinterface.o
	ar rvs $@ $?

clean:
	@-rm -f *.o $(LIB) 


