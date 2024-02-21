CXX ?= g++
PKG_CXXFLAGS = -O3 -pthread -fPIC $(SHLIB_OPENMP_CXXFLAGS)
PKG_CXXFLAGS += -I"./include" -I"../rigraph/include"

LIB = ../leidenalg.o
lib: $(LIB)

$(LIB): CPMVertexPartition.o LinearResolutionParameterVertexPartition.o RBConfigurationVertexPartition.o SurpriseVertexPartition.o ModularityVertexPartition.o RBERVertexPartition.o GraphHelper.o MutableVertexPartition.o ResolutionParameterVertexPartition.o Optimiser.o SignificanceVertexPartition.o
	$(CXX) -r $? -o $@

clean:
	@-rm -f *.o $(LIB)


