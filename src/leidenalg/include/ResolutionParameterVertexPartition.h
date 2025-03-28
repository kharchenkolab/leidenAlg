#ifndef RESOLUTIONPARAMETERVERTEXPARTITION_H
#define RESOLUTIONPARAMETERVERTEXPARTITION_H

#include <MutableVertexPartition.h>

#include "igraph.h"


class ResolutionParameterVertexPartition : public MutableVertexPartition
{
  public:
    ResolutionParameterVertexPartition(Graph* graph,
          vector<size_t> membership, double resolution_parameter);
    ResolutionParameterVertexPartition(Graph* graph,
          vector<size_t> membership);
    ResolutionParameterVertexPartition(Graph* graph, double resolution_parameter);
    ResolutionParameterVertexPartition(Graph* graph);
    virtual ~ResolutionParameterVertexPartition();

    double resolution_parameter;

    virtual double quality()
    {
      return this->quality(this->resolution_parameter);
    };

    virtual double quality(double resolution_parameter)
    {
      Rcpp::Rcerr << "Function not implemented. This should be implented in a derived class, since the base class does not implement a specific method." << "\n";
    };

  private:

};

#endif // RESOLUTIONPARAMETERVERTEXPARTITION_H
