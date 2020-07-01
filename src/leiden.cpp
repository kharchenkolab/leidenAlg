#include <RcppArmadillo.h>
#include <R.h>
#include <Rdefines.h>
#include <vector>
#include "igraph.h"
#include "leidenalg/include/GraphHelper.h"
#include "leidenalg/include/Optimiser.h"
#include "leidenalg/include/RBERVertexPartition.h"
#include "leidenalg/include/RBConfigurationVertexPartition.h"

using namespace std;
using namespace Rcpp;


// a wrapper for the Leidgen algorithm implementation (https://github.com/vtraag/leidenalg)

int R_SEXP_to_vector(SEXP sv, igraph_vector_t *v) {
  v->stor_begin=REAL(sv);
  v->stor_end=v->stor_begin+GET_LENGTH(sv);
  v->end=v->stor_end;
  return 0;
}

int R_SEXP_to_igraph(SEXP graph, igraph_t *res) {

  res->n=(igraph_integer_t) REAL(VECTOR_ELT(graph, 0))[0];
  res->directed=LOGICAL(VECTOR_ELT(graph, 1))[0];
  R_SEXP_to_vector(VECTOR_ELT(graph, 2), &res->from);
  R_SEXP_to_vector(VECTOR_ELT(graph, 3), &res->to);
  R_SEXP_to_vector(VECTOR_ELT(graph, 4), &res->oi);
  R_SEXP_to_vector(VECTOR_ELT(graph, 5), &res->ii);
  R_SEXP_to_vector(VECTOR_ELT(graph, 6), &res->os);
  R_SEXP_to_vector(VECTOR_ELT(graph, 7), &res->is);

  /* attributes */
  REAL(VECTOR_ELT(VECTOR_ELT(graph, 8), 0))[0] = 1; /* R objects refcount */
  REAL(VECTOR_ELT(VECTOR_ELT(graph, 8), 0))[1] = 0; /* igraph_t objects */
  res->attr=VECTOR_ELT(graph, 8);

  return 0;
}

//' find the optimal graph partition using the Leiden algorithm, Traag et al 2019 https://doi.org/10.1038/s41598-019-41695-z. Refer to find_partition() in the python interface, https://github.com/vtraag/leidenalg
//' 
//' @param graph The input graph for which to detect communities.
//' @param edge_weights Weights of edges. Can be either an iterable or an edge attribute.
//' @param resolution numeric
//' @param niter integer Number of iterations to run the Leiden algorithm. By default, 2 iterations are run. If the number of iterations is negative, the Leiden algorithm is run until an iteration in which there was no improvement.
//' @export
// [[Rcpp::export]]
std::vector<size_t> leiden_community(SEXP graph, std::vector<double>& edge_weights, double resolution=1.0, int niter=2) {
  igraph_t g;
  
  R_SEXP_to_igraph(graph, &g);
  Graph og(&g,edge_weights);
  Optimiser o( (int) (R::runif(0,1)*(double)RAND_MAX) );
  RBConfigurationVertexPartition p(&og,resolution);
    //RBERVertexPartition p(&og,resolution);
  //o.find_partition(og,resolution);
  double val=1;
  int iter=0;
  while(val>0 && (iter<niter || niter<0)) {
    val=o.optimise_partition(&p);
    iter++;
  }
  return(p.membership());
  //return(igraph_ecount(&g));

}
  
