#include <RcppArmadillo.h>
#include <R.h>
#include <Rdefines.h>
#include <vector>
#include "igraph.h"
#include "rigraph/include/igraph.h"
#include "rigraph/include/igraph_constructors.h"
#include "leidenalg/include/GraphHelper.h"
#include "leidenalg/include/Optimiser.h"
#include "leidenalg/include/RBERVertexPartition.h"
#include "leidenalg/include/RBConfigurationVertexPartition.h"
#include "rigraph/include/igraph_constructors.h"

using namespace std;
using namespace Rcpp;


// Debug Mode implies checking assertions.
#if defined(_GLIBCXX_ASSERTIONS)
# define _GLIBCXX_ASSERTIONS 0
#endif

// a wrapper for the Leidgen algorithm implementation (https://github.com/vtraag/leidenalg)

// REMOVED v1.1.0 with igraph (R package) v1.5.0
//
// int R_SEXP_to_vector(SEXP sv, igraph_vector_t *v) {
//   v->stor_begin=REAL(sv);
//   v->stor_end=v->stor_begin+GET_LENGTH(sv);
//   v->end=v->stor_end;
//   return 0;
// }


// int R_SEXP_to_igraph(SEXP graph, igraph_t *res) {
// 
//   res->n=(igraph_integer_t) REAL(VECTOR_ELT(graph, 0))[0];
//   res->directed=LOGICAL(VECTOR_ELT(graph, 1))[0];
//   R_SEXP_to_vector(VECTOR_ELT(graph, 2), &res->from);
//   R_SEXP_to_vector(VECTOR_ELT(graph, 3), &res->to);
//   R_SEXP_to_vector(VECTOR_ELT(graph, 4), &res->oi);
//   R_SEXP_to_vector(VECTOR_ELT(graph, 5), &res->ii);
//   R_SEXP_to_vector(VECTOR_ELT(graph, 6), &res->os);
//   R_SEXP_to_vector(VECTOR_ELT(graph, 7), &res->is);
//   
//   /* attributes */
//   REAL(VECTOR_ELT(VECTOR_ELT(graph, 8), 0))[0] = 1; /* R objects refcount */
//   REAL(VECTOR_ELT(VECTOR_ELT(graph, 8), 0))[1] = 0; /* igraph_t objects */
//   res->attr=VECTOR_ELT(graph, 8);
//   
//   return 0;
// }
//   


// https://stackoverflow.com/questions/10250438/using-stdvector-with-igraph

void Stl_To_Igraph_vector_t(std::vector<double>& vectR, igraph_vector_t* v) {
    size_t n = vectR.size();

    /* Make sure that there is enough space for the items in v */
    igraph_vector_resize(v, n);

    /* Copy all the items */
    for (size_t i = 0; i < n; i++) {
        VECTOR(*v)[i] = vectR[i];
    }
}


// Refer to find_partition()
// 
// [[Rcpp::export]]
std::vector<size_t> find_partition_rcpp(std::vector<double>& edge_weights, int num_vertices, bool direction, double resolution=1.0, int niter=2) {
  
  igraph_t g;
  igraph_vector_t edges;

  Stl_To_Igraph_vector_t(edge_weights, &edges);
  igraph_create(&g, &edges, num_vertices, direction);

  Graph og(&g, edge_weights);

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
  