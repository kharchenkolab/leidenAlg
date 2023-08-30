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

// https://stackoverflow.com/questions/10250438/using-stdvector-with-igraph

void Stl_To_Igraph_vector_t(std::vector<int>& vectR, igraph_vector_t* v) {
    size_t n = vectR.size();

    /* Make sure that there is enough space for the items in v */
    igraph_vector_resize(v, n);

    /* Copy all the items */
    for (size_t i = 0; i < n; i++) {
        VECTOR(*v)[i] = vectR[i];
    }
}

//' @details For notes of the graph object, refer to https://igraph.org/c/doc/igraph-Basic.html
//'
//' @param edgelist The graph edge list
//' @param edgelist_length integer The length of the graph edge list
//' @param num_vertices integer The number of vertices in the graph
//' @param direction boolean Whether the graph is directed or undirected
//' @rdname find_partition
//' @export
// [[Rcpp::export]]
std::vector<size_t> find_partition_rcpp(std::vector<int>& edgelist, int edgelist_length, int num_vertices, bool direction, std::vector<double>& edge_weights, double resolution=1.0, int niter=2, int nrep=1, int seed=1) {

  igraph_t g;
  igraph_vector_t edges;

  // initialize igraph_vector_t
  igraph_vector_init(&edges, edgelist_length);

  // questionable attempt to convert 'std::vector<int>' to 'igraph_vector_t' (not 'igraph_vector_int_t' as documented by 'igraph_create()')
  Stl_To_Igraph_vector_t(edgelist, &edges);

  igraph_create(&g, &edges, num_vertices, direction);

  Graph og(&g, edge_weights);

  // RBConfigurationVertexPartition p(&og,resolution);
    //RBERVertexPartition p(&og,resolution);
  //o.find_partition(og,resolution);
  double best_quality = -1;
  std::vector<size_t> best_cluster;
  seed--;
  for(int i=0; i<nrep; i++) {
    Rcpp::checkUserInterrupt();
    seed++;
    Optimiser o(seed);
    RBConfigurationVertexPartition p(&og,resolution);
    double val=1;
    int iter=0;
    while(val>0 && (iter<niter || niter<0)) {
      val=o.optimise_partition(&p);
      iter++;
    }
    if (p.quality(resolution) > best_quality) {
      best_cluster = p.membership();
      best_quality = p.quality(resolution);
    }
  }

  // destroy the igraph_t 'g' and the igraph_vector_t 'edges'
  // https://igraph.org/c/html/latest/igraph-Data-structures.html#igraph_vector_destroy
  // https://igraph.org/c/html/latest/igraph-Generators.html#igraph_create
  igraph_destroy(&g);
  igraph_vector_destroy(&edges);

  return(best_cluster);
  //return(igraph_ecount(&g));

}
