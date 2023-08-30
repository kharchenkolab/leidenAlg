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


//' Refer to the R function find_partition()
//' For notes of the graph object, refer to https://igraph.org/c/doc/igraph-Basic.html
//'
//' @param edgelist The graph edge list
//' @param edgelist_length integer The length of the graph edge list
//' @param num_vertices integer The number of vertices in the graph
//' @param direction boolean Whether the graph is directed or undirected
//' @param edge_weights Vector of edge weights. In weighted graphs, a real number is assigned to each (directed or undirected) edge. For an unweighted graph, this is set to 1. Refer to igraph, weighted graphs.
//' @param resolution Integer resoluiton parameter controlling communities detected (default=1.0) Higher resolutions lead to more communities, while lower resolutions lead to fewer communities.
//' @param niter Number of iterations that the algorithm should be run for (default=2)
//' @return A vector of membership values
//' @export
//' @examples
//' library(igraph)
//' library(leidenAlg)
//'
//' g <- make_star(10)
//' E(g)$weight <- seq(ecount(g))
//' find_partition(g, E(g)$weight)
//'
// [[Rcpp::export]]
std::vector<size_t> find_partition_rcpp(std::vector<int>& edgelist, int edgelist_length, int num_vertices, bool direction, std::vector<double>& edge_weights, double resolution=1.0, int niter=2) {

  igraph_t g;
  igraph_vector_t edges;

  // initialize igraph_vector_t
  igraph_vector_init(&edges, edgelist_length);

  // questionable attempt to convert 'std::vector<int>' to 'igraph_vector_t' (not 'igraph_vector_int_t' as documented by 'igraph_create()')
  Stl_To_Igraph_vector_t(edgelist, &edges);

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

  // destroy the igraph_t 'g' and the igraph_vector_t 'edges'
  // https://igraph.org/c/html/latest/igraph-Data-structures.html#igraph_vector_destroy
  // https://igraph.org/c/html/latest/igraph-Generators.html#igraph_create
  igraph_destroy(&g);
  igraph_vector_destroy(&edges);

  return(p.membership());
  //return(igraph_ecount(&g));

}

//' Finds the optimal partition using the Leiden algorithm
//' @details For notes of the graph object, refer to https://igraph.org/c/doc/igraph-Basic.html
//' @param edgelist The graph edge list
//' @param edgelist_length integer The length of the graph edge list
//' @param num_vertices integer The number of vertices in the graph
//' @param direction boolean Whether the graph is directed or undirected
//' @param edge_weights Vector of edge weights. In weighted graphs, a real number is assigned to each (directed or undirected) edge. For an unweighted graph, this is set to 1. Refer to igraph, weighted graphs.
//' @param resolution Integer resoluiton parameter controlling communities detected (default=1.0) Higher resolutions lead to more communities, while lower resolutions lead to fewer communities.
//' @param niter Number of iterations that the algorithm should be run for (default=2)
//' @param nrep Number of replicate starts with random number being updated. (default=10) The result with the best quality will be returned.
//' @export
// [[Rcpp::export]]
std::vector<size_t> find_partition_with_rep_rcpp(std::vector<int>& edgelist, int edgelist_length, int num_vertices, bool direction, std::vector<double>& edge_weights, double resolution=1.0, int niter=2, int nrep=1) {

  igraph_t g;
  igraph_vector_t edges;
  igraph_vector_init(&edges, edgelist_length);
  Stl_To_Igraph_vector_t(edgelist, &edges);
  igraph_create(&g, &edges, num_vertices, direction);

  Graph og(&g, edge_weights);

  double best_quality = -1;
  std::vector<size_t> best_cluster;
  for(int i=0; i<nrep; i++) {
    Rcpp::checkUserInterrupt();
    int seed = R::runif(0,1)*(double)RAND_MAX;
    Optimiser o( (int) (R::runif(0,1)*(double)RAND_MAX) );
    RBConfigurationVertexPartition p(&og,resolution);
    double val=1;
    int iter=0;
    while(val>0 && (iter<niter || niter<0)) {
      val=o.optimise_partition(&p);
      iter++;
    }
    double q = p.quality(resolution);
    if (q > best_quality) {
      best_cluster = p.membership();
      best_quality = q;
    }
  }

  igraph_destroy(&g);
  igraph_vector_destroy(&edges);

  return(best_cluster);
}
