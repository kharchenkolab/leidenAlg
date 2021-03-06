# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Finds the optimal partition using the Leiden algorithm
#' 
#' @param graph The igraph graph to define the partition on
#' @param edge_weights Vector of edge weights. In weighted graphs, a real number is assigned to each (directed or undirected) edge. Refer to igraph, weighted graphs.
#' @param resolution Integer resoluiton parameter controlling communities detected (default=1.0) Higher resolutions lead to more communities, while lower resolutions lead to fewer communities.
#' @param niter Number of iterations that the algorithm should be run for (default=2)
#' @return A vector of membership values
#' @examples 
#' library(igraph)
#' library(leidenAlg)
#'
#' g <- make_star(10)
#' E(g)$weight <- seq(ecount(g))
#' find_partition(g, E(g)$weight)
#' 
#' @export
find_partition <- function(graph, edge_weights, resolution = 1.0, niter = 2L) {
    .Call('_leidenAlg_find_partition', PACKAGE = 'leidenAlg', graph, edge_weights, resolution, niter)
}

