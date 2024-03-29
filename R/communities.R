#' @useDynLib leidenAlg
#' @exportPattern "^[[:alpha:]]+"

#' @useDynLib leidenAlg
#' @import Rcpp
#' @import parallel
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom igraph membership
#' @importFrom igraph walktrap.community
#' @importFrom igraph induced.subgraph
#' @importFrom methods is
#' @import sccore
#' @importFrom graphics par
#' @importFrom grDevices adjustcolor
#' @importFrom stats as.dendrogram is.leaf dendrapply as.hclust
#' @import Matrix
NULL


#' Leiden algorithm community detection
#' Detects communities using Leiden algorithm (implementation copied from https://github.com/vtraag/leidenalg)
#'
#' @param graph graph on which communities should be detected
#' @param resolution resolution parameter (default=1.0) - higher numbers lead to more communities
#' @param n.iterations number of iterations that the algorithm should be run for (default=2)
#' @return a fakeCommunities object that returns membership and dendrogram
#' @examples
#' leiden.community(exampleGraph)
#'
#' @export
leiden.community <- function(graph, resolution=1.0, n.iterations=2) {

  ## add check for unweighted graph, i.e. graph$weight is NULL
  if (!igraph::is_weighted(graph)){
    ## simply set the vector of edge weights to 1
    igraph::E(graph)$weight <- 1
  }

  x <- find_partition(graph, igraph::E(graph)$weight, resolution, n.iterations)

  # enclose in a masquerading class
  fv <- as.factor(stats::setNames(x, igraph::V(graph)$name))
  res <- list(membership=fv, dendrogram=NULL, algorithm='leiden', resolution=resolution, n.iter=n.iterations, names=names(fv))
  class(res) <- rev("fakeCommunities")
  return(res)
}

## Here for backwards compatibility

#' Finds the optimal partition using the Leiden algorithm
#'
#' @param graph The igraph graph to define the partition on
#' @param edge_weights Vector of edge weights. In weighted graphs, a real number is assigned to each (directed or undirected) edge. For an unweighted graph, this is set to 1. Refer to igraph, weighted graphs.
#' @param resolution Numeric scalar, resolution parameter controlling communities detected (default=1.0) Higher resolutions lead to more communities, while lower resolutions lead to fewer communities.
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
find_partition <- function(graph, edge_weights, resolution=1.0, niter = 2.0) {
  if (!is(graph, "igraph")) {
    stop("Input 'graph' must be a valid 'igraph' object")
  }
  edgelist <- as.vector(t(igraph::as_edgelist(graph, names=FALSE))) - 1
  edgelist_length <- length(edgelist)
  num_vertices <- length(igraph::V(graph)) - 1
  direction <- igraph::is_weighted(graph)
  find_partition_rcpp(edgelist, edgelist_length, num_vertices, direction, edge_weights, resolution, niter)
}

#' Finds the optimal partition using the Leiden algorithm with replicate starts
#' @description
#' This function performs Leiden algorithm \code{nrep} times and returns the
#' result from the run with the maximum quality.
#'
#' Since Leiden algorithm has stochastic process, repeating stochastically may
#' improve the result. However, users should be aware of whether there is indeed
#' a community structure with exploration, rather than blindly trusting the
#' returned result that comes with the highest quality value.
#'
#' The random number generator (RNG) is not re-seeded at each new start of
#' community detection, in order to keep the independence of each replicate. To
#' get reproducible result, users can run \code{set.seed()} before calling these
#' functions.
#'
#' \code{\link{find_partition}} only performs the community detection once and
#' the reproducibility can also be ensured with \code{set.seed()}.
#' @param graph The igraph graph to define the partition on
#' @param edge_weights Vector of edge weights. In weighted graphs, a real number is assigned to each (directed or undirected) edge. For an unweighted graph, this is set to 1. Refer to igraph, weighted graphs.
#' @param resolution Numeric scalar, resolution parameter controlling communities detected (default=1.0) Higher resolutions lead to more communities, while lower resolutions lead to fewer communities.
#' @param niter Number of iterations that the algorithm should be run for (default=2)
#' @param nrep Number of replicate starts with random number being updated. (default=10) The result with the best quality will be returned.
#' @return A vector of membership values
#' @export
#' @examples
#' library(igraph)
#' 
#' # To run 10 replicates and get the partitioning with the highest quality
#' membership <- find_partition_with_rep(exampleGraph, E(exampleGraph)$weight, nrep = 10)
#' 
#' # To get reprodicible result for every function call, do `set.seed()` right before calling
#' set.seed(233)
#' res1 <- find_partition_with_rep(exampleGraph, E(exampleGraph)$weight, resolution = 2)
#' # Here, no seed was set...
#' res2 <- find_partition_with_rep(exampleGraph, E(exampleGraph)$weight, resolution = 2)
#' set.seed(233)
#' res3 <- find_partition_with_rep(exampleGraph, E(exampleGraph)$weight, resolution = 2)
#' identical(res1, res2) # FALSE (usually), as no seed as set
#' identical(res1, res3) # TRUE (always), as set.seed() was used directly before the function call
#' 
find_partition_with_rep <- function(graph, edge_weights, resolution=1.0, niter = 2.0, nrep = 10) {
    if (!is(graph, "igraph")) {
       stop("Input 'graph' must be a valid 'igraph' object")
    }
    edgelist <- as.vector(t(igraph::as_edgelist(graph, names=FALSE))) - 1
    edgelist_length <- length(edgelist)
    num_vertices <- length(igraph::V(graph)) - 1
    direction <- igraph::is_weighted(graph)
    find_partition_with_rep_rcpp(edgelist, edgelist_length, num_vertices, direction, edge_weights, resolution, niter, nrep)
}


#' Recursive leiden communities
#' Constructs an n-step recursive clustering, using leiden.community
#'
#' @param graph graph
#' @param max.depth Recursive depth (default=2)
#' @param n.cores integer Number of cores to use (default = parallel::detectCores(logical=FALSE)). If logical=FALSE, uses the number of physical CPUs/cores. If logical=TRUE, uses the logical number of CPUS/cores. See parallel::detectCores()
#' @param min.community.size integer Minimal community size parameter for the walktrap communities---Communities smaller than that will be merged (default=10)
#' @param verbose boolean Whether to output progress messages (default=FALSE)
#' @param resolution resolution parameter passed to leiden.community (either a single value, or a value equivalent to max.depth) (default=1)
#' @param cur.depth integer Current depth of clustering (default=1)
#' @param hierarchical boolean If TRUE, calculate hierarchy on the multilevel clusters (default=TRUE)
#' @param ... passed to leiden.community
#' @return a fakeCommunities object that returns membership and dendrogram
#' @examples
#' rleiden.community(exampleGraph, n.cores=1)
#'
#' @export
rleiden.community <- function(graph, max.depth=2, n.cores=parallel::detectCores(logical=FALSE), min.community.size=10, verbose=FALSE, resolution=1, cur.depth=1, hierarchical=TRUE, ...) {

  if(verbose & cur.depth==1) message(paste0("running ",max.depth,"-recursive Leiden clustering: "));
  if(length(resolution)>1) {
    if(length(resolution)!=max.depth) { stop("resolution value must be either a single number or a vector of length max.depth")}
    res <- resolution[cur.depth]
  } else { res <- resolution }
  mt <- leiden.community(graph, resolution=res, ...);

  mem <- membership(mt);
  tx <- table(mem)
  ivn <- names(tx)[tx<min.community.size]
  if(length(ivn)>1) {
    mem[mem %in% ivn] <- as.integer(ivn[1]); # collapse into one group
  }
  if(verbose) message(length(unique(mem)),' ');

  setnames = function(x){names(x) <- x; x}

  if(cur.depth<max.depth) {
    # start recursive run
    wtl <- plapply(setnames(unique(mem)), function(cluster) {
      cn <- names(mem)[which(mem==cluster)]
      sg <- induced.subgraph(graph,cn)
      rleiden.community(induced.subgraph(graph,cn), max.depth=max.depth, resolution=resolution, cur.depth=cur.depth+1, min.community.size=min.community.size, hierarchical=hierarchical, verbose=verbose, n.cores=1, ...)
    },n.cores=n.cores)

    # merge clusters, cleanup
    mbl <- lapply(wtl,membership);
    # combined clustering factor
    fv <- unlist(lapply(setnames(names(wtl)), function(cn){
      paste(cn,as.character(mbl[[cn]]), sep='-')
    }))
    names(fv) <- unlist(lapply(mbl, names))
  } else {
    fv <- mem;
    if(hierarchical) {
      # use walktrap on the last level
      wtl <- plapply(setnames(unique(mem)), function(cluster) {
        cn <- names(mem)[which(mem==cluster)]
        sg <- induced.subgraph(graph,cn)
        res <- walktrap.community(induced.subgraph(graph,cn))
        ## otherwise, need to use igraph:::complete.dend
        complete.dend = utils::getFromNamespace("complete.dend", "igraph")
        res$merges <- complete.dend(res, FALSE)
        res
      }, n.cores=n.cores)
    }
  }

  if(hierarchical) {
    # calculate hierarchy on the multilevel clusters
    if(length(wtl)>1) {
      cgraph <- sccore::getClusterGraph(graph,mem)
      chwt <- walktrap.community(cgraph, steps=8) ## originally "communities"
      d <- as.dendrogram(chwt);

      # merge hierarchical portions
      wtld <- lapply(wtl, as.dendrogram)
      max.height <- max(unlist(lapply(wtld,attr,'height')))

      # shift leaf ids to fill in 1..N range
      mn <- unlist(lapply(wtld,attr,'members'))
      shift.leaf.ids <- function(l,v) { if(is.leaf(l)) { la <- attributes(l); l <- as.integer(l)+v; attributes(l) <- la; }; l  }
      nshift <- cumsum(c(0,mn))[-(length(mn)+1)]; names(nshift) <- names(mn); # how much to shift ids in each tree

      get.heights <- function(l) {
        if(is.leaf(l)) {
          return(attr(l,'height'))
        } else {
          return(c(attr(l,'height'),unlist(lapply(l,get.heights))))
        }
      }
      min.d.height <- min(get.heights(d))
      height.scale <- length(wtld)*2
      height.shift <- 2

      shift.heights <- function(l,s) { attr(l,'height') <- attr(l,'height')+s; l }

      glue.dends <- function(l) {
        if(is.leaf(l)) {
          nam <- as.character(attr(l,'label'));
          id <- dendrapply(wtld[[nam]], shift.leaf.ids, v=nshift[nam])
          return(dendrapply(id,shift.heights,s=max.height-attr(id,'height')))

        }
        attr(l,'height') <- (attr(l,'height')-min.d.height)*height.scale + max.height + height.shift;
        l[[1]] <- glue.dends(l[[1]]); l[[2]] <- glue.dends(l[[2]])
        attr(l,'members') <- attr(l[[1]],'members') + attr(l[[2]],'members')
        return(l)
      }
      combd <- glue.dends(d)
    } else {
      combd <- as.dendrogram(wtl[[1]]);
    }
  } else {
    combd <- NULL;
  }

  if(cur.depth==1) {
    if(verbose) {
      message(paste0(' detected a total of ',length(unique(fv)),' clusters '));
      message("done\n");
    }
  }

  # enclose in a masquerading class
  res <- list(membership=fv, dendrogram=combd, algorithm='rleiden', names=names(fv));
  if(hierarchical & cur.depth==max.depth) {
    # reconstruct merges matrix
    hcm <- as.hclust(as.dendrogram(combd))$merge
    # translate hclust $merge to walktrap-like $merges
    res$merges <- hcm + nrow(hcm) + 1
    res$merges[hcm < 0] <- -hcm[hcm < 0] - 1
  }
  class(res) <- rev("fakeCommunities")
  return(res)
}

#' Returns pre-calculated dendrogram
#'
#' @param object fakeCommunities object
#' @param ... further parameters for generic
#' @return dendrogram
#' @examples
#' rLeidenComm = suppressWarnings(rleiden.community(exampleGraph, n.cores=1))
#' as.dendrogram.fakeCommunities(rLeidenComm)
#'
#' @method as.dendrogram fakeCommunities
#' @export
as.dendrogram.fakeCommunities <- function(object, ...) {
  return(object$dendrogram)
}

#' Returns pre-calculated membership factor
#'
#' @param communities fakeCommunities object
#' @param ... further parameters for generic
#' @return membership factor
#' @examples
#' leidenComm = leiden.community(exampleGraph)
#' membership.fakeCommunities(leidenComm)
#'
#' @method membership fakeCommunities
#' @export
membership.fakeCommunities <- function(communities, ...) {
  return(communities$membership)
}



