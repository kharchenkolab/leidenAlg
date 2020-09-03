#' @useDynLib leidenAlg
#' @import Rcpp
#' @import igraph
#' @import parallel
#' @importFrom igraph decompose
#' @importFrom igraph spectrum
#' @importFrom graphics par
#' @importFrom grDevices adjustcolor
#' @importFrom stats as.dendrogram is.leaf
NULL


#' setNames wrapper function, also called 'sn' elsewhere
#'
#' @description Set names equal to values
#' @param x an object for which names attribute will be meaningful 
#' @return An object with names assigned equal to values
#' @examples
#' vec = c(1, 2, 3, 4)
#' set.names(vec)
#' 
#' @export
#' @keywords internal
set.names <- function(x) { stats::setNames(x, x) }


#' Parallel lapply: Use mclapply if n.cores>1, otherwise use regular lapply() if n.cores=1
#'
#' @description Parallel, optionally verbose lapply. See ?parallel::mclapply for more info.
#' @param ... Arguments fed to parallel::mclapply(...), pbapply::pblapply(...), or lapply(...)
#' @param progress Show progress bar via pbapply (default=FALSE)
#' @param n.cores Number of cores to use (default=1)
#' @param mc.preschedule See ?parllel::mclapply (default=FALSE) If TRUE then the computation is first divided to (at most) as many jobs are there are cores and then the jobs are started, each job possibly covering more than one value. If FALSE, then one job is forked for each value of X. The former is better for short computations or large number of values in X, the latter is better for jobs that have high variance of completion time and not too many values of X compared to mc.cores.
#' @examples
#' square = function(x){ x**2 }
#' papply(1:10, square, n.cores=1, progress=TRUE)
#'
#' @return list, as returned by lapply
#' @export
papply <- function(..., progress=FALSE, n.cores=parallel::detectCores(), mc.preschedule=FALSE) {
  if (progress && requireNamespace("pbapply", quietly=TRUE)) {
    result <- pbapply::pblapply(..., cl=n.cores)
  } else if(n.cores>1) {
    result <- parallel::mclapply(..., mc.cores=n.cores, mc.preschedule=mc.preschedule)
  } else {
    # fall back on lapply
    result <- lapply(...)
  }

  is.error <- (sapply(result, class) == "try-error")
  if (any(is.error)) {
    stop(paste("Errors in papply:", result[is.error]))
  }

  return(result)
}

#' Leiden algorithm community detection
#'
#' Detect communities using Leiden algorithm (implementation copied from https://github.com/vtraag/leidenalg)
#' @param graph graph on which communities should be detected
#' @param resolution resolution parameter (default=1.0) - higher numbers lead to more communities
#' @param n.iterations number of iterations that the algorithm should be run for (default=2)
#' @return a fakeCommunities object that returns membership and dendrogram
#' @export 
leiden.community <- function(graph, resolution=1.0, n.iterations=2) {

  x <- find_partition(graph, igraph::E(graph)$weight, resolution, n.iterations)

  # enclose in a masquerading class
  fv <- as.factor(stats::setNames(x, V(graph)$name))
  res <- list(membership=fv, dendrogram=NULL, algorithm='leiden', resolution=resolution, n.iter=n.iterations, names=names(fv))
  class(res) <- rev("fakeCommunities")
  return(res)
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
#' @param mc.preschedule boolean (default=FALSE) Parameter fed to mclapply(). If TRUE, then the computation is first divided to (at most) as many jobs are there are cores and then the jobs are started, each job possibly covering more than one value. If FALSE, then one job is forked for each value of X in mclapply(X, FUN, ...)
#' @param ... passed to leiden.community
#' @return a fakeCommunities object that returns membership and dendrogram
#' @export
rleiden.community <- function(graph, max.depth=2, n.cores=parallel::detectCores(logical=FALSE), min.community.size=10, verbose=FALSE, resolution=1, cur.depth=1, hierarchical=TRUE, mc.preschedule=FALSE, ...) {

  if(verbose & cur.depth==1){
    cat(paste0("running ",max.depth,"-recursive Leiden clustering: "))
  }

  if(length(resolution)>1) {
    if(length(resolution)!=max.depth) { stop("resolution value must be either a single number or a vector of length max.depth")}
    res <- resolution[cur.depth]
  } else { 
    res <- resolution 
  }
  mt <- leiden.community(graph, resolution=res, ...)

  mem <- membership(mt)
  tx <- table(mem)
  ivn <- names(tx)[tx<min.community.size]
  if(length(ivn)>1) {
    mem[mem %in% ivn] <- as.integer(ivn[1]) ## collapse into one group
  }

  if(verbose){
    cat(length(unique(mem)),' ')
  }

  ## internal function in igraph, community.R
  complete.dend <- function(comm, use.modularity) {
    merges <- comm$merges
    if (nrow(merges) < comm$vcount-1) {
      if (use.modularity) {
        stop(paste("`use.modularity' requires a full dendrogram,",
                   "i.e. a connected graph"))
      }
      miss <- seq_len(comm$vcount + nrow(merges))[-as.vector(merges)]
      miss <- c(miss, seq_len(length(miss)-2) + comm$vcount+nrow(merges))
      miss <- matrix(miss, byrow=TRUE, ncol=2)
      merges <- rbind(merges, miss)
    }
    storage.mode(merges) <- "integer"

    return(merges)
  }

  if(cur.depth<max.depth) {
    # start recursive run
    wtl <- papply(set.names(unique(mem)), function(cluster) {
      cn <- names(mem)[which(mem==cluster)]
      sg <- induced.subgraph(graph,cn)
      rleiden.community(induced.subgraph(graph,cn), max.depth=max.depth, resolution=resolution, cur.depth=cur.depth+1, min.community.size=min.community.size, hierarchical=hierarchical, verbose=verbose, n.cores=1, ...)
    }, n.cores=n.cores)

    # merge clusters, cleanup
    mbl <- lapply(wtl,membership)
    # combined clustering factor
    fv <- unlist(lapply(set.names(names(wtl)),function(cn) {
      paste(cn,as.character(mbl[[cn]]),sep='-')
    }))
    names(fv) <- unlist(lapply(mbl,names))
  } else {
    fv <- mem
    if(hierarchical) {
      # use walktrap on the last level
      wtl <- papply(set.names(unique(mem)), function(cluster) {
        cn <- names(mem)[which(mem==cluster)]
        sg <- induced.subgraph(graph,cn)
        res <- walktrap.community(induced.subgraph(graph,cn))
        res$merges <- complete.dend(res,FALSE)
        res
      },n.cores=n.cores)
    }
  }

  if(hierarchical) {
    # calculate hierarchy on the multilevel clusters
    if(length(wtl)>1) {
      cgraph <- getClusterGraph(graph,mem)
      chwt <- walktrap.community(cgraph,steps=8)
      d <- stats::as.dendrogram(chwt)

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
          return(c(attr(l,'height'),unlist(lapply(l, get.heights))))
        }
      }
      min.d.height <- min(get.heights(d))
      height.scale <- length(wtld)*2
      height.shift <- 2

      shift.heights <- function(l,s) { attr(l,'height') <- attr(l,'height')+s; l }

      glue.dends <- function(l) {
        if(is.leaf(l)) {
          nam <- as.character(attr(l,'label'));
          id <- stats::dendrapply(wtld[[nam]], shift.leaf.ids, v=nshift[nam])
          return(stats::dendrapply(id,shift.heights,s=max.height-attr(id,'height')))

        }
        attr(l,'height') <- (attr(l,'height')-min.d.height)*height.scale + max.height + height.shift;
        l[[1]] <- glue.dends(l[[1]]); l[[2]] <- glue.dends(l[[2]])
        attr(l,'members') <- attr(l[[1]],'members') + attr(l[[2]],'members')
        return(l)
      }
      combd <- glue.dends(d)
    } else {
      combd <- as.dendrogram(wtl[[1]])
    }
  } else {
    combd <- NULL
  }

  if(cur.depth==1) {
    if(verbose) {
      cat(paste0('Detected a total of ',length(unique(fv)),' clusters '))
      cat("Done\n")
    }
  }

  # enclose in a masquerading class
  res <- list(membership=fv, dendrogram=combd, algorithm='rleiden', names=names(fv))
  if(hierarchical & cur.depth==max.depth) {
    # reconstruct merges matrix
    hcm <- stats::as.hclust(as.dendrogram(combd))$merge
    # translate hclust $merge to walktrap-like $merges
    res$merges <- hcm + nrow(hcm) + 1
    res$merges[hcm < 0] <- -hcm[hcm < 0] - 1
  }
  class(res) <- rev("fakeCommunities")
  return(res)
}


#' Returns pre-calculated dendrogram
#'
#' @param obj fakeCommunities object
#' @param ... dropped
#' @return dendrogram
#' @export
dendrogram.fakeCommunities <- function(obj, ...) {
  return(obj$dendrogram)
}


#' Returns pre-calculated membership factor
#'
#' @param obj fakeCommunities object
#' @return membership factor
#' @export
membership.fakeCommunities <- function(obj) {
  return(obj$membership)
}

#' @description Collapse vertices belonging to each cluster in a graph
#'
#' @param graph igraph object Graph to be clustered
#' @param groups Factor on vertices describing cluster assignment (can specify integer vertex ids, or character vertex names which will be matched)
#' @param method string Method used, either "sum" or "paga" (default="sum"). "paga" refers to PAGA, <https://github.com/theislab/paga>
#' @param node.scale numeric Value to scale the size of the nodes, via 'vertex.size' in igraph::plot() (default=50)
#' @param edge.scale numeric Value to scale the edge thickness of the noes, via 'edge.width' in graph::plot()  (default=50)
#' @param edge.alpha numeric Value to scale the opacity of 'alpha.f' in adjustcolor(); typically in [0,1] (default=0.3)
#' @param plot boolean Whether to show collapsed graph plot (default=FALSE)
#' @inheritParams collapseGraphPaga
#' @return collapsed graph
#' @export
getClusterGraph <- function(graph, groups, method="sum", plot=FALSE, node.scale=50, edge.scale=50, edge.alpha=0.3, ...) {
  V(graph)$num <- 1;
  if(is.integer(groups) && is.null(names(groups))) {
    nv <- vcount(graph)
    if(length(groups)!=nv) stop('length of groups should be equal to the number of vertices')
    if(max(groups)>nv) stop('groups specifies ids that are larger than the number of vertices in the graph')
    if(any(is.na(groups))) {
      # remove vertices that are not part of the groups
      vi <- which(!is.na(groups));
      g <- induced.subgraph(graph,vi);
      groups <- groups[vi];
    } else {
      g <- graph;
    }
  } else {
    gn <- V(graph)$name;
    groups <- stats::na.omit(groups[names(groups) %in% gn]);
    if(length(groups)<2) stop('valid names of groups elements include too few cells')
    if(length(groups)<length(gn)) {
      g <- induced.subgraph(graph,names(groups))
    } else {
      g <- graph;
    }
    if(is.factor(groups)) {
      groups <- groups[V(g)$name]
    } else {
      groups <- as.factor(stats::setNames(as.character(groups[V(g)$name]),V(g)$name))
    }
  }

  if (method == "sum") {
    gcon <- collapseGraphSum(g, groups, ...)
  } else if (method == "paga") {
    gcon <- collapseGraphPaga(g, groups, ...)
  } else {
    stop("Unknown method: ", method)
  }

  if(plot) {
    set.seed(1)
    par(mar = rep(0.1, 4))
    plot.igraph(gcon, layout=layout_with_fr(gcon), vertex.size=V(gcon)$num/(sum(V(gcon)$num)/node.scale), 
      edge.width=E(gcon)$weight/sum(E(gcon)$weight/edge.scale), adjustcolor('black', alpha.f=edge.alpha))
  }
  return(invisible(gcon))
}