#pragma once
​
#include <Rcpp.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <Rconfig.h>
#include <R_ext/Rdynload.h>
#include <vector>
​
#ifdef HAVE_VISIBILITY_ATTRIBUTE
  # define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
  # define attribute_hidden
#endif
​
#ifdef __cplusplus
extern "C" {
#endif

std::vector<size_t> find_partition_rcpp(std::vector<int> &edgelist, int edgelist_length, int num_vertices, bool direction, std::vector<double> &edge_weights, double resolution, int niter)
{
    static SEXP (*fun)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP) = (SEXP(*)(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP))R_GetCCallable("leidenAlg", "find_partition_rcpp");
    return fun(edgelist, edgelist_length, num_vertices, direction, edge_weights, resolution, niter);
}
​
#ifdef __cplusplus
}
#endif
