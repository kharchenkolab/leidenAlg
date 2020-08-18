// [[Rcpp::plugins(openmp)]]

#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
bool checkOpenMP() {
#ifdef _OPENMP
	return true;
#else
	return false;
#endif
}
