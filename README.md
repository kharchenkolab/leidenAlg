[![<kharchenkolab>](https://circleci.com/gh/kharchenkolab/leidenAlg.svg?style=svg)](https://app.circleci.com/pipelines/github/kharchenkolab/leidenAlg)
[![CRAN status](https://www.r-pkg.org/badges/version/leidenAlg)](https://cran.r-project.org/package=leidenAlg)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/leidenAlg)](https://cran.r-project.org/package=leidenAlg)

# leidenAlg

Implements the Leiden algorithm via an R interface


#### Note: cluster_leiden() now in igraph

Since [October 2020](https://github.com/igraph/rigraph/pull/399), the R package [igraph](https://cran.r-project.org/package=igraph) contains the function `cluster_leiden()` implemented by Vincent Traag ([@vtraag](https://github.com/vtraag)). The usage of this function is detailed in the igraph documentation [here](https://igraph.org/r/html/1.2.7/cluster_leiden.html). We recommend users use this function. 

There is still no R package which entirely encompasses all of the functionality of the original Python/C++ implementation here from https://github.com/vtraag/leidenalg. We hope interested developers could use this package as a starting point for creating an R package which mirrors the full available functionality of the Python package. See [here](https://github.com/vtraag/leidenalg#usage) for details. 

## Summary

The Leiden algorithm is an iterative community detection algorithm on networks---the algorithm is designed to converge to a partition in which all subsets of all communities are locally optimally assigned, yielding communities guaranteed to be connected.

The algorithm was written to improve upon defects of the Louvain algorithm. Consequently, the Leiden algorithm is faster, scales well, and can be run on graphs of millions of nodes (as long as they can fit in memory).

The basic steps are:
* (1) local moving of nodes to quickly find partitions
* (2) refinement of partitions
* (3) aggregation of the network based on the refined partition, using the non-refined partition to create an initial partition for the aggregate network. Steps are iterated until convergence.

For details on the algorithm, see ["From Louvain to Leiden: guaranteeing well-connected communities"](https://www.nature.com/articles/s41598-019-41695-z) Traag, Waltman, van Eck. Sci Rep 9, 5233 (2019). https://doi.org/10.1038/s41598-019-41695-z

For the original implementation in C++ with python bindings, see: https://github.com/vtraag/leidenalg

## Installation

To install the stable version from [CRAN](https://CRAN.R-project.org/package=leidenAlg), use:

```r
install.packages('leidenAlg')
```

To install the latest version, use:

```r
install.packages('devtools')
devtools::install_github('kharchenkolab/leidenAlg', build_vignettes = TRUE)
```

Note that this package depends on [igraph](https://CRAN.R-project.org/package=igraph), which requires various libraries to install correctly e.g. `libxml2`. Please see the installation instructions at that page for more details, along with the README [here](https://github.com/igraph/rigraph).

Debian-based users of Linux can install the required packages via:

```
sudo apt-get update
sudo apt-get install libxml2-dev libgmp-dev libglpk-dev
```

For users of Red Hat distributions, use the following command to install the required packages:

```
sudo yum update
sudo yum install libxml2-devel gmp-devel glpk-devel
```

For Mac OS, the commands with the [Homebrew package manager](https://brew.sh/) are as follows:

```
brew update
brew install libxml2 glpk gmp
```
 
**Note:** For Mac OS users, there is a guide for troubleshooting [here](https://github.com/kharchenkolab/leidenAlg/wiki/Installing-leidenAlg-for-Mac-OS) if issues arise. 

## Functions

* `find_partition()`: Finds the optimal partition using the Leiden algorithm.

* `find_partition_with_rep()`: Finds the optimal partition using the Leiden algorithm with replicate starts

* `leiden.community()`: Detects communities using Leiden algorithm, output as `fakeCommunities` class for downstream use.

* `rleiden.community()`: Recursive leiden communities, constructs an n-step recursive clustering, using leiden.community.detection. Returns a `fakeCommunities` object that has methods membership(), without dendrogram.

* `as.dendrogram()`: Returns pre-calculated dendrogram from `"fakeCommunities"` object

* `membership()`: Returns pre-calculated membership factor from `"fakeCommunities"` object


## Citation
If you find `leidenAlg` useful for your publication, please cite:

```
Peter Kharchenko, Viktor Petukhov, Yichen Wang, and Evan Biederstedt (2023).
leidenAlg: Implements the Leiden Algorithm via an R Interface. R
package version 1.1.4. https://github.com/kharchenkolab/leidenAlg
```
