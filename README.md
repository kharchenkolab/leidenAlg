# leidenAlg

Implements the Leiden algorithm via an R interface, from conos

The Leiden algorithm is a iterative community detection algorithm on networks---the algorithm is designed to converge to a partition in which all subsets of all communities are locally optimally assigned, yielding communities guaranteed to be connected.

C++ version with python interface here: https://github.com/vtraag/leidenalg

Leiden algorithm paper: (Traag et al 2019)[https://www.nature.com/articles/s41598-019-41695-z]
* Written to improve upon defects of the Louvain algorithm. 
* Faster, scales well, and can be run on graphs of millions of nodes (as long as they can fit in memory).

The Leiden algorithm is a iterative community detection algorithm on networks---the algorithm is designed to converge to a partition in which all subsets of all communities are locally optimally assigned, yielding communities guaranteed to be connected. The basic steps are:
(1) local moving of nodes to quickly find partitions, (2) refinement of partitions, (3) aggregation of the network based on the refned partition, using the non-refned partition to create an initial partition for the aggregate network. Steps are iterated until convergence.

Python interface [here](https://github.com/vtraag/leidenalg/blob/master/src/functions.py)
* the main function`find_partition` (detect communities, i.e. optimal partition) 
* `find_partition_multiplex` (detect communities for multiplex graph)
* `find_partition_temporal` (detect communities for temporal graphs)



## Functions

* `leiden.community()`: Detect communities using Leiden algorithm, output as `fakeCommunities` class for downstream use.

* `rleiden.community()`: Recursive leiden communities, constructs an n-step recursive clustering, using leiden.community.detection. Returns a f`akeCommunities` object that has methods membership(), without dendogram.

* `dendrogram.fakeCommunities()`: Returns pre-calculated dendrogram

* `membership.fakeCommunities()`: Returns pre-calculated membership factor


## Installation

For Mac OS X 10.15.5, 

`brew install gcc gfortran`

I've included `llvm` as well for my configuration below, though it's not required.

including the following in my `~/.zshrc`:

```
export PATH="/usr/local/opt/llvm/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/llvm/lib"
export CPPFLAGS="-I/usr/local/opt/llvm/include"
```

and my `.R/Makevars` contains:

```
XCBASE:=$(shell xcrun --show-sdk-path)
LLVMBASE:=$(shell brew --prefix llvm)
GCCBASE:=$(shell brew --prefix gcc)
GETTEXT:=$(shell brew --prefix gettext)

CC=$(LLVMBASE)/bin/clang -fopenmp
CXX=$(LLVMBASE)/bin/clang++ -fopenmp
CXX11=$(LLVMBASE)/bin/clang++ -fopenmp
CXX14=$(LLVMBASE)/bin/clang++ -fopenmp
CXX17=$(LLVMBASE)/bin/clang++ -fopenmp
CXX1X=$(LLVMBASE)/bin/clang++ -fopenmp

CPPFLAGS=-isystem "$(LLVMBASE)/include" -isysroot "$(XCBASE)"
LDFLAGS=-L"$(LLVMBASE)/lib" -L"$(GETTEXT)/lib" --sysroot="$(XCBASE)"

FC=$(GCCBASE)/bin/gfortran -fopenmp
F77=$(GCCBASE)/bin/gfortran -fopenmp
FLIBS=-L$(GCCBASE)/lib/gcc/9/ -lm
```
