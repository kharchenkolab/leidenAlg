## 25 September 2020

library(conos)
library(dplyr)

panel <- readRDS(file.path(find.package('conos'),'extdata','panel.rds'))

library(pagoda2)
panel.preprocessed <- lapply(panel, basicP2proc, n.cores=4, min.cells.per.gene=0, n.odgenes=2e3, get.largevis=FALSE, make.geneknn=FALSE)

con <- Conos$new(panel.preprocessed, n.cores=4)

con$buildGraph(k=30, k.self=5, space='PCA', ncomps=30, n.odgenes=2000, matching.method='mNN', metric='angular', score.component.variance=TRUE, verbose=TRUE)

## conosGraph.rda
exampleGraph = con$graph
exampleGraph = igraph::induced_subgraph(conosGraph, v=V(conosGraph)[1:100]) ## length 100
save(exampleGraph, file="conosGraph.rda", compress="xz")


