library(leidenAlg)

test_that("leiden.community() functionality", {
	leidenComm = leiden.community(exampleGraph)
	expect_equal(length(leidenComm), 6)
})


test_that("rleiden.community() functionality", {
	## warning due to how conos created conosGraph, not the package
	rLeidenComm = suppressWarnings(rleiden.community(exampleGraph, n.cores=1))
	expect_equal(length(rLeidenComm), 4)
})


test_that("as.dendrogram.fakeCommunities() functionality", {
	leidenComm = leiden.community(exampleGraph)
	expect_equal(length(as.dendrogram.fakeCommunities(leidenComm)), 0)
	rLeidenComm = suppressWarnings(rleiden.community(exampleGraph, n.cores=1))
	expect_equal(length(as.dendrogram.fakeCommunities(rLeidenComm)), 2)
})


test_that("membership.fakeCommunities() functionality", {
	leidenComm = leiden.community(exampleGraph)
	expect_equal(length(membership.fakeCommunities(leidenComm)), 100)
})

test_that("find_partition() functionality", {
    membership = find_partition_with_rep(exampleGraph, igraph::E(exampleGraph)$weight, nrep = 5)
    expect_equal(length(membership), 100)
})


test_that("find_partition_with_rep() functionality", {
    set.seed(42)
    membership1 = find_partition_with_rep(exampleGraph, igraph::E(exampleGraph)$weight, nrep = 5)
    set.seed(42)
    membership2 = find_partition_with_rep(exampleGraph, igraph::E(exampleGraph)$weight, nrep = 5)
    expect_true(identical(membership1, membership2))
    expect_equal(length(membership1), 100)
})

