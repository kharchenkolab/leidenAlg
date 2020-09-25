library(leidenAlg)

test_that("leiden.community() functionality", {
	leidenComm = leiden.community(conosGraph)
	expect_equal(length(leidenComm), 6)
})


test_that("rleiden.community() functionality", {
	## warning due to how conos created conosGraph, not the package
	rLeidenComm = suppressWarnings(rleiden.community(conosGraph, n.cores=1))
	expect_equal(length(rLeidenComm), 4)
})


test_that("dendrogram.fakeCommunities() functionality", {
	leidenComm = leiden.community(conosGraph)
	expect_equal(length(dendrogram.fakeCommunities(leidenComm)), 0)
	rLeidenComm = suppressWarnings(rleiden.community(conosGraph, n.cores=1))
	expect_equal(length(dendrogram.fakeCommunities(rLeidenComm)), 2)
})


test_that("membership.fakeCommunities() functionality", {
	leidenComm = leiden.community(conosGraph)
	expect_equal(length(membership.fakeCommunities(leidenComm)), 100)
})

