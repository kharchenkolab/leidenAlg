library(leidenAlg)

test_that("leiden.community() functionality", {
	leidenComm = leiden.community(conosGraph)
	expect_equal(length(leidenComm), 6)
})


test_that("rleiden.community() functionality", {
	##rleiden.community(conosGraph, n.cores=1)
	## expect_equal(length(leidenComm), 6)
})

