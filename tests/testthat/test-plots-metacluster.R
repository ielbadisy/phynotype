test_that("plot_consensus returns a PCA-embedding ggplot for metacluster_fit", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "hclust"),
    k = 2:3,
    seed = 1
  )
  p <- plotconsensus(mfit)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Consensus clusters")
  expect_equal(as.character(p$labels$x), "PC1")
  expect_equal(as.character(p$labels$y), "PC2")
  expect_equal(nrow(p$data), nrow(iris))
})

test_that("plot_coassoc returns a tile heatmap ggplot matching the coassociation matrix", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "hclust"),
    k = 2:3,
    seed = 1
  )
  p <- plotcoassoc(mfit)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomTile"))
  expect_equal(nrow(p$data), nrow(iris)^2)
  expect_equal(sort(unique(p$data$value)), sort(unique(as.vector(mfit$coassoc_matrix))))
})

test_that("plotcoassoc orders by consensus cluster and labels its axes", {
  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"), k = 2:3, seed = 1)
  p <- plotcoassoc(mfit)
  o <- mfit$consensus_fit$order; o <- o[order(mfit$final_clusters[o])]
  expect_equal(p$data$value, as.vector(mfit$coassoc_matrix[o, o]))
  expect_false(is.unsorted(mfit$final_clusters[o]))
  expect_equal(p$labels$x, "Observation")
  expect_equal(p$labels$fill, "Co-association")
})
