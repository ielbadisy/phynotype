test_that("plot_consensus returns a PCA-embedding ggplot for metacluster_fit", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "hclust"),
    k = 2:3,
    seed = 1
  )
  p <- plot_consensus(mfit)

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
  p <- plot_coassoc(mfit)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomTile"))
  expect_equal(nrow(p$data), nrow(iris)^2)
  expect_equal(sort(unique(p$data$value)), sort(unique(as.vector(mfit$coassoc_matrix))))
})
