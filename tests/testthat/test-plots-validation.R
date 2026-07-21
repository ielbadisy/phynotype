test_that("plot_silhouette returns a ggplot with one segment per observation", {
  skip_if_not_installed("cluster")
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plot_silhouette(fit)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomSegment"))
  expect_equal(nrow(p$data), nrow(iris))
  expect_equal(p$labels$title, "Silhouette widths")
})

test_that("plot.cluster_fit dispatches to plot_clusters", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plot(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Cluster embedding")
})
