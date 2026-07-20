test_that("compute_pca_embedding returns a two-component data frame with cluster factor", {
  data <- as.matrix(iris[, 1:4])
  clusters <- kmeans(scale(data), centers = 3)$cluster
  embedding <- phynotype:::compute_pca_embedding(data, clusters)

  expect_s3_class(embedding, "data.frame")
  expect_equal(nrow(embedding), nrow(data))
  expect_named(embedding, c("x", "y", "cluster"))
  expect_true(is.numeric(embedding$x))
  expect_true(is.numeric(embedding$y))
  expect_s3_class(embedding$cluster, "factor")
  expect_equal(as.integer(as.character(embedding$cluster)), clusters)
})

test_that("compute_pca_embedding matches prcomp on the first two components", {
  data <- as.matrix(iris[, 1:4])
  clusters <- rep(1:2, length.out = nrow(data))
  embedding <- phynotype:::compute_pca_embedding(data, clusters)

  pc <- stats::prcomp(data, center = TRUE, scale. = TRUE)
  expect_equal(embedding$x, unname(pc$x[, 1]))
  expect_equal(embedding$y, unname(pc$x[, 2]))
})
