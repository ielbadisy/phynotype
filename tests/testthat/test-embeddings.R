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

test_that("compute_pca_embedding can encode mixed data frames", {
  mixed <- data.frame(
    x = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "b"))
  )
  clusters <- c(1, 1, 2, 2)
  embedding <- phynotype:::compute_pca_embedding(mixed, clusters)

  expect_s3_class(embedding, "data.frame")
  expect_equal(nrow(embedding), nrow(mixed))
  expect_true(is.numeric(embedding$x))
  expect_true(is.numeric(embedding$y))
})

test_that("compute_embedding resolves distance inputs to MDS", {
  d <- stats::dist(iris[, 1:4])
  clusters <- kmeans(scale(iris[, 1:4]), centers = 3)$cluster
  embedding <- phynotype:::compute_embedding(d, clusters)

  expect_equal(embedding$method, "mds")
  expect_equal(embedding$labels$x, "MDS1")
  expect_equal(embedding$labels$y, "MDS2")
  expect_equal(nrow(embedding$data), attr(d, "Size"))
  expect_named(embedding$data, c("x", "y", "cluster"))
})
