test_that("compute_pca_embedding returns a two-component data frame with cluster factor", {
  data <- as.matrix(iris[, 1:4])
  clusters <- kmeans(scale(data), centers = 3)$cluster
  embedding <- phynotype:::compute_pca_embedding(data, clusters)$coords

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
  embedding <- phynotype:::compute_pca_embedding(data, clusters)$coords

  pc <- stats::prcomp(data, center = TRUE, scale. = TRUE)
  expect_equal(embedding$x, unname(pc$x[, 1]))
  expect_equal(embedding$y, unname(pc$x[, 2]))
})

test_that("compute_pca_embedding also returns variable loadings", {
  data <- as.matrix(iris[, 1:4])
  clusters <- rep(1:2, length.out = nrow(data))
  embedding <- phynotype:::compute_pca_embedding(data, clusters)

  expect_named(embedding$loadings, c("variable", "x", "y"))
  expect_equal(nrow(embedding$loadings), ncol(data))
  expect_equal(embedding$loadings$variable, colnames(data))
})

test_that("compute_embedding resolves mixed data to FAMD", {
  skip_if_not_installed("FactoMineR")
  mixed <- data.frame(
    x = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "b"))
  )
  clusters <- c(1, 1, 2, 2)
  embedding <- phynotype:::compute_embedding(mixed, clusters)

  expect_equal(embedding$method, "famd")
  expect_equal(embedding$labels$x, "Dim 1")
  expect_equal(embedding$labels$y, "Dim 2")
  expect_equal(nrow(embedding$data), nrow(mixed))
  expect_named(embedding$data, c("x", "y", "cluster"))
})

test_that("compute_embedding resolves categorical data to MCA", {
  skip_if_not_installed("FactoMineR")
  cat_df <- data.frame(
    a = factor(c("x", "x", "y", "y")),
    b = factor(c("u", "v", "u", "v"))
  )
  clusters <- c(1, 1, 2, 2)
  embedding <- phynotype:::compute_embedding(cat_df, clusters)

  expect_equal(embedding$method, "mca")
  expect_equal(embedding$labels$x, "Dim 1")
  expect_equal(embedding$labels$y, "Dim 2")
  expect_equal(nrow(embedding$data), nrow(cat_df))
  expect_named(embedding$data, c("x", "y", "cluster"))
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
