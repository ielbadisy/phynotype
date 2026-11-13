test_that("plot_clusters dispatches for cluster_fit, cluster_explore, and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p1 <- plotclusters(fit)
  expect_s3_class(p1, "ggplot")
  expect_equal(as.character(p1$labels$x), "PC1")
  expect_equal(as.character(p1$labels$y), "PC2")

  exp <- explore(fit)
  p2 <- plotclusters(exp)
  expect_s3_class(p2, "ggplot")

  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  p3 <- plotclusters(mfit)
  expect_s3_class(p3, "ggplot")
  expect_equal(p3$labels$title, "Consensus clusters")
})

test_that("plot_clusters uses MDS labels for distance-based fits", {
  d <- stats::dist(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  p <- plotclusters(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$labels$x), "MDS1")
  expect_equal(as.character(p$labels$y), "MDS2")
})

test_that("plot_clusters uses FAMD labels for mixed-data fits", {
  skip_if_not_installed("FactoMineR")
  mixed <- data.frame(
    x = c(1, 2, 8, 9, 1.5, 8.5),
    group = factor(c("a", "a", "b", "b", "a", "b"))
  )
  fit <- cluster(mixed, method = "kproto", k = 2, seed = 1, nstart = 2)
  p <- plotclusters(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$labels$x), "Dim 1")
  expect_equal(as.character(p$labels$y), "Dim 2")
})

test_that("plot_clusters uses MCA labels for categorical-only fits", {
  skip_if_not_installed("FactoMineR")
  cat_df <- data.frame(
    a = factor(c("x", "x", "y", "y")),
    b = factor(c("u", "v", "u", "v"))
  )
  fit <- cluster(cat_df, method = "kmm", k = 2, seed = 1)
  p <- plotclusters(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$labels$x), "Dim 1")
  expect_equal(as.character(p$labels$y), "Dim 2")
})

test_that("plot_biplot renders natively for PCA embeddings", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plotbiplot(fit)

  expect_s3_class(p, "ggplot")
  expect_match(as.character(p$labels$x), "^Dim1")
  expect_match(as.character(p$labels$y), "^Dim2")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1))))
})

test_that("plot_biplot honors top_n by keeping only the highest-contributing variables", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p_top <- plotbiplot(fit, top_n = 2)

  expect_s3_class(p_top, "ggplot")
  arrow_layer <- Filter(function(l) inherits(l$geom, "GeomSegment"), p_top$layers)[[1]]
  expect_equal(nrow(arrow_layer$data), 2)
})

test_that("plot_biplot dispatches for cluster_explore and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)
  p1 <- plotbiplot(exp)
  expect_s3_class(p1, "ggplot")

  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "pam"), k = 2:3, seed = 1)
  p2 <- plotbiplot(mfit)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_biplot renders natively for MCA embeddings", {
  skip_if_not_installed("FactoMineR")
  cat_df <- data.frame(
    a = factor(c("x", "x", "y", "y")),
    b = factor(c("u", "v", "u", "v"))
  )
  fit <- cluster(cat_df, method = "kmm", k = 2, seed = 1)
  p <- plotbiplot(fit, embedding = "mca")
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})

test_that("plot_biplot errors for embeddings with no biplot equivalent", {
  d <- stats::dist(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  exp <- explore(fit, embedding = "mds")
  expect_error(plotbiplot(exp), "not supported")
})

test_that("plot_biplot variant = 'cos2' colors individuals by quality of representation", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plotbiplot(fit, variant = "cos2")

  expect_s3_class(p, "ggplot")
  point_layer <- Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)[[1]]
  expect_true("cos2" %in% names(point_layer$data))
})

test_that("plot_biplot variant = 'label' shows individuals as text instead of points", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plotbiplot(fit, variant = "label")

  expect_s3_class(p, "ggplot")
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})

test_that("plot_cluster_sizes returns a bar chart ggplot for cluster_fit, cluster_explore, and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p1 <- plotclustersizes(fit)
  expect_s3_class(p1, "ggplot")
  expect_true(inherits(p1$layers[[1]]$geom, "GeomCol"))

  exp <- explore(fit)
  p2 <- plotclustersizes(exp)
  expect_s3_class(p2, "ggplot")

  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  p3 <- plotclustersizes(mfit)
  expect_s3_class(p3, "ggplot")
})

test_that("plot_feature_profiles filters features and defaults to all features", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)

  p_all <- plotfeatureprofiles(exp)
  expect_s3_class(p_all, "ggplot")
  expect_equal(sort(unique(p_all$data$feature)), sort(colnames(iris[, 1:4])))

  p_subset <- plotfeatureprofiles(exp, features = c("Sepal.Length", "Petal.Length"))
  expect_setequal(unique(p_subset$data$feature), c("Sepal.Length", "Petal.Length"))
})
