test_that("plot_clusters dispatches for cluster_fit, cluster_explore, and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p1 <- plot_clusters(fit)
  expect_s3_class(p1, "ggplot")
  expect_equal(as.character(p1$labels$x), "PC1")
  expect_equal(as.character(p1$labels$y), "PC2")

  exp <- explore(fit)
  p2 <- plot_clusters(exp)
  expect_s3_class(p2, "ggplot")

  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  p3 <- plot_clusters(mfit)
  expect_s3_class(p3, "ggplot")
  expect_equal(p3$labels$title, "Consensus clusters")
})

test_that("plot_clusters uses MDS labels for distance-based fits", {
  d <- stats::dist(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  p <- plot_clusters(fit)

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
  p <- plot_clusters(fit)

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
  p <- plot_clusters(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$labels$x), "Dim 1")
  expect_equal(as.character(p$labels$y), "Dim 2")
})

test_that("plot_biplot draws variable loading arrows for PCA embeddings", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plot_biplot(fit)

  expect_s3_class(p, "ggplot")
  expect_equal(as.character(p$labels$x), "PC1")
  expect_equal(as.character(p$labels$y), "PC2")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1))))

  p_top <- plot_biplot(fit, top_n = 2)
  segment_layer <- Filter(function(l) inherits(l$geom, "GeomSegment"), p_top$layers)[[1]]
  expect_equal(nrow(segment_layer$data), 2)
})

test_that("plot_biplot dispatches for cluster_explore and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)
  p1 <- plot_biplot(exp)
  expect_s3_class(p1, "ggplot")

  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "pam"), k = 2:3, seed = 1)
  p2 <- plot_biplot(mfit)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_biplot errors for the MDS embedding, which has no variable space", {
  d <- stats::dist(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  exp <- explore(fit, embedding = "mds")
  expect_error(plot_biplot(exp), "no variable loadings")
})

test_that("plot_cluster_network draws one node per cluster and weighted edges", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plot_cluster_network(fit)

  expect_s3_class(p, "ggplot")
  point_layer <- Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)[[1]]
  expect_equal(nrow(point_layer$data), 3)
  segment_layer <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)[[1]]
  expect_equal(nrow(segment_layer$data), 3)
  expect_true(all(segment_layer$data$similarity >= 0 & segment_layer$data$similarity <= 1))
})

test_that("plot_cluster_network honors min_similarity and validates its range", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- plot_cluster_network(fit, min_similarity = 1)
  segment_layer <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  if (length(segment_layer) > 0) {
    expect_true(all(segment_layer[[1]]$data$similarity >= 1))
  }
  expect_error(plot_cluster_network(fit, min_similarity = 2), "min_similarity")
})

test_that("plot_cluster_network dispatches for cluster_explore and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)
  p1 <- plot_cluster_network(exp)
  expect_s3_class(p1, "ggplot")

  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "pam"), k = 2:3, seed = 1)
  p2 <- plot_cluster_network(mfit)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_cluster_sizes returns a bar chart ggplot for cluster_fit, cluster_explore, and metacluster_fit", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p1 <- plot_cluster_sizes(fit)
  expect_s3_class(p1, "ggplot")
  expect_true(inherits(p1$layers[[1]]$geom, "GeomCol"))

  exp <- explore(fit)
  p2 <- plot_cluster_sizes(exp)
  expect_s3_class(p2, "ggplot")

  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  p3 <- plot_cluster_sizes(mfit)
  expect_s3_class(p3, "ggplot")
})

test_that("plot_feature_profiles filters features and defaults to all features", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)

  p_all <- plot_feature_profiles(exp)
  expect_s3_class(p_all, "ggplot")
  expect_equal(sort(unique(p_all$data$feature)), sort(colnames(iris[, 1:4])))

  p_subset <- plot_feature_profiles(exp, features = c("Sepal.Length", "Petal.Length"))
  expect_setequal(unique(p_subset$data$feature), c("Sepal.Length", "Petal.Length"))
})
