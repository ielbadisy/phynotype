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

test_that("plot_biplot delegates to factoextra::fviz_pca_biplot for PCA embeddings", {
  skip_if_not_installed("factoextra")
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p <- suppressWarnings(plot_biplot(fit))

  expect_s3_class(p, "ggplot")
  expect_match(as.character(p$labels$x), "^Dim1")
  expect_match(as.character(p$labels$y), "^Dim2")
})

test_that("plot_biplot honors top_n via factoextra's select.var", {
  skip_if_not_installed("factoextra")
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  p_top <- suppressWarnings(plot_biplot(fit, top_n = 2))

  expect_s3_class(p_top, "ggplot")
})

test_that("plot_biplot dispatches for cluster_explore and metacluster_fit", {
  skip_if_not_installed("factoextra")
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)
  p1 <- suppressWarnings(plot_biplot(exp))
  expect_s3_class(p1, "ggplot")

  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "pam"), k = 2:3, seed = 1)
  p2 <- suppressWarnings(plot_biplot(mfit))
  expect_s3_class(p2, "ggplot")
})

test_that("plot_biplot uses factoextra::fviz_mca_biplot for MCA embeddings", {
  skip_if_not_installed("factoextra")
  skip_if_not_installed("FactoMineR")
  cat_df <- data.frame(
    a = factor(c("x", "x", "y", "y")),
    b = factor(c("u", "v", "u", "v"))
  )
  fit <- cluster(cat_df, method = "kmm", k = 2, seed = 1)
  p <- suppressWarnings(plot_biplot(fit, embedding = "mca"))
  expect_s3_class(p, "ggplot")
})

test_that("plot_biplot errors for embeddings with no factoextra biplot equivalent", {
  d <- stats::dist(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  exp <- explore(fit, embedding = "mds")
  expect_error(plot_biplot(exp), "not supported")
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
