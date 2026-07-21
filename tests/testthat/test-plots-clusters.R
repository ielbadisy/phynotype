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
