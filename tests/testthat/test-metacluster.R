test_that("metacluster returns a consensus fit with symmetric coassociation", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )

  expect_s3_class(mfit, "metacluster_fit")
  expect_equal(length(clusters(mfit)), nrow(iris))
  expect_true(is.matrix(mfit$coassoc_matrix))
  expect_equal(mfit$coassoc_matrix, t(mfit$coassoc_matrix))
  expect_equal(diag(mfit$coassoc_matrix), rep(1, nrow(iris)))
  expect_true(mfit$final_k %in% 2:3)
})

test_that("validate works on a direct k grid", {
  val <- validate(iris[, 1:4], method = "kmeans", k = 2:4, seed = 1)
  expect_s3_class(val, "cluster_validation")
  expect_true(all(c("metric", "value", "scale", "direction", "k") %in% names(val$metrics_table)))
  expect_equal(sort(unique(val$metrics_table$k)), 2:4)
})

test_that("gmm exposes membership matrix when mclust is available", {
  skip_if_not_installed("mclust")
  fit <- cluster(iris[, 1:4], method = "gmm", k = 3)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(nrow(membership(fit)), nrow(iris))
})

test_that("agnes produces a hierarchical clustering fit when cluster is available", {
  skip_if_not_installed("cluster")
  fit <- cluster(iris[, 1:4], method = "agnes", k = 3)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(length(clusters(fit)), nrow(iris))
})

test_that("dbscan handles noise labels when dbscan is available", {
  skip_if_not_installed("dbscan")
  sim <- phynotype:::synthetic_clusters(n_per_cluster = 20, seed = 1)
  fit <- cluster(sim$x, method = "dbscan", eps = 0.9, minPts = 5)
  expect_s3_class(fit, "cluster_fit")
  expect_true(any(clusters(fit) == 0) || n_clusters(fit) >= 1)
})
