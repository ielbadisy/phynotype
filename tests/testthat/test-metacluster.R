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

test_that("metacluster mixes native mixed-data and numeric-only methods on mixed data", {
  skip_if_not_installed("clustMixType")
  sim <- phynotype:::synthetic_clusters(n_per_cluster = 20, seed = 1)
  mixed <- data.frame(sim$x, group = factor(sample(c("a", "b"), nrow(sim$x), replace = TRUE)))

  mfit <- metacluster(
    mixed,
    methods = c("kproto", "kmeans", "hclust", "kmm"),
    k = 2,
    seed = 1,
    nstart = 2
  )

  expect_s3_class(mfit, "metacluster_fit")
  expect_equal(length(clusters(mfit)), nrow(mixed))
  expect_true(is.data.frame(mfit$data_info$original_data))

  val <- validate(mfit)
  expect_s3_class(val, "cluster_validation")
  expect_true(all(is.finite(val$metrics_table$value)))
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
  expect_true(any(clusters(fit) == 0) || nclusters(fit) >= 1)
})

test_that("metacluster does not pick the upper edge of the k grid on well-separated data", {
  sim <- phynotype:::synthetic_clusters(
    n_per_cluster = 40, centers = rbind(c(0, 0), c(4, 0), c(0, 4), c(4, 4)), sd = 0.8, seed = 2
  )
  mfit <- metacluster(sim$x, methods = c("kmeans", "pam", "hclust"), k = 2:8, seed = 1)
  expect_equal(mfit$final_k, 4L)
  expect_named(mfit$selection_summary, c("k", "mean_ari", "silhouette", "min_size", "eligible"))
  old <- metacluster(sim$x, methods = c("kmeans", "pam", "hclust"), k = 2:8, seed = 1,
                     select = "silhouette", min_size = 1)
  expect_equal(old$final_k, 8L)
})

test_that("metacluster excludes cuts with clusters smaller than min_size", {
  mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "pam", "hclust"), k = 2:8, seed = 1)
  s <- mfit$selection_summary
  expect_equal(mfit$params$min_size, 2)
  expect_true(any(!s$eligible))
  expect_equal(s$eligible, s$min_size >= 2)
  expect_true(min(table(clusters(mfit))) >= 2)
})
