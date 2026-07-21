test_that("validate.cluster_fit computes bootstrap ARI for kmeans, pam, and gmm fits", {
  fit_km <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  val_km <- validate(fit_km, n_boot = 3)
  expect_true("bootstrap_ari" %in% val_km$metrics_table$metric)
  expect_false(is.null(val_km$extras$stability))

  skip_if_not_installed("cluster")
  fit_pam <- cluster(iris[, 1:4], method = "pam", k = 3)
  val_pam <- validate(fit_pam, n_boot = 3)
  expect_true("bootstrap_ari" %in% val_pam$metrics_table$metric)
})

test_that("validate.cluster_fit omits bootstrap ARI for methods without a fixed k parameter", {
  fit_dbscan_ready <- tryCatch(
    cluster(iris[, 1:4], method = "hclust", k = 3),
    error = function(e) NULL
  )
  skip_if(is.null(fit_dbscan_ready))
  val <- validate(fit_dbscan_ready, n_boot = 2)
  expect_false("bootstrap_ari" %in% val$metrics_table$metric)
})

test_that("validate.cluster_fit appends ari and nmi rows when truth is supplied", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  val <- validate(fit, truth = iris$Species)

  expect_true(all(c("ari", "nmi") %in% val$metrics_table$metric))
  ari_row <- val$metrics_table[val$metrics_table$metric == "ari", ]
  expect_equal(ari_row$value, adjusted_rand_index(iris$Species, clusters(fit)))
})

test_that("validate.cluster_fit filters the metrics table when metrics is supplied", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  val <- validate(fit, metrics = c("silhouette", "davies_bouldin"))
  expect_setequal(val$metrics_table$metric, c("silhouette", "davies_bouldin"))
})

test_that("validate.metacluster_fit reports pairwise partition agreement and honors truth/metrics", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  val <- validate(mfit, truth = iris$Species)
  expect_s3_class(val, "cluster_validation")
  expect_true(all(c("ari", "nmi") %in% val$metrics_table$metric))
  if (!is.null(mfit$stability_summary)) {
    expect_true("pairwise_partition_agreement" %in% val$metrics_table$metric)
  }

  val_filtered <- validate(mfit, metrics = "silhouette")
  expect_equal(val_filtered$metrics_table$metric, "silhouette")
})

test_that("validate.default errors on non-matrix/data.frame input and builds a grid over k", {
  expect_error(validate(1:10, method = "kmeans", k = 2), "matrix or data frame")

  val <- validate(iris[, 1:4], method = "kmeans", k = 2:3, seed = 1)
  expect_s3_class(val, "cluster_validation")
  expect_equal(val$object_type, "validation_grid")
  expect_equal(sort(unique(val$metrics_table$k)), 2:3)
})

test_that("validate.cluster_fit errors for distance-only training data", {
  d <- mixed_distance(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)
  expect_error(validate(fit), "not yet implemented")
})
