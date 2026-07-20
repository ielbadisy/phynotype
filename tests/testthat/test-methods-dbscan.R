skip_if_not_installed("dbscan")

test_that("validate_dbscan_params rejects missing or invalid eps/minPts", {
  data <- as.matrix(iris[, 1:4])
  expect_error(cluster(data, method = "dbscan", minPts = 5), "eps.*positive")
  expect_error(cluster(data, method = "dbscan", eps = 0.5), "minPts.*positive")
  expect_error(cluster(data, method = "dbscan", eps = -1, minPts = 5), "eps.*positive")
  expect_error(cluster(data, method = "dbscan", eps = 0.5, minPts = 0), "minPts.*positive")
})

test_that("fit_dbscan labels noise as 0 and reports n_clusters excluding noise", {
  sim <- phynotype:::synthetic_clusters(n_per_cluster = 20, seed = 1)
  fit <- cluster(sim$x, method = "dbscan", eps = 0.9, minPts = 5)

  expect_s3_class(fit, "cluster_fit")
  expect_null(fit$centers)
  expect_null(fit$prototypes)
  expect_equal(fit$n_clusters, length(setdiff(unique(fit$clusters), 0L)))
  expect_true(all(fit$clusters >= 0))
})

test_that("predict.cluster_fit for dbscan assigns new points via the nearest-non-noise fallback", {
  sim <- phynotype:::synthetic_clusters(n_per_cluster = 30, seed = 1)
  fit <- cluster(sim$x, method = "dbscan", eps = 0.9, minPts = 5)

  new_data <- sim$x[1:3, , drop = FALSE]
  pred <- predict(fit, new_data)

  expect_s3_class(pred, "cluster_prediction")
  expect_equal(length(pred$clusters), 3)
  expect_true(pred$prediction_type %in% c("native", "nearest_non_noise"))
})

test_that("predict_dbscan_nearest falls back to the noise label when no non-noise points exist", {
  fit <- list(
    clusters = c(0L, 0L, 0L),
    extras = list(noise_label = 0L),
    fitted_object = list(eps = 1)
  )
  new_data <- matrix(c(1, 2, 3), ncol = 1)
  training <- matrix(c(1, 2, 3), ncol = 1)

  out <- phynotype:::predict_dbscan_nearest(fit, new_data, training = training)
  expect_true(all(out == 0L))
})

test_that("predict_dbscan_nearest labels points beyond eps as noise", {
  fit <- list(
    clusters = c(1L, 1L, 2L, 2L),
    extras = list(noise_label = 0L),
    fitted_object = list(eps = 0.5)
  )
  training <- matrix(c(0, 0.1, 10, 10.1), ncol = 1)
  new_data <- matrix(c(0.05, 100), ncol = 1)

  out <- phynotype:::predict_dbscan_nearest(fit, new_data, training = training)
  expect_equal(out[1], 1L)
  expect_equal(out[2], 0L)
})
