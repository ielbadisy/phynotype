mixed_kmm_data <- function() {
  data.frame(
    x = c(1, 2, 1.5, 8, 9, 8.5, 1, 2, 8, 9),
    group = factor(c("a", "a", "a", "b", "b", "b", "a", "a", "b", "b"))
  )
}

test_that("cluster(method = 'kmm') fits mixed data and stores combined prototypes", {
  mixed <- mixed_kmm_data()
  fit <- cluster(mixed, method = "kmm", k = 2, seed = 1, nstart = 2)

  expect_s3_class(fit, "cluster_fit")
  expect_equal(fit$method, "kmm")
  expect_equal(length(clusters(fit)), nrow(mixed))
  expect_equal(n_clusters(fit), 2)
  expect_false(is.null(prototypes(fit)))
  expect_equal(nrow(prototypes(fit)), 2)
  expect_true(all(c("x", "group") %in% colnames(prototypes(fit))))
  expect_true(fit$extras$lambda >= 0)
})

test_that("cluster(method = 'kmm') recovers the two well-separated groups", {
  mixed <- mixed_kmm_data()
  fit <- cluster(mixed, method = "kmm", k = 2, seed = 1, nstart = 5)
  cls <- clusters(fit)
  expect_equal(length(unique(cls[mixed$group == "a"])), 1)
  expect_equal(length(unique(cls[mixed$group == "b"])), 1)
  expect_false(identical(unique(cls[mixed$group == "a"]), unique(cls[mixed$group == "b"])))
})

test_that("validate_kmm_params rejects non-data-frame input and k exceeding n", {
  mixed <- mixed_kmm_data()
  expect_error(
    phynotype:::validate_kmm_params(as.matrix(mixed), params = list(k = 2)),
    "row-by-feature"
  )
  expect_error(cluster(mixed, method = "kmm", k = nrow(mixed) + 1), "cannot exceed")
})

test_that("predict.cluster_fit for kmm assigns new observations to nearest prototype", {
  mixed <- mixed_kmm_data()
  fit <- cluster(mixed, method = "kmm", k = 2, seed = 1, nstart = 2)

  new_data <- data.frame(x = c(1.2, 8.7), group = factor(c("a", "b")))
  pred <- predict(fit, new_data)

  expect_s3_class(pred, "cluster_prediction")
  expect_equal(pred$prediction_type, "nearest_prototype")
  expect_equal(length(pred$clusters), 2)
  expect_false(identical(pred$clusters[1], pred$clusters[2]))
})

test_that("predict.cluster_fit for kmm errors when new_data is missing required columns", {
  mixed <- mixed_kmm_data()
  fit <- cluster(mixed, method = "kmm", k = 2, seed = 1, nstart = 2)
  expect_error(predict(fit, data.frame(x = 1)), "missing variables")
})

test_that("lambda_kmm and estimate_lambda compute a non-negative weight", {
  mixed <- mixed_kmm_data()
  lam <- phynotype:::lambda_kmm(mixed)
  expect_true(is.numeric(lam))
  expect_true(lam >= 0)
})

test_that("kmm() errors on invalid lambda", {
  mixed <- mixed_kmm_data()
  expect_error(
    phynotype:::kmm(mixed, k = 2, lambda = -1),
    "non-negative"
  )
})
