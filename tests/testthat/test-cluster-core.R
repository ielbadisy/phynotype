test_that("cluster returns a cluster_fit with kmeans", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  expect_s3_class(fit, "cluster_fit")
  expect_length(clusters(fit), nrow(iris))
  expect_equal(method_used(fit), "kmeans")
  expect_equal(dim(centers(fit)), c(3, 4))
})

test_that("pam fit stores prototypes when cluster is available", {
  skip_if_not_installed("cluster")
  fit <- cluster(iris[, 1:4], method = "pam", k = 3)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(nrow(prototypes(fit)), 3)
})

test_that("hclust fit stores hierarchical result and blocks prediction", {
  fit <- cluster(iris[, 1:4], method = "hclust", k = 3)
  expect_s3_class(fit$fitted_object, "hclust")
  expect_error(predict(fit, iris[1:5, 1:4]), "not natively supported")
})

test_that("parameter validation catches invalid k", {
  expect_error(cluster(iris[, 1:4], method = "kmeans", k = 1), "`k`")
})

test_that("mixed data requires an explicit two-step workflow", {
  mixed <- data.frame(
    x = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "b"))
  )
  expect_error(
    cluster(mixed, method = "kmeans", k = 2),
    "explicit two-step workflow",
    fixed = TRUE
  )
})

test_that("prepare_mixed_data encodes mixed inputs for numeric methods", {
  mixed <- data.frame(
    x = c(1, 2, 3, 4),
    group = factor(c("a", "a", "b", "b"))
  )
  encoded <- prepare_mixed_data(mixed, scale = TRUE)
  expect_true(is.matrix(encoded))
  expect_true(is.numeric(encoded))
  expect_true(all(c("x", "group_a", "group_b") %in% colnames(encoded)))
  fit <- cluster(encoded, method = "kmeans", k = 2, seed = 1)
  expect_s3_class(fit, "cluster_fit")
})

test_that("mixed_distance enables hierarchical clustering on mixed inputs", {
  skip_if_not_installed("cluster")
  mixed <- data.frame(
    x = c(1, 2, 8, 9),
    group = factor(c("a", "a", "b", "b"))
  )
  d <- mixed_distance(mixed)
  expect_s3_class(d, "dist")
  fit_h <- cluster(d, method = "hclust", k = 2)
  expect_s3_class(fit_h, "cluster_fit")
  fit_a <- cluster(d, method = "agnes", k = 2)
  expect_s3_class(fit_a, "cluster_fit")
})

test_that("kproto clusters mixed data natively", {
  skip_if_not_installed("clustMixType")
  mixed <- data.frame(
    x = c(1, 2, 8, 9, 1.5, 8.5),
    group = factor(c("a", "a", "b", "b", "a", "b"))
  )
  fit <- cluster(mixed, method = "kproto", k = 2, seed = 1, nstart = 2)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(length(clusters(fit)), nrow(mixed))
  expect_equal(nrow(prototypes(fit)), 2)
  pred <- predict(fit, mixed)
  expect_s3_class(pred, "cluster_prediction")
})

test_that("protomix clusters mixed data natively", {
  mixed <- data.frame(
    x = c(1, 2, 8, 9, 1.5, 8.5),
    group = factor(c("a", "a", "b", "b", "a", "b"))
  )
  fit <- cluster(mixed, method = "protomix", k = 2, seed = 1, nstart = 1, tuner_steps = 2)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(length(clusters(fit)), nrow(mixed))
  expect_true(n_clusters(fit) >= 1)
  pred <- predict(fit, mixed)
  expect_s3_class(pred, "cluster_prediction")
})
