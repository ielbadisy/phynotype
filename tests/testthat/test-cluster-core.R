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
