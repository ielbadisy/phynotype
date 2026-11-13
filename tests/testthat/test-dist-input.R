test_that("pam accepts a dist object (Gower + PAM)", {
  skip_if_not_installed("cluster")
  d <- mixeddistance(iris)
  fit <- cluster(d, method = "pam", k = 3)
  expect_s3_class(fit, "cluster_fit")
  expect_equal(nclusters(fit), 3L)
  expect_length(clusters(fit), 150L)
  ref <- cluster::pam(d, k = 3, diss = TRUE)$clustering
  expect_equal(unname(clusters(fit)), unname(ref))
  expect_length(fit$extras$medoid_ids, 3L)
  expect_error(predict(fit, iris[1:5, ]), "distance object")
})

test_that("pam on raw data is unchanged", {
  skip_if_not_installed("cluster")
  fit <- cluster(iris[, 1:4], method = "pam", k = 3)
  expect_equal(nrow(fit$prototypes), 3L)
  expect_length(predict(fit, iris[1:5, 1:4])$clusters, 5L)
})

test_that("dbscan accepts a dist object", {
  skip_if_not_installed("dbscan")
  d <- stats::dist(scale(iris[, 1:4]))
  fit <- cluster(d, method = "dbscan", eps = 0.6, minPts = 5)
  ref <- dbscan::dbscan(d, eps = 0.6, minPts = 5)$cluster
  expect_equal(unname(clusters(fit)), ref)
  expect_error(predict(fit, iris[1:5, 1:4]), "distance object")
})

test_that("dist input is still rejected for kmeans and gmm", {
  d <- stats::dist(iris[, 1:4])
  expect_error(cluster(d, method = "kmeans", k = 3), "Distance inputs")
  expect_error(cluster(d, method = "gmm", k = 3), "Distance inputs")
})
