test_that("accessors on a kmeans cluster_fit expose clusters, centers, sizes, and n_clusters", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)

  expect_equal(clusters(fit), fit$clusters)
  expect_null(membership(fit))
  expect_equal(dim(centers(fit)), c(3, 4))
  expect_null(prototypes(fit))
  expect_equal(sum(sizes(fit)), nrow(iris))
  expect_equal(n_clusters(fit), 3)
  expect_equal(method_used(fit), "kmeans")
})

test_that("accessors on a pam cluster_fit expose prototypes but not centers", {
  skip_if_not_installed("cluster")
  fit <- cluster(iris[, 1:4], method = "pam", k = 3)

  expect_null(centers(fit))
  expect_false(is.null(prototypes(fit)))
  expect_equal(nrow(prototypes(fit)), 3)
})

test_that("accessors on a gmm cluster_fit expose a membership matrix", {
  skip_if_not_installed("mclust")
  fit <- cluster(iris[, 1:4], method = "gmm", k = 3)

  expect_false(is.null(membership(fit)))
  expect_equal(nrow(membership(fit)), nrow(iris))
  expect_equal(ncol(membership(fit)), 3)
})

test_that("sizes() names match cluster labels and sum to n", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  sz <- sizes(fit)
  expect_equal(names(sz), as.character(sort(unique(fit$clusters))))
  expect_equal(sum(sz), nrow(iris))
})

test_that("accessors on a metacluster_fit dispatch to final_clusters/final_k and return NULL for centers/prototypes/membership", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )

  expect_equal(clusters(mfit), mfit$final_clusters)
  expect_null(membership(mfit))
  expect_null(centers(mfit))
  expect_null(prototypes(mfit))
  expect_equal(n_clusters(mfit), mfit$final_k)
  expect_equal(method_used(mfit), "metacluster")
  expect_equal(sum(sizes(mfit)), nrow(iris))
})
