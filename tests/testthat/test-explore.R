test_that("explore.cluster_fit returns a complete cluster_explore object", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)

  expect_s3_class(exp, "cluster_explore")
  expect_equal(nrow(exp$size_table), 3)
  expect_equal(sum(exp$size_table$size), nrow(iris))
  expect_equal(nrow(exp$feature_summary), 3 * ncol(iris[, 1:4]))
  expect_equal(nrow(exp$separation_table), ncol(iris[, 1:4]))
  expect_equal(nrow(exp$embedding), nrow(iris))
  expect_identical(exp$plot_data, exp$embedding)
  expect_false(is.null(exp$prototype_table))
  expect_equal(dim(exp$prototype_table), c(3, ncol(iris[, 1:4])))
})

test_that("explore.cluster_fit accepts an explicit data argument", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit, data = iris[, 1:4])
  expect_s3_class(exp, "cluster_explore")
  expect_equal(nrow(exp$embedding), nrow(iris))
})

test_that("explore.cluster_fit has NULL prototype_table when no centers/prototypes are stored", {
  skip_if_not_installed("dbscan")
  sim <- phynotype:::synthetic_clusters(n_per_cluster = 20, seed = 1)
  fit <- cluster(sim$x, method = "dbscan", eps = 0.9, minPts = 5)
  exp <- explore(fit)
  expect_null(exp$prototype_table)
})

test_that("explore.metacluster_fit returns a cluster_explore object without a prototype table", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )
  exp <- explore(mfit)

  expect_s3_class(exp, "cluster_explore")
  expect_null(exp$prototype_table)
  expect_equal(sum(exp$size_table$size), nrow(iris))
  expect_equal(nrow(exp$embedding), nrow(iris))
})
