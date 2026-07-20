test_that("plot_dendrogram returns the hclust object invisibly for a hierarchical cluster_fit", {
  d <- mixed_distance(iris[, 1:4])
  fit <- cluster(d, method = "hclust", k = 3)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  out <- plot_dendrogram(fit)

  expect_s3_class(out, "hclust")
  expect_identical(out, fit$fitted_object)
})

test_that("plot_dendrogram errors for non-hierarchical cluster_fit objects", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  expect_error(plot_dendrogram(fit), "hierarchical fits")
})

test_that("plot_dendrogram returns the consensus hclust object for metacluster_fit", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "hclust"),
    k = 2:3,
    seed = 1
  )

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  out <- plot_dendrogram(mfit)

  expect_identical(out, mfit$consensus_fit)
})
