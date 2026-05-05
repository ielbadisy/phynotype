test_that("feature_importance returns reproducible permutation summaries", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  imp1 <- feature_importance(fit, features = 1:2, n_repeats = 2, seed = 10)
  imp2 <- feature_importance(fit, features = 1:2, n_repeats = 2, seed = 10)

  expect_s3_class(imp1, "feature_importance")
  expect_true(all(c("feature", "importance", "std_error", "n_repeats") %in% names(imp1$summary)))
  expect_equal(imp1$summary, imp2$summary)
  expect_equal(nrow(imp1$results), 4)
})

test_that("interpretability works with hierarchical nearest-centroid prediction", {
  fit_hclust <- cluster(iris[, 1:4], method = "hclust", k = 3)
  imp <- feature_importance(fit_hclust, features = 1, n_repeats = 1, seed = 1)
  cp <- ceteris_paribus(fit_hclust, iris[1, 1:4, drop = FALSE], features = 1, grid_size = 3)
  lx <- lime_explain(fit_hclust, iris[1, 1:4, drop = FALSE], n_features = 2, n_permutations = 20, seed = 1)

  expect_s3_class(imp, "feature_importance")
  expect_s3_class(cp, "ceteris_paribus")
  expect_s3_class(lx, "lime_explanation")
})

test_that("interpretability smoke-tests all supported numeric clustering methods", {
  x <- iris[1:60, 1:4]
  fits <- list(
    kmeans = cluster(x, method = "kmeans", k = 2, seed = 1),
    hclust = cluster(x, method = "hclust", k = 2)
  )
  if (requireNamespace("cluster", quietly = TRUE)) {
    fits$pam <- cluster(x, method = "pam", k = 2)
    fits$agnes <- cluster(x, method = "agnes", k = 2)
  }
  if (requireNamespace("dbscan", quietly = TRUE)) {
    fits$dbscan <- cluster(x, method = "dbscan", eps = 0.7, minPts = 3)
  }
  if (requireNamespace("mclust", quietly = TRUE)) {
    fits$gmm <- cluster(x, method = "gmm", k = 2)
  }

  for (fit in fits) {
    pred <- predict(fit, x[1:3, , drop = FALSE])
    imp <- feature_importance(fit, features = 1, n_repeats = 1, seed = 1)
    cp <- ceteris_paribus(fit, x[1, , drop = FALSE], features = 1, grid_size = 3)

    expect_s3_class(pred, "cluster_prediction")
    expect_s3_class(imp, "feature_importance")
    expect_s3_class(cp, "ceteris_paribus")
  }
})

test_that("interpretability works with native mixed-data kproto fits", {
  skip_if_not_installed("clustMixType")
  mixed <- data.frame(
    x = c(1, 1.2, 5, 5.2, 1.1, 5.1),
    y = c(0, 0.1, 3, 3.2, 0.2, 3.1),
    group = factor(c("a", "a", "b", "b", "a", "b"))
  )
  fit_kproto <- cluster(mixed, method = "kproto", k = 2, seed = 1, nstart = 2)
  imp <- feature_importance(fit_kproto, features = "group", n_repeats = 1, seed = 1)
  cp <- ceteris_paribus(
    fit_kproto,
    mixed[1, , drop = FALSE],
    features = "group",
    target = "score"
  )
  lx <- lime_explain(fit_kproto, mixed[1, , drop = FALSE], n_features = 2, n_permutations = 20, seed = 1)

  expect_s3_class(imp, "feature_importance")
  expect_s3_class(cp, "ceteris_paribus")
  expect_s3_class(lx, "lime_explanation")
  expect_true(all(cp$profiles$feature_value %in% c("a", "b")))
})

test_that("cores controls functionals-backed parallel dispatch", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  imp <- feature_importance(
    fit,
    features = 1:2,
    n_repeats = 1,
    seed = 1,
    parallel = TRUE,
    cores = 1
  )

  expect_s3_class(imp, "feature_importance")
  expect_equal(imp$settings$cores, 1)
})

test_that("distance-only hierarchical fits have an explicit interpretability boundary", {
  skip_if_not_installed("cluster")
  d <- mixed_distance(data.frame(x = c(1, 2, 8, 9), group = factor(c("a", "a", "b", "b"))))
  fit <- cluster(d, method = "hclust", k = 2)
  expect_error(
    feature_importance(fit, data = d, n_repeats = 1),
    "row-by-feature data"
  )
})

test_that("ceteris_paribus returns one row per observation-feature-grid point", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  cp <- ceteris_paribus(fit, iris[1:2, 1:4], features = 1:2, grid_size = 4)

  expect_s3_class(cp, "ceteris_paribus")
  expect_true(all(c(
    "observation", "feature", "feature_value", "target", "cluster", "value",
    "observed_value", "baseline_value", "baseline_cluster"
  ) %in% names(cp$profiles)))
  expect_equal(nrow(cp$profiles), 2 * 2 * 4)
})

test_that("ceteris_paribus can profile cluster scores", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  cp <- ceteris_paribus(fit, iris[1, 1:4, drop = FALSE], features = "Sepal.Length", grid_size = 3, target = "score")

  expect_s3_class(cp, "ceteris_paribus")
  expect_true(all(cp$profiles$value >= 0))
  expect_true(all(cp$profiles$value <= 1))
})

test_that("ceteris_paribus plot uses the profile display path", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  cp <- ceteris_paribus(fit, iris[1, 1:4, drop = FALSE], features = 1:2, grid_size = 4, target = "score")
  plt <- plot(cp)

  expect_s3_class(plt, "ggplot")
  expect_equal(plt$labels$title, "Ceteris Paribus profile")
  expect_equal(plt$labels$y, "prediction")
})

test_that("lime_explain returns local surrogate effects", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  lx <- lime_explain(fit, iris[1:2, 1:4], n_features = 2, n_permutations = 30, seed = 1)

  expect_s3_class(lx, "lime_explanation")
  expect_true(all(c("observation", "cluster", "feature", "estimate", "absolute_effect", "rank") %in% names(lx$explanations)))
  expect_equal(nrow(lx$explanations), 4)
  expect_equal(nrow(lx$neighborhoods), 60)
})
