test_that("feature_importance returns reproducible permutation summaries", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  imp1 <- feature_importance(fit, features = 1:2, n_repeats = 2, seed = 10)
  imp2 <- feature_importance(fit, features = 1:2, n_repeats = 2, seed = 10)

  expect_s3_class(imp1, "feature_importance")
  expect_true(all(c("feature", "importance", "std_error", "n_repeats") %in% names(imp1$summary)))
  expect_equal(imp1$summary, imp2$summary)
  expect_equal(nrow(imp1$results), 4)
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
