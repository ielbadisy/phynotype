test_that("print.cluster_fit and summary.cluster_fit report method, size, and clusters", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)

  out <- capture.output(print(fit))
  expect_true(any(grepl("<cluster_fit>", out)))
  expect_true(any(grepl("kmeans", out)))
  expect_true(any(grepl(as.character(nrow(iris)), out)))

  s <- summary(fit)
  expect_s3_class(s, "summary.cluster_fit")
  expect_equal(s$method, "kmeans")
  expect_equal(s$n_obs, nrow(iris))
  expect_true(s$has_centers)
  expect_false(s$has_prototypes)

  s_out <- capture.output(print(s))
  expect_true(any(grepl("Cluster fit summary", s_out)))
})

test_that("print.metacluster_fit and summary.metacluster_fit report candidate and final k info", {
  mfit <- metacluster(
    iris[, 1:4],
    methods = c("kmeans", "pam", "hclust"),
    k = 2:3,
    seed = 1
  )

  out <- capture.output(print(mfit))
  expect_true(any(grepl("<metacluster_fit>", out)))
  expect_true(any(grepl(as.character(nrow(mfit$candidate_table)), out)))

  s <- summary(mfit)
  expect_s3_class(s, "summary.metacluster_fit")
  expect_equal(s$final_k, mfit$final_k)
  expect_equal(s$candidate_fits, nrow(mfit$candidate_table))

  s_out <- capture.output(print(s))
  expect_true(any(grepl("Meta-cluster summary", s_out)))
})

test_that("print.cluster_validation prints the metrics table", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  val <- validate(fit)

  out <- capture.output(print(val))
  expect_true(any(grepl("<cluster_validation>", out)))
  expect_true(any(grepl("silhouette", out)))

  expect_identical(summary(val), val)
})

test_that("print.cluster_explore reports the number of feature summary rows", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  exp <- explore(fit)

  out <- capture.output(print(exp))
  expect_true(any(grepl("<cluster_explore>", out)))
  expect_true(any(grepl(as.character(nrow(exp$feature_summary)), out)))
})

test_that("print.cluster_prediction reports method and prediction count", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  pred <- predict(fit, iris[1:5, 1:4])

  out <- capture.output(print(pred))
  expect_true(any(grepl("<cluster_prediction>", out)))
  expect_true(any(grepl("kmeans", out)))
  expect_true(any(grepl("5", out)))
})

test_that("print.feature_importance reports metric, feature count, and repeats", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  imp <- feature_importance(fit, features = 1:2, n_repeats = 2, seed = 10)

  out <- capture.output(print(imp))
  expect_true(any(grepl("<feature_importance>", out)))
  expect_true(any(grepl("2", out)))
})

test_that("print.ceteris_paribus reports target and profile count", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  cp <- ceteris_paribus(fit, iris[1, 1:4, drop = FALSE], features = 1, grid_size = 3)

  out <- capture.output(print(cp))
  expect_true(any(grepl("<ceteris_paribus>", out)))
  expect_true(any(grepl(as.character(nrow(cp$profiles)), out)))
})

test_that("print.lime_explanation reports target, observations, and effect count", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  lx <- lime_explain(fit, iris[1, 1:4, drop = FALSE], n_features = 2, n_permutations = 20, seed = 1)

  out <- capture.output(print(lx))
  expect_true(any(grepl("<lime_explanation>", out)))
  expect_true(any(grepl(as.character(nrow(lx$explanations)), out)))
})
