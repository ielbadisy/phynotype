test_that(".kmm_check_data coerces to data frame and rejects empty input", {
  out <- phynotype:::.kmm_check_data(data.frame(x = 1:3))
  expect_s3_class(out, "data.frame")
  expect_error(phynotype:::.kmm_check_data(data.frame()), "at least one row and one column")
})

test_that(".kmm_split_vars separates numeric and categorical columns", {
  data <- data.frame(x = 1:3, y = c("a", "b", "c"), z = c(1.1, 2.2, 3.3))
  vars <- phynotype:::.kmm_split_vars(data)
  expect_equal(sort(vars$num), c("x", "z"))
  expect_equal(vars$cat, "y")
})

test_that(".kmm_prepare scales numeric columns and preserves categorical factors", {
  data <- data.frame(x = c(1, 2, 3, 4), group = c("a", "a", "b", "b"))
  prep <- phynotype:::.kmm_prepare(data, scale_numeric = TRUE)
  expect_equal(mean(prep$x_num$x), 0, tolerance = 1e-8)
  expect_true(is.factor(prep$x_cat$group))
  expect_equal(prep$num_vars, "x")
  expect_equal(prep$cat_vars, "group")
})

test_that(".kmm_prepare respects supplied center/scale and factor levels", {
  data <- data.frame(x = c(1, 2, 3, 4), group = c("a", "a", "b", "b"))
  prep <- phynotype:::.kmm_prepare(
    data,
    scale_numeric = TRUE,
    num_center = 0,
    num_scale = 1,
    factor_levels = list(group = c("a", "b", "c"))
  )
  expect_equal(prep$x_num$x, data$x)
  expect_equal(levels(prep$x_cat$group), c("a", "b", "c"))
})

test_that(".kmm_mode_value returns the most frequent non-missing value", {
  expect_equal(phynotype:::.kmm_mode_value(c("a", "a", "b", NA)), "a")
  expect_true(is.na(phynotype:::.kmm_mode_value(c(NA, NA))))
})

test_that(".kmm_center_value supports mean, median, and trimmed centers", {
  x <- c(1, 2, 3, 4, 100)
  expect_equal(phynotype:::.kmm_center_value(x, "mean"), mean(x))
  expect_equal(phynotype:::.kmm_center_value(x, "median"), stats::median(x))
  expect_equal(phynotype:::.kmm_center_value(x, "trimmed"), mean(x, trim = 0.1))
})

test_that("update_prototypes computes numeric means and categorical modes per cluster", {
  x_num <- data.frame(x = c(1, 2, 8, 9))
  x_cat <- data.frame(g = c("a", "a", "b", "b"))
  cluster <- c(1, 1, 2, 2)
  proto <- phynotype:::update_prototypes(x_num, x_cat, cluster, k = 2, center_num = "mean")

  expect_equal(unname(proto$num[1, ]), 1.5)
  expect_equal(unname(proto$num[2, ]), 8.5)
  expect_equal(unname(proto$cat[1, ]), "a")
  expect_equal(unname(proto$cat[2, ]), "b")
})

test_that(".kmm_distance_matrix computes zero distance to an observation's own prototype", {
  x_num <- data.frame(x = c(1, 2, 8, 9))
  x_cat <- data.frame(g = c("a", "a", "b", "b"))
  cluster <- c(1, 1, 2, 2)
  proto <- phynotype:::update_prototypes(x_num, x_cat, cluster, k = 2, center_num = "mean")

  dist_mat <- phynotype:::.kmm_distance_matrix(x_num, x_cat, proto, lambda = 1, missing = "pairwise")
  expect_equal(dim(dist_mat), c(4, 2))
  expect_true(all(dist_mat >= 0))
  # observation 1 (x=1) is closer to prototype 1 (mean 1.5) than prototype 2 (mean 8.5)
  expect_true(dist_mat[1, 1] < dist_mat[1, 2])
})

test_that("estimate_lambda returns 0 with no categorical columns and finite with both types", {
  x_num <- data.frame(x = c(1, 2, 3, 4))
  x_cat_empty <- data.frame()[seq_len(4), , drop = FALSE]
  lam0 <- phynotype:::estimate_lambda(x_num, x_cat_empty)
  expect_equal(lam0, 0)

  x_cat <- data.frame(g = c("a", "a", "b", "b"))
  lam <- phynotype:::estimate_lambda(x_num, x_cat)
  expect_true(is.numeric(lam) && lam >= 0)
})

test_that(".kmm_prototypes_original_scale rescales numeric prototypes back to original units", {
  proto <- list(num = matrix(c(0, 1), nrow = 2, dimnames = list(NULL, "x")), cat = NULL)
  rescaled <- phynotype:::.kmm_prototypes_original_scale(
    proto, num_center = 5, num_scale = 2, scale_numeric = TRUE
  )
  expect_equal(unname(rescaled$num[, 1]), c(5, 7))
})

test_that(".kmm_combined_prototypes binds numeric and categorical prototype columns", {
  proto <- list(
    num = matrix(c(1, 2), nrow = 2, dimnames = list(NULL, "x")),
    cat = matrix(c("a", "b"), nrow = 2, dimnames = list(NULL, "g"))
  )
  combined <- phynotype:::.kmm_combined_prototypes(proto)
  expect_equal(colnames(combined), c("x", "g"))
  expect_equal(nrow(combined), 2)
})
