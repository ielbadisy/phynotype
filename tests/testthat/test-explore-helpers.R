test_that("build_feature_summary computes correct per-cluster statistics", {
  data <- data.frame(x = c(1, 2, 3, 10, 11, 12), y = c(5, 5, 5, 1, 1, 1))
  clusters <- c(1, 1, 1, 2, 2, 2)

  summary_df <- phynotype:::build_feature_summary(data, clusters)

  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 4)
  expect_named(summary_df, c("cluster", "feature", "mean", "sd", "median", "min", "max"))

  row_x1 <- summary_df[summary_df$cluster == 1 & summary_df$feature == "x", ]
  expect_equal(row_x1$mean, mean(c(1, 2, 3)))
  expect_equal(row_x1$sd, stats::sd(c(1, 2, 3)))
  expect_equal(row_x1$median, stats::median(c(1, 2, 3)))
  expect_equal(row_x1$min, 1)
  expect_equal(row_x1$max, 3)

  row_y2 <- summary_df[summary_df$cluster == 2 & summary_df$feature == "y", ]
  expect_equal(row_y2$mean, 1)
  expect_equal(row_y2$sd, 0)
})

test_that("compute_separation_table returns eta-squared values bounded in [0, 1]", {
  data <- as.matrix(iris[, 1:4])
  clusters <- as.integer(iris$Species)

  sep <- phynotype:::compute_separation_table(data, clusters)

  expect_s3_class(sep, "data.frame")
  expect_named(sep, c("feature", "separation"))
  expect_equal(sep$feature, colnames(data))
  expect_true(all(sep$separation >= 0 & sep$separation <= 1))
  # Petal.Length/Petal.Width separate species almost perfectly.
  expect_true(sep$separation[sep$feature == "Petal.Length"] > 0.9)
})

test_that("compute_separation_table is near zero when clusters carry no signal", {
  set.seed(1)
  data <- matrix(stats::rnorm(200), ncol = 2, dimnames = list(NULL, c("a", "b")))
  clusters <- rep(c(1, 2), length.out = nrow(data))

  sep <- phynotype:::compute_separation_table(data, clusters)
  expect_true(all(sep$separation < 0.2))
})
