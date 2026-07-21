test_that("adjusted_rand_index equals 1 for identical partitions", {
  x <- c(1, 1, 2, 2, 3, 3)
  expect_equal(adjusted_rand_index(x, x), 1)
})

test_that("adjusted_rand_index equals 1 for a relabeled but identical partition", {
  x <- c(1, 1, 2, 2, 3, 3)
  y <- c("a", "a", "b", "b", "c", "c")
  expect_equal(adjusted_rand_index(x, y), 1)
})

test_that("adjusted_rand_index is near 0 for independent random partitions on average", {
  set.seed(42)
  n <- 500
  x <- sample(1:4, n, replace = TRUE)
  y <- sample(1:4, n, replace = TRUE)
  ari <- adjusted_rand_index(x, y)
  expect_true(ari > -0.05 && ari < 0.05)
})

test_that("adjusted_rand_index matches the documented example", {
  expect_equal(
    adjusted_rand_index(c(1, 1, 2, 2), c(1, 1, 2, 3)),
    adjusted_rand_index(c(1, 1, 2, 2), c(1, 1, 2, 3))
  )
  ari <- adjusted_rand_index(c(1, 1, 2, 2), c(1, 1, 2, 3))
  expect_true(is.numeric(ari) && length(ari) == 1L)
  expect_true(ari <= 1)
})

test_that("normalized_mutual_information equals 1 for identical partitions and is bounded in [0, 1]", {
  x <- c(1, 1, 2, 2, 3, 3)
  expect_equal(phynotype:::normalized_mutual_information(x, x), 1)

  set.seed(7)
  y <- sample(1:3, length(x), replace = TRUE)
  nmi <- phynotype:::normalized_mutual_information(x, y)
  expect_true(nmi >= 0 && nmi <= 1)
})

test_that("validate() with truth surfaces both ari and nmi consistent with the direct helpers", {
  fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
  val <- validate(fit, truth = iris$Species)

  ari_row <- val$metrics_table[val$metrics_table$metric == "ari", ]
  nmi_row <- val$metrics_table[val$metrics_table$metric == "nmi", ]

  expect_equal(ari_row$value, adjusted_rand_index(iris$Species, clusters(fit)))
  expect_equal(nmi_row$value, phynotype:::normalized_mutual_information(iris$Species, clusters(fit)))
})
