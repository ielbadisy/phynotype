synthetic_clusters <- function(n_per_cluster = 40, centers = NULL, sd = 0.5, seed = 1) {
  if (is.null(centers)) {
    centers <- matrix(c(0, 0, 3, 3, -3, 3), ncol = 2, byrow = TRUE)
  }
  set.seed(seed)
  parts <- lapply(seq_len(nrow(centers)), function(i) {
    matrix(
      stats::rnorm(n_per_cluster * ncol(centers), mean = rep(centers[i, ], each = n_per_cluster), sd = sd),
      ncol = ncol(centers),
      byrow = FALSE
    )
  })
  x <- do.call(rbind, parts)
  y <- rep(seq_len(nrow(centers)), each = n_per_cluster)
  colnames(x) <- paste0("x", seq_len(ncol(x)))
  list(x = x, y = y)
}
