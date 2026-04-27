compute_pca_embedding <- function(data, clusters) {
  pc <- stats::prcomp(data, center = TRUE, scale. = TRUE)
  data.frame(
    x = pc$x[, 1],
    y = pc$x[, 2],
    cluster = factor(clusters)
  )
}
