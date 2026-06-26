#' Plot silhouette widths
#'
#' Display observation-level silhouette widths colored by cluster. Observations
#' with negative widths lie closer to a neighboring cluster than to their own
#' and may be misclassified.
#'
#' @param x A `cluster_fit` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [validate()]
#'
#' @export
#'
#' @examples
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#'   plot_silhouette(fit)
#' }
plot_silhouette <- function(x, ...) {
  UseMethod("plot_silhouette")
}

#' @export
plot_silhouette.cluster_fit <- function(x, ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for silhouette plots.", call. = FALSE)
  }
  data <- x$data_info$original_data
  sil <- cluster::silhouette(as.integer(x$clusters), stats::dist(data))
  sil_df <- data.frame(
    observation = seq_len(nrow(sil)),
    cluster = factor(sil[, "cluster"]),
    width = sil[, "sil_width"]
  )
  ggplot2::ggplot(
    sil_df,
    ggplot2::aes(
      x = .data[["observation"]],
      xend = .data[["observation"]],
      y = 0,
      yend = .data[["width"]],
      color = .data[["cluster"]]
    )
  ) +
    ggplot2::geom_segment(linewidth = 0.7) +
    ggplot2::labs(title = "Silhouette widths", x = "Observation", y = "Silhouette width", color = "Cluster") +
    ggplot2::theme_minimal()
}

#' @export
plot.cluster_fit <- function(x, ...) {
  plot_clusters(x, ...)
}
