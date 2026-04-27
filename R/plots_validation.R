#' Plot silhouette widths
#'
#' @param x A `cluster_fit` object.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
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
  ggplot2::ggplot(sil_df, ggplot2::aes_string(x = "observation", xend = "observation", y = 0, yend = "width", color = "cluster")) +
    ggplot2::geom_segment(linewidth = 0.7) +
    ggplot2::labs(title = "Silhouette widths", x = "Observation", y = "Silhouette width", color = "Cluster") +
    ggplot2::theme_minimal()
}

#' Plot validation metrics
#'
#' @param x A `cluster_validation` object.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
plot_validation <- function(x, ...) {
  UseMethod("plot_validation")
}

#' @export
plot_validation.cluster_validation <- function(x, ...) {
  ggplot2::ggplot(x$metrics_table, ggplot2::aes_string(x = "metric", y = "value")) +
    ggplot2::geom_col(fill = "#4D9221") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Validation metrics", x = NULL, y = "Value") +
    ggplot2::theme_minimal()
}

#' @export
plot.cluster_validation <- function(x, ...) {
  plot_validation(x, ...)
}

#' @export
plot.cluster_fit <- function(x, ...) {
  plot_clusters(x, ...)
}
