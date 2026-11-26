#' Plot silhouette widths
#'
#' Display observation-level silhouette widths colored by cluster, grouped by
#' cluster and sorted by decreasing width within each cluster. Observations
#' with negative widths lie closer to a neighboring cluster than to their own
#' and may be misclassified. The `row` column of the plot data gives the
#' original row index.
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
#'   plotsilhouette(fit)
#' }
plotsilhouette <- function(x, ...) {
  UseMethod("plotsilhouette")
}

#' @export
plotsilhouette.cluster_fit <- function(x, ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for silhouette plots.", call. = FALSE)
  }
  data <- metric_basis(x$data_info$original_data, x$data_info)
  sil <- cluster::silhouette(as.integer(x$clusters), stats::dist(data))
  # classic silhouette layout: clusters in turn, widths decreasing within each
  o <- order(sil[, "cluster"], -sil[, "sil_width"])
  sil_df <- data.frame(
    observation = seq_along(o),
    row = o,
    cluster = factor(sil[o, "cluster"]),
    width = sil[o, "sil_width"]
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
    ggplot2::labs(title = "Silhouette widths", x = "Observation (sorted within cluster)", y = "Silhouette width", color = "Cluster") +
    scalecolorphynotype() +
    themephynotype()
}

#' @export
plot.cluster_fit <- function(x, ...) {
  plotclusters(x, ...)
}
