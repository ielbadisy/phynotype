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
#' @details
#' The validation plot shows one panel per metric so incompatible scales are
#' never combined on a shared axis. Each panel displays a lollipop-style mark
#' with the numeric value and a strip label that includes the nominal scale and
#' preferred direction when available.
#'
#' @return A ggplot object.
#' @export
plot_validation <- function(x, ...) {
  UseMethod("plot_validation")
}

#' @export
plot_validation.cluster_validation <- function(x, ...) {
  plot_data <- x$metrics_table
  plot_data$panel_label <- paste0(
    plot_data$metric,
    "\n",
    ifelse(is.na(plot_data$scale), "", plot_data$scale),
    ifelse(is.na(plot_data$direction), "", paste0("\n", plot_data$direction))
  )
  ggplot2::ggplot(plot_data, ggplot2::aes(x = 0, y = value)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.4) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 0, y = 0, yend = value), colour = "#2C7FB8", linewidth = 0.9) +
    ggplot2::geom_point(colour = "#542788", size = 2.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = format(value, digits = 3)),
      hjust = -0.15,
      size = 3,
      family = ""
    ) +
    ggplot2::facet_wrap(~panel_label, scales = "free_y", ncol = 1) +
    ggplot2::labs(title = "Validation metrics", x = NULL, y = NULL) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = "grey20"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' @export
plot.cluster_validation <- function(x, ...) {
  plot_validation(x, ...)
}

#' @export
plot.cluster_fit <- function(x, ...) {
  plot_clusters(x, ...)
}
