#' Render the consensus 2D embedding plot
#'
#' @param x A `cluster_explore` object built from a `metacluster_fit`.
#' @noRd
.plot_consensus_explore <- function(x) {
  labels <- x$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }
  ggplot2::ggplot(x$plot_data, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Consensus clusters", x = labels$x, y = labels$y, color = "Cluster") +
    scale_color_phynotype() +
    theme_phynotype()
}

#' Render the co-association matrix heatmap
#'
#' Displays the \eqn{n \times n} co-association matrix as a heatmap. High
#' values (near 1) indicate pairs of observations that were consistently
#' co-clustered across candidate partitions.
#'
#' @param x A `metacluster_fit` object.
#' @noRd
.plot_coassoc_meta <- function(x) {
  mat <- x$coassoc_matrix
  df <- expand.grid(row = seq_len(nrow(mat)), col = seq_len(ncol(mat)))
  df$value <- as.vector(mat)
  ggplot2::ggplot(df, ggplot2::aes(x = .data[["row"]], y = .data[["col"]], fill = .data[["value"]])) +
    ggplot2::geom_tile() +
    ggplot2::labs(title = "Co-association matrix", x = NULL, y = NULL, fill = "Agreement") +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    theme_phynotype()
}
