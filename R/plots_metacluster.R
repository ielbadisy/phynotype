#' Plot consensus clustering in 2D
#'
#' @param x A `metacluster_fit` object.
#' @param data Optional numeric data used to compute the embedding.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
plot_consensus <- function(x, data = NULL, ...) {
  UseMethod("plot_consensus")
}

#' @export
plot_consensus.metacluster_fit <- function(x, data = NULL, ...) {
  exp <- explore(x, data = data)
  ggplot2::ggplot(exp$plot_data, ggplot2::aes_string(x = "x", y = "y", color = "cluster")) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Consensus clusters", x = "PC1", y = "PC2", color = "Cluster") +
    ggplot2::theme_minimal()
}

#' Plot the co-association matrix
#'
#' @param x A `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
plot_coassoc <- function(x, ...) {
  UseMethod("plot_coassoc")
}

#' @export
plot_coassoc.metacluster_fit <- function(x, ...) {
  mat <- x$coassoc_matrix
  df <- expand.grid(row = seq_len(nrow(mat)), col = seq_len(ncol(mat)))
  df$value <- as.vector(mat)
  ggplot2::ggplot(df, ggplot2::aes_string(x = "row", y = "col", fill = "value")) +
    ggplot2::geom_tile() +
    ggplot2::labs(title = "Co-association matrix", x = NULL, y = NULL, fill = "Agreement") +
    ggplot2::theme_minimal()
}
