#' Plot consensus clustering in 2D
#'
#' Project the consensus cluster assignments onto a two-dimensional embedding.
#'
#' @param x A `metacluster_fit` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for row-by-feature
#'   data and classical MDS for distance inputs. `"pca"` and `"mds"` may be
#'   selected explicitly.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [metacluster()], [plot_coassoc()]
#'
#' @export
#'
#' @examples
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"),
#'                     k = 2:3, seed = 1)
#' plot_consensus(mfit)
plot_consensus <- function(x, data = NULL, embedding = c("auto", "pca", "mds"), ...) {
  UseMethod("plot_consensus")
}

#' @export
plot_consensus.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "mds"), ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  labels <- exp$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }
  ggplot2::ggplot(exp$plot_data, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Consensus clusters", x = labels$x, y = labels$y, color = "Cluster") +
    ggplot2::theme_minimal()
}

#' Plot the co-association matrix
#'
#' Display the \eqn{n \times n} co-association matrix as a heatmap. High values
#' (near 1) indicate pairs of observations that were consistently co-clustered
#' across candidate partitions.
#'
#' @param x A `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [metacluster()], [plot_consensus()]
#'
#' @export
#'
#' @examples
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"),
#'                     k = 2:3, seed = 1)
#' plot_coassoc(mfit)
plot_coassoc <- function(x, ...) {
  UseMethod("plot_coassoc")
}

#' @export
plot_coassoc.metacluster_fit <- function(x, ...) {
  mat <- x$coassoc_matrix
  df <- expand.grid(row = seq_len(nrow(mat)), col = seq_len(ncol(mat)))
  df$value <- as.vector(mat)
  ggplot2::ggplot(df, ggplot2::aes(x = .data[["row"]], y = .data[["col"]], fill = .data[["value"]])) +
    ggplot2::geom_tile() +
    ggplot2::labs(title = "Co-association matrix", x = NULL, y = NULL, fill = "Agreement") +
    ggplot2::theme_minimal()
}
