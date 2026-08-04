#' Plot clustered observations in 2D
#'
#' Project observations onto a two-dimensional embedding and color them by
#' cluster assignment. For `metacluster_fit` objects, delegates to
#' [plot_consensus()].
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data,
#'   FAMD for mixed numeric/categorical data, MCA for categorical data, and
#'   classical MDS for distance inputs. `"pca"`, `"famd"`, `"mca"`, and
#'   `"mds"` may be selected explicitly.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [explore()] for the underlying embedding, [plot_cluster_sizes()],
#'   [plot_feature_profiles()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_clusters(fit)
plot_clusters <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  UseMethod("plot_clusters")
}

#' @export
plot_clusters.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_clusters(exp)
}

#' @export
plot_clusters.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  plot_consensus(x, data = data, embedding = embedding, ...)
}

#' @export
plot_clusters.cluster_explore <- function(x, ...) {
  labels <- x$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }
  ggplot2::ggplot(x$plot_data, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Cluster embedding", x = labels$x, y = labels$y, color = "Cluster") +
    ggplot2::theme_minimal()
}

#' Plot cluster sizes
#'
#' Display a bar chart of the number of observations per cluster.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [sizes()], [plot_clusters()], [plot_feature_profiles()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_cluster_sizes(fit)
plot_cluster_sizes <- function(x, ...) {
  UseMethod("plot_cluster_sizes")
}

#' @export
plot_cluster_sizes.cluster_fit <- function(x, ...) {
  exp <- explore(x)
  plot_cluster_sizes(exp)
}

#' @export
plot_cluster_sizes.metacluster_fit <- function(x, ...) {
  exp <- explore(x)
  plot_cluster_sizes(exp)
}

#' @export
plot_cluster_sizes.cluster_explore <- function(x, ...) {
  dat <- x$size_table
  dat$cluster <- factor(dat$cluster)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[["cluster"]], y = .data[["size"]])) +
    ggplot2::geom_col(fill = "#2C7FB8") +
    ggplot2::labs(title = "Cluster sizes", x = "Cluster", y = "Size") +
    ggplot2::theme_minimal()
}

#' Plot feature profiles by cluster
#'
#' Display per-cluster mean values for each feature as a grouped bar chart.
#'
#' @param x A `cluster_explore` object (produced by [explore()]).
#' @param features Optional character vector of feature names to include.
#'   Defaults to all features.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [explore()], [plot_clusters()], [plot_cluster_sizes()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' exp <- explore(fit)
#' plot_feature_profiles(exp)
#' plot_feature_profiles(exp, features = c("Sepal.Length", "Petal.Length"))
plot_feature_profiles <- function(x, features = NULL, ...) {
  UseMethod("plot_feature_profiles")
}

#' @export
plot_feature_profiles.cluster_explore <- function(x, features = NULL, ...) {
  if (is.null(x$feature_summary)) {
    stop("Feature profiles are unavailable for this explore object.", call. = FALSE)
  }
  dat <- x$feature_summary
  if (!is.null(features)) {
    dat <- dat[dat$feature %in% features, , drop = FALSE]
  }
  dat$cluster <- factor(dat$cluster)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[["feature"]], y = .data[["mean"]], fill = .data[["cluster"]])) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Feature profiles", x = "Feature", y = "Mean", fill = "Cluster") +
    ggplot2::theme_bw()
}
