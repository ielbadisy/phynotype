#' Plot clustered observations in 2D
#'
#' Project observations onto their first two principal components and color
#' them by cluster assignment. For `metacluster_fit` objects, delegates to
#' [plot_consensus()].
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix or data frame used to compute the PCA
#'   embedding. Defaults to the training data stored in `x`.
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
plot_clusters <- function(x, data = NULL, ...) {
  UseMethod("plot_clusters")
}

#' @export
plot_clusters.cluster_fit <- function(x, data = NULL, ...) {
  exp <- explore(x, data = data)
  plot_clusters(exp)
}

#' @export
plot_clusters.metacluster_fit <- function(x, data = NULL, ...) {
  plot_consensus(x, data = data, ...)
}

#' @export
plot_clusters.cluster_explore <- function(x, ...) {
  ggplot2::ggplot(x$plot_data, ggplot2::aes_string(x = "x", y = "y", color = "cluster")) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Cluster embedding", x = "PC1", y = "PC2", color = "Cluster") +
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
  ggplot2::ggplot(dat, ggplot2::aes_string(x = "cluster", y = "size")) +
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
  dat <- x$feature_summary
  if (!is.null(features)) {
    dat <- dat[dat$feature %in% features, , drop = FALSE]
  }
  dat$cluster <- factor(dat$cluster)
  ggplot2::ggplot(dat, ggplot2::aes_string(x = "feature", y = "mean", fill = "cluster")) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Feature profiles", x = "Feature", y = "Mean", fill = "Cluster") +
    ggplot2::theme_bw()
}
