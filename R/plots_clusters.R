#' Plot clustered observations in 2D
#'
#' @param x A `cluster_fit` or `cluster_explore` object.
#' @param data Optional numeric data used to compute the embedding.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
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
#' @param x A `cluster_fit` or `cluster_explore` object.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
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
#' @param x A `cluster_explore` object.
#' @param features Optional character vector of features to plot.
#' @param ... Unused.
#'
#' @return A ggplot object.
#' @export
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
