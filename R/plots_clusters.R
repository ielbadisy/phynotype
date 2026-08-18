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
    theme_phynotype()
}

#' Plot a cluster biplot
#'
#' A thin wrapper around `factoextra`'s `fviz_pca_biplot()` and
#' `fviz_mca_biplot()`, coloring individuals by cluster assignment
#' (`habillage`). Only the `"pca"` and `"mca"` embeddings are supported,
#' matching the biplot functions `factoextra` itself provides; there is no
#' `fviz_famd_biplot()` equivalent, and MDS has no variable space to project.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data
#'   and MCA for categorical data. `"pca"` and `"mca"` may be selected
#'   explicitly.
#' @param top_n Optional integer; if supplied, only the `top_n` variables (or
#'   categories) contributing the most to the plotted axes are drawn (passed
#'   to `factoextra` as `select.var = list(contrib = top_n)`).
#' @param ... Additional arguments passed to `factoextra::fviz_pca_biplot()`
#'   or `factoextra::fviz_mca_biplot()`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot_clusters()], [explore()] for the underlying embedding.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("factoextra", quietly = TRUE)) {
#'   fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#'   plot_biplot(fit)
#' }
plot_biplot <- function(x, data = NULL, embedding = c("auto", "pca", "mca"), top_n = NULL, ...) {
  UseMethod("plot_biplot")
}

#' @export
plot_biplot.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "mca"), top_n = NULL, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, top_n = top_n, ...)
}

#' @export
plot_biplot.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "mca"), top_n = NULL, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, top_n = top_n, ...)
}

#' @export
plot_biplot.cluster_explore <- function(x, data = NULL, embedding = c("auto", "pca", "mca"), top_n = NULL, ...) {
  if (!requireNamespace("factoextra", quietly = TRUE)) {
    stop("Package `factoextra` is required for `plot_biplot()`.", call. = FALSE)
  }
  if (is.null(x$embedding_fit) || !x$embedding_method %in% c("pca", "mca")) {
    stop(
      "Biplots require a `\"pca\"` or `\"mca\"` embedding; ",
      "the `\"", x$embedding_method, "\"` embedding is not supported ",
      "(factoextra has no equivalent biplot function).",
      call. = FALSE
    )
  }
  select_var <- if (is.null(top_n)) NULL else list(contrib = top_n)
  fviz_biplot <- switch(
    x$embedding_method,
    pca = factoextra::fviz_pca_biplot,
    mca = factoextra::fviz_mca_biplot
  )
  fviz_biplot(
    x$embedding_fit,
    habillage = x$plot_data$cluster,
    addEllipses = FALSE,
    repel = TRUE,
    select.var = select_var,
    title = "Cluster biplot",
    ...
  ) + theme_phynotype()
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
    theme_phynotype()
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
    theme_phynotype()
}
