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

#' Plot a cluster biplot with variable loadings
#'
#' Project observations onto a two-dimensional PCA, FAMD, or MCA embedding,
#' color them by cluster assignment, and overlay arrows showing how each
#' original variable (or, for MCA, each category) relates to the embedding
#' axes. Not available for the `"mds"` embedding, which has no variable
#' space to project.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data,
#'   FAMD for mixed numeric/categorical data, and MCA for categorical data.
#'   `"pca"`, `"famd"`, and `"mca"` may be selected explicitly.
#' @param top_n Optional integer; if supplied, only the `top_n` variables
#'   (or categories) with the largest loading magnitude are drawn.
#' @param scale Numeric scaling factor applied to the loading arrows so they
#'   are visible alongside the observation cloud. Defaults to a magnitude
#'   that spans about 80% of the observation cloud's radius.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot_clusters()], [explore()] for the underlying embedding and
#'   loadings.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_biplot(fit)
plot_biplot <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca"), top_n = NULL, scale = NULL, ...) {
  UseMethod("plot_biplot")
}

#' @export
plot_biplot.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca"), top_n = NULL, scale = NULL, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, top_n = top_n, scale = scale)
}

#' @export
plot_biplot.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca"), top_n = NULL, scale = NULL, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, top_n = top_n, scale = scale)
}

#' @export
plot_biplot.cluster_explore <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca"), top_n = NULL, scale = NULL, ...) {
  if (is.null(x$embedding_loadings)) {
    stop(
      "Biplots require a `\"pca\"`, `\"famd\"`, or `\"mca\"` embedding; ",
      "the `\"", x$embedding_method, "\"` embedding has no variable loadings.",
      call. = FALSE
    )
  }
  labels <- x$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }
  loadings <- x$embedding_loadings
  if (!is.null(top_n)) {
    magnitude <- sqrt(loadings$x^2 + loadings$y^2)
    loadings <- loadings[order(magnitude, decreasing = TRUE)[seq_len(min(top_n, nrow(loadings)))], , drop = FALSE]
  }
  if (is.null(scale)) {
    point_radius <- max(sqrt(x$plot_data$x^2 + x$plot_data$y^2), na.rm = TRUE)
    loading_radius <- max(sqrt(loadings$x^2 + loadings$y^2), na.rm = TRUE)
    scale <- if (is.finite(loading_radius) && loading_radius > 0) {
      0.8 * point_radius / loading_radius
    } else {
      1
    }
  }
  loadings$x <- loadings$x * scale
  loadings$y <- loadings$y * scale

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = x$plot_data,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]]),
      size = 2, alpha = 0.7
    ) +
    ggplot2::geom_segment(
      data = loadings,
      ggplot2::aes(x = 0, y = 0, xend = .data[["x"]], yend = .data[["y"]]),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
      color = "grey30"
    ) +
    ggplot2::geom_text(
      data = loadings,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["variable"]]),
      color = "grey20", vjust = -0.4, size = 3
    ) +
    ggplot2::labs(title = "Cluster biplot", x = labels$x, y = labels$y, color = "Cluster") +
    ggplot2::theme_minimal()
}

#' Plot a network graph of cluster relationships
#'
#' Summarize a clustering solution as a network: one node per cluster,
#' positioned at its centroid in the two-dimensional embedding and sized by
#' cluster size, connected by edges whose thickness reflects how similar
#' (close) each pair of cluster centroids is. This gives a compact overview
#' of how many well-separated groups exist versus how many clusters sit close
#' together, which is easy to miss on a dense point-cloud scatter plot.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data,
#'   FAMD for mixed numeric/categorical data, MCA for categorical data, and
#'   classical MDS for distance inputs. `"pca"`, `"famd"`, `"mca"`, and
#'   `"mds"` may be selected explicitly.
#' @param min_similarity Numeric threshold in `[0, 1]`; edges with similarity
#'   below this value are omitted. Similarity is `1` for coincident centroids
#'   and `0` for the two most distant centroids in the solution. Defaults to
#'   `0` (draw every edge).
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [explore()] for the underlying embedding, [plot_clusters()],
#'   [plot_coassoc()] for the observation-level co-association network.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_cluster_network(fit)
plot_cluster_network <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), min_similarity = 0, ...) {
  UseMethod("plot_cluster_network")
}

#' @export
plot_cluster_network.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), min_similarity = 0, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_cluster_network(exp, min_similarity = min_similarity)
}

#' @export
plot_cluster_network.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), min_similarity = 0, ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_cluster_network(exp, min_similarity = min_similarity)
}

#' @export
plot_cluster_network.cluster_explore <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), min_similarity = 0, ...) {
  if (!is.numeric(min_similarity) || length(min_similarity) != 1L || is.na(min_similarity) ||
      min_similarity < 0 || min_similarity > 1) {
    stop("`min_similarity` must be a single number in [0, 1].", call. = FALSE)
  }
  centroids <- stats::aggregate(x$plot_data[c("x", "y")], by = list(cluster = x$plot_data$cluster), FUN = mean)
  nodes <- merge(centroids, x$size_table, by = "cluster")

  n <- nrow(nodes)
  if (n < 2L) {
    edges <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0), similarity = numeric(0))
  } else {
    pairs <- utils::combn(n, 2)
    dists <- vapply(seq_len(ncol(pairs)), function(i) {
      a <- nodes[pairs[1, i], c("x", "y")]
      b <- nodes[pairs[2, i], c("x", "y")]
      sqrt(sum((a - b)^2))
    }, numeric(1))
    max_dist <- max(dists)
    similarity <- if (max_dist > 0) 1 - dists / max_dist else rep(1, length(dists))
    edges <- data.frame(
      x = nodes$x[pairs[1, ]],
      y = nodes$y[pairs[1, ]],
      xend = nodes$x[pairs[2, ]],
      yend = nodes$y[pairs[2, ]],
      similarity = similarity
    )
    edges <- edges[edges$similarity >= min_similarity, , drop = FALSE]
  }

  labels <- x$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }

  p <- ggplot2::ggplot()
  if (nrow(edges) > 0L) {
    p <- p + ggplot2::geom_segment(
      data = edges,
      ggplot2::aes(
        x = .data[["x"]], y = .data[["y"]],
        xend = .data[["xend"]], yend = .data[["yend"]],
        alpha = .data[["similarity"]], linewidth = .data[["similarity"]]
      ),
      color = "grey40", show.legend = FALSE
    ) +
      ggplot2::scale_linewidth(range = c(0.2, 2))
  }
  p +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], size = .data[["size"]], color = factor(.data[["cluster"]]))
    ) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["cluster"]]),
      color = "white", fontface = "bold"
    ) +
    ggplot2::labs(
      title = "Cluster network", x = labels$x, y = labels$y,
      color = "Cluster", size = "Cluster size"
    ) +
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
