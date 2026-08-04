#' Explore clustering structure
#'
#' Summarize cluster sizes, feature profiles, feature separation, and a
#' two-dimensional embedding for a fitted clustering solution.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param data Optional numeric matrix, data frame, or distance object.
#'   Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` chooses PCA for numeric data,
#'   FAMD for mixed numeric/categorical data, MCA for categorical data, and
#'   classical MDS for distance objects. Explicit values `"pca"`, `"famd"`,
#'   `"mca"`, and `"mds"` are also supported.
#' @param ... Unused.
#'
#' @details
#' `explore()` computes four summaries:
#' \describe{
#'   \item{Size table}{Number of observations per cluster.}
#'   \item{Feature summary}{Per-cluster mean, standard deviation, median, min,
#'     and max for each feature.}
#'   \item{Separation table}{The eta-squared statistic for each feature,
#'     \eqn{\eta^2_j = \mathrm{SS}_{B,j} / \mathrm{SS}_{T,j}}, measuring
#'     how much between-cluster variance each feature explains.}
#'   \item{Embedding}{Two-dimensional projection for visualization
#'     (see [plot_clusters()]).}
#' }
#'
#' @return A `cluster_explore` object with components:
#' \describe{
#'   \item{`size_table`}{Data frame with cluster sizes.}
#'   \item{`feature_summary`}{Data frame with per-cluster descriptive
#'     statistics for each feature.}
#'   \item{`separation_table`}{Data frame with per-feature eta-squared
#'     values.}
#'   \item{`embedding`}{Data frame with two embedding coordinates and cluster
#'     labels.}
#' }
#'
#' @seealso [cluster()] to fit, [plot_clusters()] to plot the embedding,
#'   [plot_feature_profiles()] to plot feature profiles.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' exp <- explore(fit)
#' exp$size_table
#' head(exp$feature_summary)
#' exp$separation_table
explore <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  UseMethod("explore")
}

#' @export
explore.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  if (is.null(data)) {
    data <- x$data_info$original_data
  }
  cluster_factor <- factor(x$clusters)
  size_table <- data.frame(cluster = levels(cluster_factor), size = as.integer(table(cluster_factor)))
  if (inherits(data, "dist")) {
    feature_summary <- NULL
    separation_table <- NULL
  } else {
    summary_data <- if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1)))) {
      prepare_mixed_data(data, scale = TRUE)
    } else if (is.data.frame(data)) {
      as.matrix(data)
    } else {
      data
    }
    feature_summary <- build_feature_summary(summary_data, x$clusters)
    separation_table <- compute_separation_table(summary_data, x$clusters)
  }
  embedding <- compute_embedding(data, x$clusters, method = embedding, data_info = x$data_info)
  prototype_table <- if (!is.null(x$prototypes)) as.data.frame(x$prototypes) else if (!is.null(x$centers)) as.data.frame(x$centers) else NULL
  new_cluster_explore(
    size_table = size_table,
    feature_summary = feature_summary,
    separation_table = separation_table,
    prototype_table = prototype_table,
    embedding = embedding$data,
    plot_data = embedding$data,
    embedding_method = embedding$method,
    embedding_labels = embedding$labels
  )
}

#' @export
explore.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  if (is.null(data)) {
    data <- x$data_info$original_data
  }
  cluster_factor <- factor(x$final_clusters)
  size_table <- data.frame(cluster = levels(cluster_factor), size = as.integer(table(cluster_factor)))
  if (inherits(data, "dist")) {
    feature_summary <- NULL
    separation_table <- NULL
  } else {
    summary_data <- if (is.data.frame(data) && !all(vapply(data, is.numeric, logical(1)))) {
      prepare_mixed_data(data, scale = TRUE)
    } else if (is.data.frame(data)) {
      as.matrix(data)
    } else {
      data
    }
    feature_summary <- build_feature_summary(summary_data, x$final_clusters)
    separation_table <- compute_separation_table(summary_data, x$final_clusters)
  }
  embedding <- compute_embedding(data, x$final_clusters, method = embedding, data_info = x$data_info)
  new_cluster_explore(
    size_table = size_table,
    feature_summary = feature_summary,
    separation_table = separation_table,
    prototype_table = NULL,
    embedding = embedding$data,
    plot_data = embedding$data,
    embedding_method = embedding$method,
    embedding_labels = embedding$labels
  )
}
