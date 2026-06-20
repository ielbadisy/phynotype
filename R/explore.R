#' Explore clustering structure
#'
#' Summarize cluster sizes, feature profiles, feature separation, and a PCA
#' embedding for a fitted clustering solution.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param data Optional numeric matrix or data frame. Defaults to the training
#'   data stored in `x`.
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
#'   \item{PCA embedding}{Two-dimensional PCA projection for visualization
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
#'   \item{`embedding`}{Data frame with two PCA coordinates and cluster
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
explore <- function(x, data = NULL, ...) {
  UseMethod("explore")
}

#' @export
explore.cluster_fit <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    data <- x$data_info$original_data
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  cluster_factor <- factor(x$clusters)
  size_table <- data.frame(cluster = levels(cluster_factor), size = as.integer(table(cluster_factor)))
  feature_summary <- build_feature_summary(data, x$clusters)
  separation_table <- compute_separation_table(data, x$clusters)
  embedding <- compute_pca_embedding(data, x$clusters)
  plot_data <- embedding
  prototype_table <- if (!is.null(x$prototypes)) as.data.frame(x$prototypes) else if (!is.null(x$centers)) as.data.frame(x$centers) else NULL
  new_cluster_explore(
    size_table = size_table,
    feature_summary = feature_summary,
    separation_table = separation_table,
    prototype_table = prototype_table,
    embedding = embedding,
    plot_data = plot_data
  )
}

#' @export
explore.metacluster_fit <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    data <- x$data_info$original_data
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  cluster_factor <- factor(x$final_clusters)
  size_table <- data.frame(cluster = levels(cluster_factor), size = as.integer(table(cluster_factor)))
  feature_summary <- build_feature_summary(data, x$final_clusters)
  separation_table <- compute_separation_table(data, x$final_clusters)
  embedding <- compute_pca_embedding(data, x$final_clusters)
  new_cluster_explore(
    size_table = size_table,
    feature_summary = feature_summary,
    separation_table = separation_table,
    prototype_table = NULL,
    embedding = embedding,
    plot_data = embedding
  )
}
