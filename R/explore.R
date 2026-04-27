#' Explore clustering structure
#'
#' Summarize cluster sizes, feature profiles, separation, and a PCA embedding.
#'
#' @param x A `cluster_fit` object.
#' @param data Optional numeric matrix or data frame. Defaults to training data.
#' @param ... Unused.
#'
#' @return A `cluster_explore` object.
#' @export
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
