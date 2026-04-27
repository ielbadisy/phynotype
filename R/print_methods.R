#' @export
print.cluster_fit <- function(x, ...) {
  cat("<cluster_fit>\n")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  Observations: ", x$data_info$n_obs, "\n", sep = "")
  cat("  Clusters: ", x$n_clusters, "\n", sep = "")
  invisible(x)
}

#' @export
print.metacluster_fit <- function(x, ...) {
  cat("<metacluster_fit>\n")
  cat("  Methods: ", paste(unique(x$candidate_table$method), collapse = ", "), "\n", sep = "")
  cat("  Candidate fits: ", nrow(x$candidate_table), "\n", sep = "")
  cat("  Final clusters: ", x$final_k, "\n", sep = "")
  invisible(x)
}

#' @export
summary.cluster_fit <- function(object, ...) {
  size_vec <- sizes(object)
  out <- list(
    method = object$method,
    n_obs = object$data_info$n_obs,
    n_clusters = object$n_clusters,
    sizes = size_vec,
    has_centers = !is.null(object$centers),
    has_prototypes = !is.null(object$prototypes)
  )
  class(out) <- "summary.cluster_fit"
  out
}

#' @export
summary.metacluster_fit <- function(object, ...) {
  out <- list(
    methods = unique(object$candidate_table$method),
    candidate_fits = nrow(object$candidate_table),
    final_k = object$final_k,
    sizes = sizes(object),
    selection_summary = object$selection_summary
  )
  class(out) <- "summary.metacluster_fit"
  out
}

#' @export
print.summary.cluster_fit <- function(x, ...) {
  cat("Cluster fit summary\n")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  Observations: ", x$n_obs, "\n", sep = "")
  cat("  Clusters: ", x$n_clusters, "\n", sep = "")
  cat("  Sizes: ", paste(names(x$sizes), x$sizes, sep = "=", collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.summary.metacluster_fit <- function(x, ...) {
  cat("Meta-cluster summary\n")
  cat("  Methods: ", paste(x$methods, collapse = ", "), "\n", sep = "")
  cat("  Candidate fits: ", x$candidate_fits, "\n", sep = "")
  cat("  Final clusters: ", x$final_k, "\n", sep = "")
  cat("  Sizes: ", paste(names(x$sizes), x$sizes, sep = "=", collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.cluster_validation <- function(x, ...) {
  cat("<cluster_validation>\n")
  cat("  Object type: ", x$object_type, "\n", sep = "")
  cat("  Metrics: ", nrow(x$metrics_table), "\n", sep = "")
  invisible(x)
}

#' @export
summary.cluster_validation <- function(object, ...) {
  object
}

#' @export
print.cluster_explore <- function(x, ...) {
  cat("<cluster_explore>\n")
  cat("  Rows in feature summary: ", nrow(x$feature_summary), "\n", sep = "")
  invisible(x)
}

#' @export
print.cluster_prediction <- function(x, ...) {
  cat("<cluster_prediction>\n")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  Predictions: ", length(x$clusters), "\n", sep = "")
  invisible(x)
}
