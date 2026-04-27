#' Validate clustering results
#'
#' Compute internal validation metrics for a fitted clustering object.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or numeric matrix/data frame.
#' @param ... Additional arguments passed to methods.
#' @param method Clustering method for direct grid validation.
#' @param k Candidate values of `k` for direct validation.
#' @param truth Optional reference labels for external metrics.
#' @param metrics Optional character vector of metrics.
#' @param n_boot Number of bootstrap resamples for stability.
#'
#' @return A `cluster_validation` object.
#' @export
validate <- function(x,
                     ...,
                     method = "kmeans",
                     k = NULL,
                     truth = NULL,
                     metrics = NULL,
                     n_boot = 10) {
  UseMethod("validate")
}

#' @export
validate.cluster_fit <- function(x, ..., truth = NULL, metrics = NULL, n_boot = 10) {
  if (x$method == "pam" && !requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required to compute silhouette for `pam` fits.", call. = FALSE)
  }
  data <- x$data_info$original_data
  if (inherits(data, "dist")) {
    stop("Validation for distance-only inputs is not yet implemented.", call. = FALSE)
  }
  metrics_table <- data.frame(
    metric = c("silhouette", "calinski_harabasz", "davies_bouldin", "total_within"),
    value = c(
      if (requireNamespace("cluster", quietly = TRUE)) compute_silhouette_metric(data, x$clusters) else NA_real_,
      compute_calinski_harabasz(data, x$clusters),
      compute_davies_bouldin(data, x$clusters),
      compute_total_within(data, x$clusters)
    )
  )
  if (!is.null(truth)) {
    metrics_table <- rbind(
      metrics_table,
      data.frame(
        metric = c("ari", "nmi"),
        value = c(adjusted_rand_index(truth, x$clusters), normalized_mutual_information(truth, x$clusters))
      )
    )
  }
  stability <- if (x$method %in% c("kmeans", "pam", "gmm") && !is.null(x$params$k)) {
    bootstrap_cluster_stability(data, method = x$method, k = x$params$k, n_boot = n_boot, seed = x$params$seed)
  } else {
    NULL
  }
  if (!is.null(stability)) {
    metrics_table <- rbind(
      metrics_table,
      data.frame(metric = "bootstrap_ari", value = stability$mean)
    )
  }
  if (!is.null(metrics)) {
    metrics_table <- metrics_table[metrics_table$metric %in% metrics, , drop = FALSE]
  }
  new_cluster_validation(
    metrics_table = metrics_table,
    per_cluster_table = compute_per_cluster_silhouette(data, x$clusters),
    settings = list(method = x$method, n_boot = n_boot),
    object_type = class(x)[1],
    extras = list(stability = stability)
  )
}

#' @export
validate.metacluster_fit <- function(x, ..., truth = NULL, metrics = NULL, n_boot = 10) {
  data <- x$data_info$original_data
  metrics_table <- data.frame(
    metric = c("silhouette", "calinski_harabasz", "davies_bouldin"),
    value = c(
      if (requireNamespace("cluster", quietly = TRUE)) compute_silhouette_metric(data, x$final_clusters) else NA_real_,
      compute_calinski_harabasz(data, x$final_clusters),
      compute_davies_bouldin(data, x$final_clusters)
    )
  )
  if (!is.null(truth)) {
    metrics_table <- rbind(
      metrics_table,
      data.frame(
        metric = c("ari", "nmi"),
        value = c(adjusted_rand_index(truth, x$final_clusters), normalized_mutual_information(truth, x$final_clusters))
      )
    )
  }
  if (!is.null(x$stability_summary)) {
    metrics_table <- rbind(
      metrics_table,
      data.frame(metric = "pairwise_partition_agreement", value = x$stability_summary$mean_agreement)
    )
  }
  if (!is.null(metrics)) {
    metrics_table <- metrics_table[metrics_table$metric %in% metrics, , drop = FALSE]
  }
  new_cluster_validation(
    metrics_table = metrics_table,
    per_cluster_table = compute_per_cluster_silhouette(data, x$final_clusters),
    settings = list(method = "metacluster", n_boot = n_boot),
    object_type = class(x)[1],
    extras = list(selection_summary = x$selection_summary)
  )
}

#' @export
validate.default <- function(x,
                             ...,
                             method = "kmeans",
                             k = NULL,
                             truth = NULL,
                             metrics = NULL,
                             n_boot = 10) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Direct validation requires a numeric matrix or data frame.", call. = FALSE)
  }
  k_values <- check_k_grid(k)
  rows <- lapply(k_values, function(k_i) {
    fit <- cluster(x, method = method, k = k_i, ...)
    val <- validate(fit, truth = truth, metrics = metrics, n_boot = n_boot)
    out <- val$metrics_table
    out$k <- k_i
    out
  })
  new_cluster_validation(
    metrics_table = do.call(rbind, rows),
    settings = list(method = method, grid_k = k_values),
    object_type = "validation_grid"
  )
}
