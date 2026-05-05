validate_agnes_params <- function(data, params) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for method `agnes`.", call. = FALSE)
  }
  check_required_k(params$k)
  if (!inherits(data, "dist") && !is.matrix(data)) {
    stop("`agnes` requires a numeric matrix, data frame, or distance input.", call. = FALSE)
  }
}

fit_agnes <- function(data, params) {
  method <- if (is.null(params$linkage)) "average" else params$linkage
  fitted <- cluster::agnes(data, method = method, diss = inherits(data, "dist"))
  hc <- stats::as.hclust(fitted)
  k <- as.integer(params$k)
  cls <- stats::cutree(hc, k = k)
  centers <- if (!inherits(data, "dist")) cluster_centroids(data, cls) else NULL
  list(
    clusters = cls,
    n_clusters = length(unique(cls)),
    membership = NULL,
    centers = centers,
    prototypes = NULL,
    distance_info = list(metric = "euclidean", linkage = method),
    fitted_object = fitted,
    extras = list(cut_k = k, hclust = hc)
  )
}

predict_agnes <- function(object, new_data, ...) {
  predict_nearest_center(object, new_data, prediction_type = "nearest_centroid")
}
