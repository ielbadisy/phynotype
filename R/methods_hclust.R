validate_hclust_params <- function(data, params) {
  check_required_k(params$k)
  method <- if (is.null(params$linkage)) "complete" else params$linkage
  allowed <- c("complete", "single", "average", "ward.D", "ward.D2", "mcquitty", "median", "centroid")
  if (!method %in% allowed) {
    stop("Unsupported `linkage` for `hclust`.", call. = FALSE)
  }
}

fit_hclust <- function(data, params) {
  linkage <- if (is.null(params$linkage)) "complete" else params$linkage
  dist_obj <- if (inherits(data, "dist")) data else stats::dist(data)
  fitted <- stats::hclust(dist_obj, method = linkage)
  k <- as.integer(params$k)
  clusters <- stats::cutree(fitted, k = k)
  list(
    clusters = clusters,
    n_clusters = length(unique(clusters)),
    membership = NULL,
    centers = NULL,
    prototypes = NULL,
    distance_info = list(metric = "euclidean", linkage = linkage),
    fitted_object = fitted,
    extras = list(cut_k = k, dist = dist_obj)
  )
}

predict_hclust <- function(object, new_data, ...) {
  stop("Prediction for hierarchical clustering is not natively supported.", call. = FALSE)
}
