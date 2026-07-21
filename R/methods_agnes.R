#' AGNES agglomerative nesting backend
#'
#' AGNES (AGglomerative NESting) is an agglomerative hierarchical clustering
#' method implemented in the \pkg{cluster} package (Kaufman and Rousseeuw,
#' 1990). It proceeds identically to `hclust`, successively merging the two
#' clusters \eqn{A} and \eqn{B} with the smallest inter-cluster distance:
#'
#' \deqn{
#'   d(A, B) = f\!\left(\{d(a, b) : a \in A,\, b \in B\}\right),
#' }
#'
#' where \eqn{f} is the linkage function (default `"average"` for AGNES,
#' giving UPGMA). AGNES additionally reports the agglomerative coefficient,
#' which measures the strength of the clustering structure (values near 1
#' indicate well-defined clusters).
#'
#' @references
#' Kaufman, L. and Rousseeuw, P.J. (1990). *Finding Groups in Data: An
#' Introduction to Cluster Analysis*. John Wiley & Sons, New York.
#'
#' @noRd
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
