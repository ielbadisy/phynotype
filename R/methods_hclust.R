#' Hierarchical clustering backend (hclust)
#'
#' Agglomerative hierarchical clustering builds a dendrogram by successively
#' merging the two clusters with the smallest inter-cluster distance. The
#' distance between clusters \eqn{A} and \eqn{B} depends on the chosen linkage:
#'
#' \describe{
#'   \item{Complete}{
#'     \eqn{d(A,B) = \max_{a \in A,\, b \in B} d(a,b).}
#'     Tends to produce compact, roughly equal-sized clusters.}
#'   \item{Single}{
#'     \eqn{d(A,B) = \min_{a \in A,\, b \in B} d(a,b).}
#'     Susceptible to chaining.}
#'   \item{Average (UPGMA)}{
#'     \eqn{d(A,B) = \frac{1}{|A||B|}\sum_{a \in A}\sum_{b \in B} d(a,b).}}
#'   \item{Ward}{
#'     Merge the pair that minimizes the increase in total within-cluster
#'     variance (Ward, 1963; Murtagh and Legendre, 2014).}
#' }
#'
#' The final partition is obtained by cutting the dendrogram at height
#' corresponding to \eqn{k} clusters.
#'
#' @references
#' Ward, J.H. (1963). Hierarchical grouping to optimize an objective function.
#' *Journal of the American Statistical Association*, **58**(301), 236--244.
#'
#' Murtagh, F. and Legendre, P. (2014). Ward's hierarchical agglomerative
#' clustering method: Which algorithms implement Ward's criterion? *Journal of
#' Classification*, **31**(3), 274--295.
#'
#' @noRd
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
  centers <- if (!inherits(data, "dist")) cluster_centroids(data, clusters) else NULL
  list(
    clusters = clusters,
    n_clusters = length(unique(clusters)),
    membership = NULL,
    centers = centers,
    prototypes = NULL,
    distance_info = list(metric = "euclidean", linkage = linkage),
    fitted_object = fitted,
    extras = list(cut_k = k, dist = dist_obj)
  )
}

predict_hclust <- function(object, new_data, ...) {
  predict_nearest_center(object, new_data, prediction_type = "nearest_centroid")
}

cluster_centroids <- function(data, clusters) {
  data <- as.matrix(data)
  cls <- sort(unique(clusters))
  centers <- do.call(rbind, lapply(cls, function(cl) {
    colMeans(data[clusters == cl, , drop = FALSE])
  }))
  rownames(centers) <- paste0("cluster_", cls)
  centers
}

predict_nearest_center <- function(object, new_data, prediction_type = "nearest_centroid") {
  if (is.null(object$centers)) {
    stop(
      "Prediction requires a hierarchical fit trained on row-by-feature data, not only a distance object.",
      call. = FALSE
    )
  }
  new_data <- restore_preprocessed_matrix(new_data, object$data_info)
  centers_mat <- object$centers
  dmat <- vapply(
    seq_len(nrow(centers_mat)),
    function(i) rowSums((new_data - matrix(centers_mat[i, ], nrow(new_data), ncol(new_data), byrow = TRUE))^2),
    numeric(nrow(new_data))
  )
  dmat <- as.matrix(dmat)
  if (ncol(dmat) != nrow(centers_mat) && nrow(dmat) == nrow(centers_mat)) {
    dmat <- t(dmat)
  }
  cls <- as.integer(sub("^cluster_", "", rownames(centers_mat)[max.col(-dmat)]))
  if (anyNA(cls)) {
    cls <- max.col(-dmat)
  }
  colnames(dmat) <- rownames(centers_mat)
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dmat),
    method = object$method,
    prediction_type = prediction_type
  )
}
