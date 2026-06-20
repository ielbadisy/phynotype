#' K-means clustering backend
#'
#' Lloyd's algorithm minimizes the total within-cluster sum of squares
#'
#' \deqn{
#'   \min_{\mathcal{C}} \sum_{j=1}^{k} \sum_{x_i \in C_j}
#'   \|x_i - \mu_j\|^2,
#' }
#'
#' where \eqn{\mu_j = |C_j|^{-1} \sum_{x_i \in C_j} x_i} is the centroid of
#' cluster \eqn{j}. The algorithm alternates between assigning each observation
#' to its nearest centroid and recomputing centroids until convergence (Lloyd,
#' 1982). Multiple random restarts (`nstart`) are used to reduce sensitivity
#' to initialization.
#'
#' @references
#' Lloyd, S.P. (1982). Least squares quantization in PCM. *IEEE Transactions
#' on Information Theory*, **28**(2), 129--137.
#'
#' MacQueen, J. (1967). Some methods for classification and analysis of
#' multivariate observations. *Proceedings of the 5th Berkeley Symposium on
#' Mathematical Statistics and Probability*, Vol. 1, pp. 281--297.
#'
#' @noRd
validate_kmeans_params <- function(data, params) {
  k <- check_required_k(params$k)
  if (!is.matrix(data)) {
    stop("`kmeans` requires a numeric matrix or data frame input.", call. = FALSE)
  }
  if (k > nrow(data)) {
    stop("`k` cannot exceed the number of observations.", call. = FALSE)
  }
}

fit_kmeans <- function(data, params) {
  k <- as.integer(params$k)
  nstart <- if (is.null(params$nstart)) 10L else as.integer(params$nstart)
  iter.max <- if (is.null(params$iter.max)) 100L else as.integer(params$iter.max)
  fitted <- stats::kmeans(data, centers = k, nstart = nstart, iter.max = iter.max)
  list(
    clusters = fitted$cluster,
    n_clusters = length(unique(fitted$cluster)),
    membership = NULL,
    centers = fitted$centers,
    prototypes = NULL,
    distance_info = list(metric = "euclidean"),
    fitted_object = fitted,
    extras = list(
      tot_withinss = fitted$tot.withinss,
      withinss = fitted$withinss
    )
  )
}

predict_kmeans <- function(object, new_data, ...) {
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
  cls <- max.col(-dmat)
  center_names <- rownames(centers_mat)
  if (is.null(center_names) || length(center_names) != nrow(centers_mat)) {
    center_names <- as.character(seq_len(nrow(centers_mat)))
  }
  colnames(dmat) <- center_names
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dmat),
    method = object$method,
    prediction_type = "native"
  )
}
