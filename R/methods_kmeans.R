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
