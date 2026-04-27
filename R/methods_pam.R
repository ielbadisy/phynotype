validate_pam_params <- function(data, params) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for method `pam`.", call. = FALSE)
  }
  k <- check_required_k(params$k)
  if (!is.matrix(data)) {
    stop("`pam` requires a numeric matrix or data frame input.", call. = FALSE)
  }
  if (k > nrow(data)) {
    stop("`k` cannot exceed the number of observations.", call. = FALSE)
  }
}

fit_pam <- function(data, params) {
  fitted <- cluster::pam(data, k = as.integer(params$k))
  medoid_ids <- fitted$id.med
  prototypes <- data[medoid_ids, , drop = FALSE]
  rownames(prototypes) <- paste0("cluster_", seq_len(nrow(prototypes)))
  list(
    clusters = fitted$clustering,
    n_clusters = length(unique(fitted$clustering)),
    membership = NULL,
    centers = NULL,
    prototypes = prototypes,
    distance_info = list(metric = "euclidean"),
    fitted_object = fitted,
    extras = list(medoid_ids = medoid_ids)
  )
}

predict_pam <- function(object, new_data, ...) {
  new_data <- restore_preprocessed_matrix(new_data, object$data_info)
  medoids <- object$prototypes
  dmat <- vapply(
    seq_len(nrow(medoids)),
    function(i) rowSums((new_data - matrix(medoids[i, ], nrow(new_data), ncol(new_data), byrow = TRUE))^2),
    numeric(nrow(new_data))
  )
  dmat <- as.matrix(dmat)
  cls <- max.col(-dmat)
  colnames(dmat) <- rownames(medoids)
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dmat),
    method = object$method,
    prediction_type = "native"
  )
}
