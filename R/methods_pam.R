#' Partitioning Around Medoids (PAM) backend
#'
#' PAM selects \eqn{k} actual observations (medoids) \eqn{m_1, \ldots, m_k}
#' that minimize the total dissimilarity to the nearest medoid:
#'
#' \deqn{
#'   \min_{m_1,\ldots,m_k \in \mathcal{X}}
#'   \sum_{i=1}^{n} \min_{g \in \{1,\ldots,k\}} d(x_i, m_g),
#' }
#'
#' where \eqn{d(\cdot,\cdot)} is typically the Euclidean distance. Unlike
#' k-means, medoids are constrained to lie on observed data points, making PAM
#' more robust to outliers (Kaufman and Rousseeuw, 1990).
#'
#' @references
#' Kaufman, L. and Rousseeuw, P.J. (1990). *Finding Groups in Data: An
#' Introduction to Cluster Analysis*. John Wiley & Sons, New York.
#'
#' @noRd
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
  if (ncol(dmat) != nrow(medoids) && nrow(dmat) == nrow(medoids)) {
    dmat <- t(dmat)
  }
  cls <- max.col(-dmat)
  medoid_names <- rownames(medoids)
  if (is.null(medoid_names) || length(medoid_names) != nrow(medoids)) {
    medoid_names <- as.character(seq_len(nrow(medoids)))
  }
  colnames(dmat) <- medoid_names
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dmat),
    method = object$method,
    prediction_type = "native"
  )
}
