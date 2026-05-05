validate_dbscan_params <- function(data, params) {
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package `dbscan` is required for method `dbscan`.", call. = FALSE)
  }
  if (!is.matrix(data)) {
    stop("`dbscan` requires a numeric matrix or data frame input.", call. = FALSE)
  }
  eps <- params$eps
  min_pts <- if (is.null(params$minPts)) params$min_pts else params$minPts
  if (is.null(eps) || !is.numeric(eps) || length(eps) != 1L || eps <= 0) {
    stop("`eps` must be a single positive number for `dbscan`.", call. = FALSE)
  }
  if (is.null(min_pts) || !is.numeric(min_pts) || length(min_pts) != 1L || min_pts < 1) {
    stop("`minPts` must be a single positive integer for `dbscan`.", call. = FALSE)
  }
}

fit_dbscan <- function(data, params) {
  min_pts <- if (is.null(params$minPts)) params$min_pts else params$minPts
  fitted <- dbscan::dbscan(data, eps = params$eps, minPts = as.integer(min_pts))
  cls <- fitted$cluster
  core_ids <- if (!is.null(fitted$isseed)) which(fitted$isseed) else integer(0)
  list(
    clusters = cls,
    n_clusters = length(setdiff(unique(cls), 0L)),
    membership = NULL,
    centers = NULL,
    prototypes = NULL,
    distance_info = list(metric = "euclidean"),
    fitted_object = fitted,
    extras = list(noise_label = 0L, core_points = core_ids)
  )
}

predict_dbscan <- function(object, new_data, ...) {
  new_data <- restore_preprocessed_matrix(new_data, object$data_info)
  training <- restore_preprocessed_matrix(object$data_info$original_data, object$data_info)
  fitted <- object$fitted_object
  pred <- tryCatch(
    stats::predict(fitted, newdata = new_data, data = training),
    error = function(e) NULL
  )
  prediction_type <- "native"
  if (is.null(pred)) {
    pred <- predict_dbscan_nearest(object, new_data, training = training)
    prediction_type <- "nearest_non_noise"
  }
  new_cluster_prediction(
    clusters = pred,
    membership = NULL,
    distances = NULL,
    method = object$method,
    prediction_type = prediction_type
  )
}

predict_dbscan_nearest <- function(object, new_data, training) {
  labels <- object$clusters
  keep <- labels != object$extras$noise_label
  if (!any(keep)) {
    return(rep(object$extras$noise_label, nrow(new_data)))
  }
  training <- training[keep, , drop = FALSE]
  labels <- labels[keep]
  dmat <- vapply(
    seq_len(nrow(training)),
    function(i) rowSums((new_data - matrix(training[i, ], nrow(new_data), ncol(new_data), byrow = TRUE))^2),
    numeric(nrow(new_data))
  )
  dmat <- sqrt(as.matrix(dmat))
  nearest <- max.col(-dmat)
  nearest_dist <- dmat[cbind(seq_len(nrow(dmat)), nearest)]
  out <- labels[nearest]
  out[nearest_dist > object$fitted_object$eps] <- object$extras$noise_label
  out
}
