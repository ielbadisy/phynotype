validate_gmm_params <- function(data, params) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package `mclust` is required for method `gmm`.", call. = FALSE)
  }
  k <- check_required_k(params$k)
  if (!is.matrix(data)) {
    stop("`gmm` requires a numeric matrix or data frame input.", call. = FALSE)
  }
  if (k > nrow(data)) {
    stop("`k` cannot exceed the number of observations.", call. = FALSE)
  }
}

fit_gmm <- function(data, params) {
  attached <- "package:mclust" %in% search()
  if (!attached) {
    suppressPackageStartupMessages(base::library("mclust", character.only = TRUE))
    on.exit(detach("package:mclust", unload = FALSE, character.only = TRUE), add = TRUE)
  }
  fitted <- mclust::Mclust(data = data, G = as.integer(params$k))
  list(
    clusters = fitted$classification,
    n_clusters = length(unique(fitted$classification)),
    membership = fitted$z,
    centers = fitted$parameters$mean,
    prototypes = NULL,
    distance_info = list(metric = "model-based"),
    fitted_object = fitted,
    extras = list(model_name = fitted$modelName)
  )
}

predict_gmm <- function(object, new_data, ...) {
  new_data <- restore_preprocessed_matrix(new_data, object$data_info)
  attached <- "package:mclust" %in% search()
  if (!attached) {
    suppressPackageStartupMessages(base::library("mclust", character.only = TRUE))
    on.exit(detach("package:mclust", unload = FALSE, character.only = TRUE), add = TRUE)
  }
  pred <- stats::predict(object$fitted_object, newdata = new_data)
  new_cluster_prediction(
    clusters = pred$classification,
    membership = pred$z,
    distances = NULL,
    method = object$method,
    prediction_type = "native"
  )
}
