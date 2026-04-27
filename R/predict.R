#' Predict cluster assignments for new observations
#'
#' @param object A `cluster_fit` object.
#' @param new_data Numeric matrix or numeric data frame.
#' @param ... Additional arguments passed to method-specific predictors.
#'
#' @return A `cluster_prediction` object.
#' @export
predict.cluster_fit <- function(object, new_data, ...) {
  registry <- get_cluster_registry()
  entry <- registry[[object$method]]
  if (is.null(entry) || is.null(entry$predict)) {
    stop("Prediction is not supported for method `", object$method, "`.", call. = FALSE)
  }
  entry$predict(object, new_data = new_data, ...)
}
