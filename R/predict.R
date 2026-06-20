#' Predict cluster assignments for new observations
#'
#' Assign new observations to the clusters of a fitted `cluster_fit` object.
#' The prediction strategy is method-dependent: k-means, PAM, and hierarchical
#' methods use nearest-centroid or nearest-prototype rules; GMM uses the
#' posterior mode; DBSCAN uses a density-based rule falling back to the nearest
#' core point.
#'
#' @param object A `cluster_fit` object. Must have been fitted on
#'   row-by-feature data (not a `dist`-only input).
#' @param new_data Row-by-feature data compatible with the training data.
#'   Numeric methods expect a numeric matrix or data frame; `"kproto"` and
#'   `"kmm"` accept mixed-type data frames with the same column names used
#'   during training.
#' @param ... Additional arguments passed to method-specific predictors.
#'
#' @return A `cluster_prediction` object with components:
#' \describe{
#'   \item{`clusters`}{Integer vector of predicted cluster assignments.}
#'   \item{`distances`}{Distance matrix to each cluster center/prototype,
#'     or `NULL` for methods that do not produce distances.}
#'   \item{`membership`}{Soft membership matrix (GMM only), or `NULL`.}
#' }
#'
#' @seealso [cluster()] to fit, [feature_importance()], [lime_explain()],
#'   and [ceteris_paribus()] which all use `predict()` internally.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#'
#' # Predict on the training data
#' pred <- predict(fit, iris[1:10, 1:4])
#' pred$clusters
#'
#' # Predict on new data
#' new_obs <- data.frame(
#'   Sepal.Length = c(5.0, 7.0),
#'   Sepal.Width  = c(3.5, 3.0),
#'   Petal.Length = c(1.5, 5.0),
#'   Petal.Width  = c(0.3, 1.8)
#' )
#' predict(fit, new_obs)$clusters
predict.cluster_fit <- function(object, new_data, ...) {
  registry <- get_cluster_registry()
  entry <- registry[[object$method]]
  if (is.null(entry) || is.null(entry$predict)) {
    stop("Prediction is not supported for method `", object$method, "`.", call. = FALSE)
  }
  entry$predict(object, new_data = new_data, ...)
}
