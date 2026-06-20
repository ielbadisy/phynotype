#' Gaussian Mixture Model clustering backend
#'
#' A Gaussian mixture model (GMM) represents the data density as
#'
#' \deqn{
#'   p(x) = \sum_{g=1}^{k} \pi_g \,\mathcal{N}(x \mid \mu_g, \Sigma_g),
#' }
#'
#' where \eqn{\pi_g > 0} are mixing weights summing to 1, and
#' \eqn{\mathcal{N}(\cdot \mid \mu_g, \Sigma_g)} is the multivariate Gaussian
#' density with mean \eqn{\mu_g} and covariance \eqn{\Sigma_g} (Fraley and
#' Raftery, 2002). Parameters are estimated by the Expectation-Maximization
#' (EM) algorithm. Hard cluster assignments are obtained by the maximum a
#' posteriori rule:
#'
#' \deqn{
#'   \hat{y}_i = \operatorname*{arg\,max}_{g}
#'   \pi_g \,\mathcal{N}(x_i \mid \mu_g, \Sigma_g).
#' }
#'
#' Soft membership probabilities (posterior responsibilities) are stored in the
#' `membership` slot. Model selection across covariance structures is handled
#' by `mclust::Mclust()` via BIC.
#'
#' @references
#' Fraley, C. and Raftery, A.E. (2002). Model-based clustering, discriminant
#' analysis, and density estimation. *Journal of the American Statistical
#' Association*, **97**(458), 611--631.
#'
#' Fraley, C. and Raftery, A.E. (1999). MCLUST: Software for model-based
#' cluster analysis. *Journal of Classification*, **16**(2), 297--306.
#'
#' @noRd
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
