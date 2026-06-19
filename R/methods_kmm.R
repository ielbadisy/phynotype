#' Validation helpers for KMM
#'
#' @param data A data frame or matrix.
#' @param params Method parameters.
#' @keywords internal
validate_kmm_params <- function(data, params) {
  k <- check_required_k(params$k)
  if (!is.data.frame(data)) {
    stop("`kmm` requires row-by-feature data.", call. = FALSE)
  }
  if (k > nrow(data)) {
    stop("`k` cannot exceed the number of observations.", call. = FALSE)
  }
}

#' Fit the KMM algorithm
#'
#' KMM is a mixed-data clustering algorithm that minimizes a weighted
#' prototype distance combining squared Euclidean distance on numeric variables
#' and mismatch counts on categorical variables:
#'
#' \deqn{
#' D(x_i, p_g) = \sum_{j \in \mathcal{N}} (x_{ij} - \mu_{gj})^2 +
#' \lambda \sum_{j \in \mathcal{C}} I(x_{ij} \neq \nu_{gj}),
#' }
#'
#' where \eqn{\lambda \ge 0} balances categorical and numeric contributions.
#'
#' @param data Mixed-type data frame.
#' @param params Method parameters.
#' @keywords internal
fit_kmm <- function(data, params) {
  fit <- kmm(
    data = data,
    k = as.integer(params$k),
    lambda = if (is.null(params$lambda)) "auto" else params$lambda,
    nstart = if (is.null(params$nstart)) 10L else as.integer(params$nstart),
    iter.max = if (is.null(params$iter.max)) 100L else as.integer(params$iter.max),
    tol = if (is.null(params$tol)) 1e-6 else params$tol,
    scale_numeric = if (is.null(params$scale_numeric)) TRUE else isTRUE(params$scale_numeric),
    center_num = if (is.null(params$center_num)) "mean" else params$center_num,
    missing = if (is.null(params$missing)) "pairwise" else params$missing,
    seed = params$seed
  )

  list(
    clusters = fit$cluster,
    n_clusters = fit$k,
    membership = NULL,
    centers = NULL,
    prototypes = .kmm_combined_prototypes(fit$prototypes_original),
    distance_info = list(metric = "kmm", lambda = fit$lambda),
    fitted_object = fit,
    extras = list(
      objective = fit$objective,
      iterations = fit$iterations,
      within_components = fit$within_components,
      lambda = fit$lambda,
      prototypes = fit$prototypes
    )
  )
}

#' Predict with KMM
#'
#' @param object A `cluster_fit` object.
#' @param new_data Mixed-type data frame.
#' @keywords internal
predict_kmm <- function(object, new_data, ...) {
  info <- object$data_info
  data <- as.data.frame(new_data, stringsAsFactors = FALSE)
  expected <- info$feature_names
  missing_vars <- setdiff(expected, names(data))
  if (length(missing_vars) > 0L) {
    stop("`new_data` is missing variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }
  data <- data[expected]

  prep <- .kmm_prepare(
    data,
    scale_numeric = if (!is.null(object$fitted_object$scale_numeric)) object$fitted_object$scale_numeric else TRUE,
    num_center = object$fitted_object$num_center,
    num_scale = object$fitted_object$num_scale,
    factor_levels = object$fitted_object$factor_levels
  )
  dist_mat <- .kmm_distance_matrix(
    prep$x_num,
    prep$x_cat,
    object$fitted_object$prototypes,
    object$fitted_object$lambda,
    object$fitted_object$missing
  )
  cls <- max.col(-dist_mat, ties.method = "first")
  new_cluster_prediction(
    clusters = cls,
    membership = NULL,
    distances = sqrt(dist_mat),
    method = object$method,
    prediction_type = "nearest_prototype"
  )
}

kmm <- function(data,
                k,
                lambda = "auto",
                nstart = 10,
                iter.max = 100,
                tol = 1e-6,
                scale_numeric = TRUE,
                center_num = c("mean", "median", "trimmed"),
                missing = c("pairwise", "fail"),
                seed = NULL) {
  center_num <- match.arg(center_num)
  missing <- match.arg(missing)
  data <- .kmm_check_data(data)
  prep <- .kmm_prepare(data, scale_numeric = scale_numeric)
  n <- .kmm_n(prep$x_num, prep$x_cat)
  if (k > n) stop("`k` cannot exceed the number of observations.", call. = FALSE)
  if (missing == "fail" && anyNA(data)) {
    stop("Missing values detected; use missing = 'pairwise' or handle missing values before fitting.", call. = FALSE)
  }
  factor_levels <- lapply(data[prep$cat_vars], function(x) levels(as.factor(x)))
  if (identical(lambda, "auto")) {
    lambda <- estimate_lambda(prep$x_num, prep$x_cat)
  }
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda < 0) {
    stop("`lambda` must be 'auto' or a single non-negative number.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  x_mats <- .kmm_data_matrices(prep$x_num, prep$x_cat, n = n)
  best_fit <- NULL
  best_obj <- Inf
  for (s in seq_len(as.integer(nstart))) {
    fit_s <- kmm_single_start(
      x_num = prep$x_num,
      x_cat = prep$x_cat,
      x_num_m = x_mats$x_num,
      x_cat_m = x_mats$x_cat,
      k = as.integer(k),
      lambda = lambda,
      iter.max = as.integer(iter.max),
      tol = tol,
      center_num = center_num,
      missing = missing
    )
    if (fit_s$objective < best_obj) {
      best_fit <- fit_s
      best_obj <- fit_s$objective
    }
  }

  best_fit$lambda <- lambda
  best_fit$k <- as.integer(k)
  best_fit$num_vars <- prep$num_vars
  best_fit$cat_vars <- prep$cat_vars
  best_fit$scale_numeric <- scale_numeric
  best_fit$num_center <- prep$num_center
  best_fit$num_scale <- prep$num_scale
  best_fit$factor_levels <- factor_levels
  best_fit$center_num <- center_num
  best_fit$missing <- missing
  best_fit$prototypes_original <- .kmm_prototypes_original_scale(
    best_fit$prototypes, prep$num_center, prep$num_scale, scale_numeric
  )
  best_fit$call <- match.call()
  class(best_fit) <- "kmm"
  best_fit
}

kmm_single_start <- function(x_num, x_cat, x_num_m, x_cat_m, k, lambda, iter.max, tol,
                             center_num, missing) {
  n <- .kmm_n(x_num, x_cat)
  init_id <- sample(seq_len(n), k)
  proto <- list(
    num = if (ncol(x_num) > 0L) as.matrix(x_num[init_id, , drop = FALSE]) else NULL,
    cat = if (ncol(x_cat) > 0L) as.matrix(x_cat[init_id, , drop = FALSE]) else NULL
  )
  cluster <- rep(NA_integer_, n)
  old_objective <- Inf
  objective <- Inf
  for (iter in seq_len(iter.max)) {
    dist_mat <- .kmm_distance_matrix_mats(x_num_m, x_cat_m, proto, lambda, missing)
    if (any(!is.finite(apply(dist_mat, 1L, min)))) {
      stop("At least one observation has no observed distance components.", call. = FALSE)
    }
    new_cluster <- max.col(-dist_mat, ties.method = "random")
    objective <- sum(dist_mat[cbind(seq_len(n), new_cluster)])
    empty <- which(tabulate(new_cluster, nbins = k) == 0L)
    if (length(empty) > 0L) {
      far <- order(apply(dist_mat, 1L, min), decreasing = TRUE)[seq_along(empty)]
      for (j in seq_along(empty)) new_cluster[far[j]] <- empty[j]
    }
    proto <- update_prototypes(x_num, x_cat, new_cluster, k, center_num)
    if (identical(new_cluster, cluster) || abs(old_objective - objective) < tol) {
      cluster <- new_cluster
      break
    }
    cluster <- new_cluster
    old_objective <- objective
  }
  prototypes <- update_prototypes(x_num, x_cat, cluster, k, center_num)
  within_components <- .kmm_within_components(x_num, x_cat, cluster, prototypes, lambda)
  list(
    cluster = cluster,
    size = as.integer(tabulate(cluster, nbins = k)),
    k = k,
    lambda = lambda,
    objective = objective,
    iterations = iter,
    prototypes = prototypes,
    within_components = within_components
  )
}

#' Estimate the KMM lambda weight
#'
#' @param data Mixed-type data.
#' @param scale_numeric Logical.
#' @keywords internal
lambda_kmm <- function(data, scale_numeric = TRUE) {
  prep <- .kmm_prepare(data, scale_numeric = scale_numeric)
  estimate_lambda(prep$x_num, prep$x_cat)
}

estimate_lambda <- function(x_num, x_cat) {
  num_part <- 1
  if (ncol(x_num) > 0L) {
    num_part <- mean(vapply(x_num, stats::var, numeric(1), na.rm = TRUE), na.rm = TRUE)
  }
  if (ncol(x_cat) == 0L) return(0)
  gini <- function(z) {
    p <- prop.table(table(z, useNA = "no"))
    if (length(p) == 0L) return(NA_real_)
    1 - sum(p^2)
  }
  cat_part <- mean(vapply(x_cat, gini, numeric(1)), na.rm = TRUE)
  if (is.na(cat_part) || cat_part <= 0) return(1)
  num_part / cat_part
}

.kmm_within_components <- function(x_num, x_cat, cluster, prototypes, lambda) {
  num <- 0
  cat <- 0
  for (g in seq_len(length(unique(cluster)))) {
    idx <- which(cluster == g)
    if (length(idx) == 0L) next
    if (ncol(x_num) > 0L && !is.null(prototypes$num)) {
      num <- num + sum(rowSums((as.matrix(x_num[idx, , drop = FALSE]) -
        matrix(prototypes$num[g, ], length(idx), ncol(x_num), byrow = TRUE))^2))
    }
    if (ncol(x_cat) > 0L && !is.null(prototypes$cat)) {
      for (j in seq_len(ncol(x_cat))) {
        cat <- cat + sum(as.character(x_cat[idx, j]) != as.character(prototypes$cat[g, j]))
      }
    }
  }
  list(numeric = num, categorical = lambda * cat, total = num + lambda * cat)
}
