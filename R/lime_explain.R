#' LIME-style local explanations for clustering
#'
#' Explain individual cluster assignments or cluster scores with a locally
#' weighted surrogate model. The method builds a synthetic neighborhood around
#' each observation, predicts the fitted clustering behavior in that
#' neighborhood, weights perturbed samples by proximity to the explained
#' observation, and fits a simple weighted linear model.
#'
#' For an observation \eqn{x}, perturbed samples \eqn{z_i}, fitted clustering
#' rule \eqn{f}, target extractor \eqn{g}, local surrogate \eqn{h}, and kernel
#' \eqn{\pi_x(z_i)}, the LIME objective is
#'
#' \deqn{
#'   \hat{h}
#'   =
#'   \arg\min_{h \in H}
#'   \sum_{i=1}^{N}
#'   \pi_x(z_i)
#'   \left[g\{f(z_i)\} - h(z_i)\right]^2
#'   + \Omega(h).
#' }
#'
#' phynotype uses a weighted linear surrogate for \eqn{H}. Feature ranking is
#' based on the absolute fitted local coefficients after standardizing features
#' by the training-data scale. The penalty term \eqn{\Omega(h)} is represented
#' operationally by returning the largest `n_features` effects, which keeps the
#' explanation sparse and readable without adding a heavy modeling dependency.
#'
#' For `target = "cluster"`, \eqn{g\{f(z_i)\}} is an indicator that the
#' predicted cluster equals the cluster being explained. For `target = "score"`,
#' it is the cluster-specific membership or similarity score.
#'
#' @param object A `cluster_fit` object with prediction support.
#' @param new_data Row-by-feature data containing observations to explain.
#' @param n_features Maximum number of local effects returned per observation.
#' @param n_permutations Number of perturbed neighborhood samples per
#'   observation.
#' @param kernel_width Positive numeric kernel width. If `NULL`, uses
#'   `0.75 * sqrt(p)`, where `p` is the number of features.
#' @param target Output target. `"cluster"` explains a one-vs-cluster local
#'   assignment indicator; `"score"` explains a cluster-specific prediction
#'   score.
#' @param cluster Optional cluster to explain. If `NULL`, each observation is
#'   explained for its baseline predicted cluster.
#' @param seed Optional integer random seed.
#' @param parallel Logical; if `TRUE`, use `functionals::fmap()` when the
#'   suggested `functionals` package is installed.
#' @param cores Optional positive integer number of cores passed to
#'   `functionals::fmap()` when `parallel = TRUE`.
#' @param workers Optional number of workers passed to `functionals::fmap()`.
#'   Deprecated alias for `cores`.
#' @param progress Logical; if `TRUE`, request progress reporting from
#'   `functionals::fmap()`.
#' @param ... Reserved for future extensions.
#'
#' @return A `lime_explanation` object with components:
#' \describe{
#'   \item{`explanations`}{Tidy data frame with one row per (observation,
#'     feature) pair, containing the local surrogate coefficient, absolute
#'     effect size, direction, and rank.}
#'   \item{`neighborhoods`}{Data frame with all perturbed samples, their
#'     predicted responses, kernel weights, and distances to the explained
#'     observation.}
#' }
#'
#' @seealso [ceteris_paribus()] for individual conditional profiles,
#'   [feature_importance()] for global importance, [predict.cluster_fit()]
#'   which is used internally.
#'
#' @references
#' Ribeiro, M.T., Singh, S. and Guestrin, C. (2016). "Why should I trust you?":
#' Explaining the predictions of any classifier. *Proceedings of the 22nd ACM
#' SIGKDD International Conference on Knowledge Discovery and Data Mining*,
#' pp. 1135--1144.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' lx <- lime_explain(fit, iris[1:2, 1:4], n_permutations = 50, seed = 1)
#' lx$explanations
lime_explain <- function(object,
                         new_data,
                         n_features = 5L,
                         n_permutations = 500L,
                         kernel_width = NULL,
                         target = c("cluster", "score"),
                         cluster = NULL,
                         seed = NULL,
                         parallel = FALSE,
                         cores = NULL,
                         workers = NULL,
                         progress = FALSE,
                         ...) {
  check_interpretable_fit(object)
  target <- match.arg(target)
  if (!is.numeric(n_features) || length(n_features) != 1L || n_features < 1L || is.na(n_features)) {
    stop("`n_features` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(n_permutations) || length(n_permutations) != 1L || n_permutations < 10L || is.na(n_permutations)) {
    stop("`n_permutations` must be an integer greater than or equal to 10.", call. = FALSE)
  }
  if (!is.null(kernel_width) && (!is.numeric(kernel_width) || length(kernel_width) != 1L || kernel_width <= 0 || is.na(kernel_width))) {
    stop("`kernel_width` must be `NULL` or a positive number.", call. = FALSE)
  }
  if (!is.null(seed)) {
    validate_seed(seed)
  }

  new_data <- prepare_interpretability_data(object, new_data, arg = "new_data")
  training_data <- prepare_interpretability_data(object, NULL)
  training_frame <- as.data.frame(training_data, stringsAsFactors = FALSE)
  new_frame <- as.data.frame(new_data, stringsAsFactors = FALSE)
  encoded_training <- interpretability_numeric_basis(training_data)
  p <- ncol(encoded_training)
  n_features <- min(as.integer(n_features), p)
  n_permutations <- as.integer(n_permutations)
  kernel_width <- if (is.null(kernel_width)) 0.75 * sqrt(p) else kernel_width
  feature_names <- colnames(encoded_training)
  if (is.null(feature_names)) {
    feature_names <- paste0("feature_", seq_len(p))
    colnames(encoded_training) <- feature_names
  }
  encoded_sds <- safe_column_sds(encoded_training)
  original_numeric_cols <- vapply(training_frame, is.numeric, logical(1))
  original_sds <- vapply(training_frame[original_numeric_cols], stats::sd, numeric(1))
  original_sds[!is.finite(original_sds) | original_sds <= 0] <- 1

  baseline <- predict_interpretability(object, new_data)
  baseline_clusters <- baseline$prediction$clusters
  tasks <- seq_len(nrow(new_data))

  worker <- function(obs_id) {
    if (!is.null(seed)) {
      set.seed(as.integer(seed) + obs_id)
    }
    center <- new_frame[obs_id, , drop = FALSE]
    neighborhood <- center[rep(1L, n_permutations), , drop = FALSE]
    if (any(original_numeric_cols)) {
      num_names <- names(training_frame)[original_numeric_cols]
      for (nm in num_names) {
        neighborhood[[nm]] <- stats::rnorm(
          n_permutations,
          mean = as.numeric(center[[nm]]),
          sd = original_sds[[nm]]
        )
      }
    }
    if (any(!original_numeric_cols)) {
      cat_names <- names(training_frame)[!original_numeric_cols]
      for (nm in cat_names) {
        pool <- training_frame[[nm]]
        neighborhood[[nm]] <- sample(pool, n_permutations, replace = TRUE)
      }
    }
    neighborhood[1, ] <- center

    encoded_neighborhood <- encode_interpretability_like(neighborhood, training_frame)
    encoded_center <- encode_interpretability_like(center, training_frame)
    scaled_delta <- sweep(encoded_neighborhood, 2, as.numeric(encoded_center[1, ]), FUN = "-")
    scaled_delta <- sweep(scaled_delta, 2, encoded_sds, FUN = "/")
    distances <- sqrt(rowSums(scaled_delta^2))
    weights <- exp(-(distances^2) / (kernel_width^2))

    pred <- predict_interpretability(object, neighborhood)
    explained_cluster <- if (is.null(cluster)) baseline_clusters[[obs_id]] else cluster
    if (target == "cluster") {
      response <- as.numeric(pred$prediction$clusters == explained_cluster)
      response_name <- as.character(explained_cluster)
    } else {
      score_col <- cluster_score_column(pred$scores, explained_cluster, fallback = baseline_clusters[[obs_id]])
      response <- pred$scores[, score_col]
      response_name <- score_col
    }

    design <- cbind("(Intercept)" = 1, scaled_delta)
    fit <- stats::lm.wfit(x = design, y = response, w = weights)
    coefs <- fit$coefficients[-1]
    coefs[is.na(coefs)] <- 0
    ord <- order(abs(coefs), decreasing = TRUE)
    keep <- ord[seq_len(min(n_features, length(ord)))]
    explained <- data.frame(
      observation = obs_id,
      cluster = as.character(response_name),
      target = target,
      feature = names(coefs)[keep],
      estimate = as.numeric(coefs[keep]),
      absolute_effect = abs(as.numeric(coefs[keep])),
      direction = ifelse(coefs[keep] >= 0, "positive", "negative"),
      rank = seq_along(keep)
    )
    neigh <- data.frame(
      observation = obs_id,
      sample = seq_len(n_permutations),
      cluster = as.character(response_name),
      target = target,
      response = response,
      weight = weights,
      distance = distances
    )
    list(explanations = explained, neighborhoods = neigh)
  }

  pieces <- phynotype_map(tasks, worker, parallel = parallel, cores = cores, workers = workers, progress = progress)
  explanations <- do.call(rbind, lapply(pieces, `[[`, "explanations"))
  neighborhoods <- do.call(rbind, lapply(pieces, `[[`, "neighborhoods"))
  rownames(explanations) <- NULL
  rownames(neighborhoods) <- NULL

  new_lime_explanation(
    explanations = explanations,
    neighborhoods = neighborhoods,
    settings = list(
      n_features = n_features,
      n_permutations = n_permutations,
      kernel_width = kernel_width,
      target = target,
      cluster = cluster,
      seed = seed,
      parallel = parallel,
      cores = cores,
      workers = workers
    )
  )
}

encode_interpretability_like <- function(data, reference) {
  combined <- rbind(reference, data)
  encoded <- interpretability_numeric_basis(combined)
  encoded[(nrow(reference) + 1L):nrow(encoded), , drop = FALSE]
}

#' @export
plot.lime_explanation <- function(x, ...) {
  ggplot2::ggplot(
    x$explanations,
    ggplot2::aes(x = .data[["feature"]], y = .data[["estimate"]], fill = .data[["direction"]])
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ observation"), scales = "free_y") +
    ggplot2::labs(title = "LIME local explanations", x = NULL, y = "Local surrogate coefficient", fill = "Direction") +
    ggplot2::theme_minimal()
}
