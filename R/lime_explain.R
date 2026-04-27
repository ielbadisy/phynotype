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
#' @param new_data Numeric matrix or data frame containing observations to
#'   explain.
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
#' @param workers Optional number of workers passed to `functionals::fmap()`.
#' @param progress Logical; if `TRUE`, request progress reporting from
#'   `functionals::fmap()`.
#' @param ... Reserved for future extensions.
#'
#' @return A `lime_explanation` object with tidy `explanations` and
#'   `neighborhoods` data frames.
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' lx <- lime_explain(fit, iris[1:2, 1:4], n_permutations = 50, seed = 1)
#' lx
lime_explain <- function(object,
                         new_data,
                         n_features = 5L,
                         n_permutations = 500L,
                         kernel_width = NULL,
                         target = c("cluster", "score"),
                         cluster = NULL,
                         seed = NULL,
                         parallel = FALSE,
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
  p <- ncol(training_data)
  n_features <- min(as.integer(n_features), p)
  n_permutations <- as.integer(n_permutations)
  kernel_width <- if (is.null(kernel_width)) 0.75 * sqrt(p) else kernel_width
  feature_names <- colnames(training_data)
  feature_sds <- safe_column_sds(training_data)

  baseline <- predict_interpretability(object, new_data)
  baseline_clusters <- baseline$prediction$clusters
  tasks <- seq_len(nrow(new_data))

  worker <- function(obs_id) {
    if (!is.null(seed)) {
      set.seed(as.integer(seed) + obs_id)
    }
    center <- new_data[obs_id, , drop = FALSE]
    perturb <- matrix(
      stats::rnorm(n_permutations * p, mean = 0, sd = rep(feature_sds, each = n_permutations)),
      nrow = n_permutations,
      ncol = p
    )
    neighborhood <- sweep(perturb, 2, as.numeric(center), FUN = "+")
    colnames(neighborhood) <- feature_names
    neighborhood[1, ] <- center

    scaled_delta <- sweep(neighborhood, 2, as.numeric(center), FUN = "-")
    scaled_delta <- sweep(scaled_delta, 2, feature_sds, FUN = "/")
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

  pieces <- phynotype_map(tasks, worker, parallel = parallel, workers = workers, progress = progress)
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
      workers = workers
    )
  )
}

#' @export
plot.lime_explanation <- function(x, ...) {
  ggplot2::ggplot(x$explanations, ggplot2::aes_string(x = "feature", y = "estimate", fill = "direction")) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ observation"), scales = "free_y") +
    ggplot2::labs(title = "LIME local explanations", x = NULL, y = "Local surrogate coefficient", fill = "Direction") +
    ggplot2::theme_minimal()
}
