#' Permutation feature importance for clustering
#'
#' Estimate global feature importance by measuring how much a fitted clustering
#' solution changes when each feature is independently permuted. The method is
#' model-agnostic: it uses the fitted object's `predict()` method and therefore
#' applies to clustering engines with native prediction support.
#'
#' Let \eqn{f} denote the fitted clustering rule, \eqn{X} the evaluation data,
#' \eqn{\hat{c}=f(X)} the baseline predicted partition, and
#' \eqn{X^{(m)}_{\pi(j)}} the data matrix where feature \eqn{j} has been
#' permuted in repetition \eqn{m}. For the default instability metric, feature
#' importance is
#'
#' \deqn{
#'   FI_j = \frac{1}{M}\sum_{m=1}^{M}
#'   \frac{1}{n}\sum_{i=1}^{n}
#'   I\{\hat{c}_i \neq f(X^{(m)}_{\pi(j)})_i\}.
#' }
#'
#' This quantity estimates the expected fraction of assignments that change
#' when the marginal information in feature \eqn{j} is broken. Larger values
#' indicate that the fitted clustering rule relies more strongly on that
#' feature.
#'
#' For score-style internal metrics, the package computes a baseline score
#' \eqn{S(f, X)} and a permuted score
#' \eqn{S(f, X^{(m)}_{\pi(j)})}. For silhouette, larger is better and
#'
#' \deqn{
#'   FI_j = \frac{1}{M}\sum_{m=1}^{M}
#'   \left[S(f, X) - S(f, X^{(m)}_{\pi(j)})\right].
#' }
#'
#' For total within-cluster sum of squares, smaller is better and
#'
#' \deqn{
#'   FI_j = \frac{1}{M}\sum_{m=1}^{M}
#'   \left[W(f, X^{(m)}_{\pi(j)}) - W(f, X)\right].
#' }
#'
#' Users can pass a custom `loss` function for domain-specific objectives. It
#' must accept `(data, clusters, object)` and return a numeric scalar where
#' larger values mean worse fit.
#'
#' @param object A `cluster_fit` object with prediction support.
#' @param data Optional row-by-feature data used for evaluation. Defaults to the
#'   training data stored in `object`.
#' @param features Optional character vector or numeric column index specifying
#'   features to evaluate.
#' @param metric Built-in importance metric. `"instability"` measures the
#'   fraction of changed cluster assignments. `"silhouette"` measures loss of
#'   mean silhouette width. `"total_within"` measures increase in within-cluster
#'   dispersion.
#' @param loss Optional custom loss function with signature
#'   `loss(data, clusters, object)`.
#' @param n_repeats Number of independent permutations per feature.
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
#' @return A `feature_importance` object with components:
#' \describe{
#'   \item{`results`}{Data frame with one row per (feature, repeat) combination
#'     recording the baseline loss, the permuted loss, and the computed
#'     importance.}
#'   \item{`summary`}{Data frame with one row per feature, containing the mean
#'     importance, standard error, and number of repeats.}
#' }
#'
#' @seealso [lime_explain()] for local explanations, [ceteris_paribus()] for
#'   individual conditional profiles, [predict.cluster_fit()] which is used
#'   internally.
#'
#' @references
#' Breiman, L. (2001). Random forests. *Machine Learning*, **45**(1), 5--32.
#' (Permutation importance concept introduced in the context of random forests.)
#'
#' Fisher, A., Rudin, C. and Dominici, F. (2019). All models are wrong, but
#' many are useful: Learning a variable's importance by studying an entire class
#' of prediction models simultaneously. *Journal of Machine Learning Research*,
#' **20**(177), 1--81.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' imp <- feature_importance(fit, n_repeats = 3, seed = 1)
#' imp$summary
feature_importance <- function(object,
                               data = NULL,
                               features = NULL,
                               metric = c("instability", "silhouette", "total_within"),
                               loss = NULL,
                               n_repeats = 10L,
                               seed = NULL,
                               parallel = FALSE,
                               cores = NULL,
                               workers = NULL,
                               progress = FALSE,
                               ...) {
  check_interpretable_fit(object)
  metric <- match.arg(metric)
  if (!is.null(loss) && !is.function(loss)) {
    stop("`loss` must be `NULL` or a function.", call. = FALSE)
  }
  if (!is.numeric(n_repeats) || length(n_repeats) != 1L || n_repeats < 1L || is.na(n_repeats)) {
    stop("`n_repeats` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(seed)) {
    validate_seed(seed)
  }
  data <- prepare_interpretability_data(object, data)
  features <- match_interpretability_features(features, data)
  n_repeats <- as.integer(n_repeats)

  baseline <- predict_interpretability(object, data)
  baseline_clusters <- baseline$prediction$clusters
  baseline_loss <- if (is.null(loss)) {
    if (metric == "instability") 0 else interpretability_metric(metric, data, baseline_clusters)
  } else {
    loss(data, baseline_clusters, object)
  }

  tasks <- expand.grid(feature = features, repeat_id = seq_len(n_repeats), stringsAsFactors = FALSE)
  task_rows <- split(tasks, seq_len(nrow(tasks)))

  worker <- function(task) {
    task <- task[1, , drop = FALSE]
    if (!is.null(seed)) {
      set.seed(as.integer(seed) + match(task$feature, features) * 100000L + task$repeat_id)
    }
    permuted <- data
    permuted[, task$feature] <- sample(permuted[, task$feature])
    pred <- predict_interpretability(object, permuted)
    permuted_clusters <- pred$prediction$clusters
    permuted_loss <- if (is.null(loss)) {
      if (metric == "instability") {
        mean(permuted_clusters != baseline_clusters)
      } else {
        interpretability_metric(metric, permuted, permuted_clusters)
      }
    } else {
      loss(permuted, permuted_clusters, object)
    }
    importance <- if (is.null(loss) && metric == "silhouette") {
      baseline_loss - permuted_loss
    } else if (is.null(loss) && metric == "instability") {
      permuted_loss
    } else {
      permuted_loss - baseline_loss
    }
    data.frame(
      feature = task$feature,
      repeat_id = task$repeat_id,
      baseline = baseline_loss,
      permuted = permuted_loss,
      importance = importance
    )
  }

  results <- do.call(
    rbind,
    phynotype_map(task_rows, worker, parallel = parallel, cores = cores, workers = workers, progress = progress)
  )
  rownames(results) <- NULL
  summary <- summarize_importance_results(results)
  new_feature_importance(
    results = results,
    summary = summary,
    settings = list(
      metric = metric,
      n_repeats = n_repeats,
      seed = seed,
      parallel = parallel,
      cores = cores,
      workers = workers,
      used_custom_loss = !is.null(loss)
    )
  )
}

#' @export
plot.feature_importance <- function(x, ...) {
  ggplot2::ggplot(x$summary, ggplot2::aes_string(x = "feature", y = "importance")) +
    ggplot2::geom_col(fill = "#2C7FB8") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Permutation feature importance", x = NULL, y = "Importance") +
    ggplot2::theme_minimal()
}
