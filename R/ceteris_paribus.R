#' Ceteris paribus profiles for clustering
#'
#' Compute individual conditional profiles for a fitted clustering rule. A
#' ceteris paribus profile varies one feature over a grid while holding all
#' other features fixed at an observed profile. It describes how the predicted
#' cluster assignment or a cluster score changes locally as a single feature is
#' perturbed.
#'
#' For observation \eqn{x_i}, feature \eqn{j}, and grid value \eqn{z}, define
#' \eqn{x_{i,-j}} as all coordinates of \eqn{x_i} except feature \eqn{j}. The
#' ceteris paribus profile is
#'
#' \deqn{
#'   CP_{i,j}(z) = g\{f(x_{i,-j}, z)\},
#' }
#'
#' where \eqn{f} is the fitted clustering rule and \eqn{g} extracts either the
#' predicted cluster label or a cluster-specific score. For methods that return
#' membership probabilities, the score is the predicted membership. For
#' distance-based methods, phynotype converts distances into normalized radial
#' similarity scores so that larger values indicate stronger association with a
#' cluster prototype.
#'
#' These profiles are local diagnostic curves. They should be interpreted as
#' behavior of the fitted clustering rule under controlled perturbation, not as
#' causal effects.
#'
#' @param object A `cluster_fit` object with prediction support.
#' @param new_data Row-by-feature data containing observations to explain.
#' @param features Optional character vector or numeric column index specifying
#'   features to profile.
#' @param grid Optional vector or named list of vectors giving profile values.
#'   If `NULL`, numeric features use quantile grids and categorical features use
#'   their observed levels.
#' @param grid_size Number of grid values per feature when `grid = NULL`.
#' @param target Output target. `"cluster"` records predicted labels;
#'   `"score"` records a cluster-specific prediction score.
#' @param cluster Optional cluster whose score should be profiled when
#'   `target = "score"`. If `NULL`, each observation is profiled for its
#'   baseline predicted cluster.
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
#' @return A `ceteris_paribus` object with a tidy `profiles` data frame. Each
#'   row records: the observation index, the feature being varied, the grid
#'   value at that step, the predicted target (cluster label or score), the
#'   observed feature value, and the baseline prediction.
#'
#' @seealso [lime_explain()] for local linear surrogate explanations,
#'   [feature_importance()] for global permutation importance,
#'   [predict.cluster_fit()] which is used internally.
#'
#' @references
#' Biecek, P. and Burzykowski, T. (2021). *Explanatory Model Analysis*.
#' Chapman and Hall/CRC, Boca Raton. \url{https://ema.drwhy.ai/}
#'
#' Gosiewska, A. and Biecek, P. (2019). iBreakDown: Uncertainty of model
#' explanations for non-additive predictive models. *arXiv:1903.11420*.
#'
#' Apley, D.W. and Zhu, J. (2020). Visualizing the effects of predictor
#' variables in black box supervised learning models. *Journal of the Royal
#' Statistical Society: Series B*, **82**(4), 1059--1086.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' cp <- ceteris_paribus(fit, iris[1:3, 1:4], features = 1:2, grid_size = 10)
#' head(cp$profiles)
ceteris_paribus <- function(object,
                            new_data,
                            features = NULL,
                            grid = NULL,
                            grid_size = 25L,
                            target = c("cluster", "score"),
                            cluster = NULL,
                            parallel = FALSE,
                            cores = NULL,
                            workers = NULL,
                            progress = FALSE,
                            ...) {
  check_interpretable_fit(object)
  target <- match.arg(target)
  if (!is.numeric(grid_size) || length(grid_size) != 1L || grid_size < 2L || is.na(grid_size)) {
    stop("`grid_size` must be an integer greater than or equal to 2.", call. = FALSE)
  }
  new_data <- prepare_interpretability_data(object, new_data, arg = "new_data")
  training_data <- prepare_interpretability_data(object, NULL)
  features <- match_interpretability_features(features, training_data)

  baseline <- predict_interpretability(object, new_data)
  baseline_clusters <- baseline$prediction$clusters
  tasks <- expand.grid(observation = seq_len(nrow(new_data)), feature = features, stringsAsFactors = FALSE)
  task_rows <- split(tasks, seq_len(nrow(tasks)))

  worker <- function(task) {
    task <- task[1, , drop = FALSE]
    obs_id <- task$observation
    feature <- task$feature
    values <- make_feature_grid(training_data, feature, grid = grid, grid_size = as.integer(grid_size))
    profile_data <- new_data[rep(obs_id, length(values)), , drop = FALSE]
    profile_data[, feature] <- values
    pred <- predict_interpretability(object, profile_data)
    if (target == "cluster") {
      out_value <- pred$prediction$clusters
      out_cluster <- pred$prediction$clusters
      baseline_value <- baseline_clusters[[obs_id]]
      baseline_cluster <- baseline_clusters[[obs_id]]
    } else {
      score_col <- cluster_score_column(pred$scores, cluster, fallback = baseline_clusters[[obs_id]])
      out_value <- pred$scores[, score_col]
      out_cluster <- rep(score_col, length(values))
      baseline_value <- baseline$scores[obs_id, score_col]
      baseline_cluster <- score_col
    }
    data.frame(
      observation = obs_id,
      feature = feature,
      feature_value = values,
      target = target,
      cluster = as.character(out_cluster),
      value = as.numeric(out_value),
      observed_value = as.vector(new_data[obs_id, feature, drop = TRUE]),
      baseline_value = as.numeric(baseline_value),
      baseline_cluster = as.character(baseline_cluster)
    )
  }

  profiles <- do.call(
    rbind,
    phynotype_map(task_rows, worker, parallel = parallel, cores = cores, workers = workers, progress = progress)
  )
  rownames(profiles) <- NULL
  new_ceteris_paribus(
    profiles = profiles,
    settings = list(
      target = target,
      cluster = cluster,
      grid_size = as.integer(grid_size),
      method = object$method,
      parallel = parallel,
      cores = cores,
      workers = workers
    )
  )
}

#' @export
plot.ceteris_paribus <- function(x, ...) {
  teal <- "#44B8C1"
  purple <- "#3A22B8"
  profile_data <- x$profiles
  numeric_x <- suppressWarnings(!anyNA(as.numeric(profile_data$feature_value)))
  if (numeric_x) {
    profile_data$feature_value <- as.numeric(profile_data$feature_value)
  }
  baseline_data <- unique(profile_data[, c(
    "observation", "feature", "observed_value",
    "baseline_value", "baseline_cluster"
  )])
  if (numeric_x) {
    baseline_data$observed_value <- as.numeric(baseline_data$observed_value)
  }
  line_layer <- if (identical(x$settings$target, "cluster")) {
    ggplot2::geom_step(color = teal, linewidth = 0.8, alpha = 0.95)
  } else {
    ggplot2::geom_line(color = teal, linewidth = 0.9, alpha = 0.95)
  }
  baseline_layers <- if (numeric_x) {
    list(
      ggplot2::geom_vline(
        data = baseline_data,
        ggplot2::aes(xintercept = observed_value),
        color = purple,
        linetype = "dashed",
        linewidth = 0.45,
        alpha = 0.75
      ),
      ggplot2::geom_point(
        data = baseline_data,
        ggplot2::aes(x = observed_value, y = baseline_value),
        inherit.aes = FALSE,
        color = purple,
        fill = purple,
        size = 2.6
      )
    )
  } else {
    list()
  }

  ggplot2::ggplot(
    profile_data,
    ggplot2::aes(
      x = feature_value,
      y = value,
      group = interaction(observation, cluster)
    )
  ) +
    line_layer +
    baseline_layers +
    ggplot2::facet_wrap(stats::as.formula("~ feature"), scales = "free_x") +
    ggplot2::labs(
      title = "Ceteris Paribus profile",
      subtitle = paste("created for the", x$settings$method, "model"),
      x = NULL,
      y = "prediction"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = purple, size = 14, face = "plain"),
      plot.subtitle = ggplot2::element_text(color = purple, size = 9),
      axis.title.y = ggplot2::element_text(color = purple),
      axis.text = ggplot2::element_text(color = purple),
      strip.text = ggplot2::element_text(color = purple, face = "plain"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "#E7E7E7", linewidth = 0.3),
      panel.grid.major.y = ggplot2::element_line(color = "#E7E7E7", linewidth = 0.3),
      legend.position = "none"
    )
}
