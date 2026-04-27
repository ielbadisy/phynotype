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
#' @param new_data Numeric matrix or data frame containing observations to
#'   explain.
#' @param features Optional character vector or numeric column index specifying
#'   features to profile.
#' @param grid Optional numeric vector or named list of numeric vectors giving
#'   profile values. If `NULL`, feature-specific quantile grids are built from
#'   the training data.
#' @param grid_size Number of grid values per feature when `grid = NULL`.
#' @param target Output target. `"cluster"` records predicted labels;
#'   `"score"` records a cluster-specific prediction score.
#' @param cluster Optional cluster whose score should be profiled when
#'   `target = "score"`. If `NULL`, each observation is profiled for its
#'   baseline predicted cluster.
#' @param parallel Logical; if `TRUE`, use `functionals::fmap()` when the
#'   suggested `functionals` package is installed.
#' @param workers Optional number of workers passed to `functionals::fmap()`.
#' @param progress Logical; if `TRUE`, request progress reporting from
#'   `functionals::fmap()`.
#' @param ... Reserved for future extensions.
#'
#' @return A `ceteris_paribus` object with a tidy `profiles` data frame.
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' cp <- ceteris_paribus(fit, iris[1:2, 1:4], features = 1:2, grid_size = 5)
#' cp
ceteris_paribus <- function(object,
                            new_data,
                            features = NULL,
                            grid = NULL,
                            grid_size = 25L,
                            target = c("cluster", "score"),
                            cluster = NULL,
                            parallel = FALSE,
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
    } else {
      score_col <- cluster_score_column(pred$scores, cluster, fallback = baseline_clusters[[obs_id]])
      out_value <- pred$scores[, score_col]
      out_cluster <- rep(score_col, length(values))
    }
    data.frame(
      observation = obs_id,
      feature = feature,
      feature_value = values,
      target = target,
      cluster = as.character(out_cluster),
      value = as.numeric(out_value)
    )
  }

  profiles <- do.call(
    rbind,
    phynotype_map(task_rows, worker, parallel = parallel, workers = workers, progress = progress)
  )
  rownames(profiles) <- NULL
  new_ceteris_paribus(
    profiles = profiles,
    settings = list(
      target = target,
      cluster = cluster,
      grid_size = as.integer(grid_size),
      parallel = parallel,
      workers = workers
    )
  )
}

#' @export
plot.ceteris_paribus <- function(x, ...) {
  ggplot2::ggplot(x$profiles, ggplot2::aes_string(x = "feature_value", y = "value", color = "cluster", group = "interaction(observation, cluster)")) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(stats::as.formula("~ feature"), scales = "free_x") +
    ggplot2::labs(title = "Ceteris paribus profiles", x = "Feature value", y = "Profile value", color = "Cluster") +
    ggplot2::theme_minimal()
}
