#' Validate clustering results
#'
#' Compute internal and external validation metrics for a fitted clustering
#' object or over a grid of candidate cluster counts.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or numeric matrix/data frame.
#'   When a raw data matrix is supplied, `validate()` fits and scores solutions
#'   for each value in `k` using `method`.
#' @param ... Additional arguments passed to methods.
#' @param method Clustering method used when `x` is raw data.
#' @param k Candidate values of `k` for direct grid validation.
#' @param truth Optional integer or factor vector of reference labels. When
#'   supplied, the adjusted Rand index and normalized mutual information are
#'   appended to the metric table.
#' @param metrics Optional character vector selecting a subset of metrics to
#'   return (e.g. `c("silhouette", "calinski_harabasz")`).
#' @param n_boot Number of bootstrap resamples for bootstrap ARI stability
#'   (only for `"kmeans"`, `"pam"`, and `"gmm"` fits with a fixed `k`).
#'
#' @details
#' ## Internal metrics
#'
#' **Silhouette width** (Rousseeuw, 1987):
#' \deqn{
#'   s(i) = \frac{b(i) - a(i)}{\max\{a(i),\, b(i)\}},
#' }
#' where \eqn{a(i)} is the mean intra-cluster distance and \eqn{b(i)} is the
#' minimum mean distance to any other cluster. Values near 1 indicate dense,
#' well-separated clusters.
#'
#' **Calinski-Harabasz index** (Calinski and Harabasz, 1974):
#' \deqn{
#'   \mathrm{CH} = \frac{\mathrm{BSS} / (k-1)}{\mathrm{WSS} / (n-k)},
#' }
#' where BSS is the between-cluster sum of squares and WSS is the
#' within-cluster sum of squares. Higher values indicate better separation.
#'
#' **Davies-Bouldin index** (Davies and Bouldin, 1979):
#' \deqn{
#'   \mathrm{DB} = \frac{1}{k} \sum_{j=1}^{k}
#'   \max_{l \ne j} \frac{s_j + s_l}{d(\mu_j, \mu_l)},
#' }
#' where \eqn{s_j} is the mean intra-cluster scatter. Lower values are better.
#'
#' **Total within-cluster sum of squares**:
#' \deqn{
#'   \mathrm{WSS} = \sum_{j=1}^{k} \sum_{i \in C_j} \|x_i - \mu_j\|^2.
#' }
#'
#' **Bootstrap ARI** (Fang and Wang, 2012): mean adjusted Rand index between
#' the reference partition and partitions fitted on bootstrap resamples.
#'
#' ## External metrics
#'
#' **Adjusted Rand index** (Hubert and Arabie, 1985):
#' \deqn{
#'   \mathrm{ARI} = \frac{\sum_{ij}\binom{n_{ij}}{2} - E}{\frac{1}{2}
#'   \!\left[\sum_i\binom{a_i}{2}+\sum_j\binom{b_j}{2}\right] - E},
#' }
#' where \eqn{E} is the expected index under random partitions. Values near 1
#' indicate agreement close to the reference.
#'
#' **Normalized mutual information** (Strehl and Ghosh, 2002):
#' \deqn{
#'   \mathrm{NMI}(U, V) = \frac{I(U; V)}{\sqrt{H(U)\, H(V)}}.
#' }
#'
#' @return A `cluster_validation` object with components:
#' \describe{
#'   \item{`metrics_table`}{Data frame with columns `metric`, `value`,
#'     `scale`, `direction`.}
#'   \item{`per_cluster_table`}{Per-cluster mean silhouette widths, or
#'     `NULL`.}
#' }
#'
#' @seealso [cluster()] to fit a solution, [plot_silhouette()] to visualize
#'   per-observation silhouette widths, [explore()] for structural summaries.
#'
#' @references
#' Rousseeuw, P.J. (1987). Silhouettes: A graphical aid to the interpretation
#' and validation of cluster analysis. *Journal of Computational and Applied
#' Mathematics*, **20**, 53–65.
#'
#' Calinski, T. and Harabasz, J. (1974). A dendrite method for cluster
#' analysis. *Communications in Statistics*, **3**(1), 1–27.
#'
#' Davies, D.L. and Bouldin, D.W. (1979). A cluster separation measure. *IEEE
#' Transactions on Pattern Analysis and Machine Intelligence*, **1**(2),
#' 224–227.
#'
#' Hubert, L. and Arabie, P. (1985). Comparing partitions. *Journal of
#' Classification*, **2**(1), 193–218.
#'
#' Strehl, A. and Ghosh, J. (2002). Cluster ensembles: A knowledge reuse
#' framework for combining multiple partitions. *Journal of Machine Learning
#' Research*, **3**, 583–617.
#'
#' Fang, Y. and Wang, J. (2012). Selection of the number of clusters via the
#' bootstrap method. *Computational Statistics and Data Analysis*, **56**(3),
#' 468–477.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#'
#' # Internal metrics
#' val <- validate(fit)
#' val$metrics_table
#'
#' # External metrics with known labels
#' val_ext <- validate(fit, truth = iris$Species)
#' val_ext$metrics_table
#'
#' # Grid search over k values
#' grid_val <- validate(iris[, 1:4], method = "kmeans", k = 2:5)
#' grid_val$metrics_table
validate <- function(x,
                     ...,
                     method = "kmeans",
                     k = NULL,
                     truth = NULL,
                     metrics = NULL,
                     n_boot = 10) {
  UseMethod("validate")
}

append_metric_row <- function(metrics_table, metric, value, scale = NA_character_, direction = NA_character_) {
  metrics_table[nrow(metrics_table) + 1L, ] <- list(metric, value, scale, direction)
  metrics_table
}

#' @export
validate.cluster_fit <- function(x, ..., truth = NULL, metrics = NULL, n_boot = 10) {
  if (x$method == "pam" && !requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required to compute silhouette for `pam` fits.", call. = FALSE)
  }
  data <- x$data_info$original_data
  if (inherits(data, "dist")) {
    stop("Validation for distance-only inputs is not yet implemented.", call. = FALSE)
  }
  metric_data <- data
  if (is.data.frame(data) && any(!vapply(data, is.numeric, logical(1)))) {
    metric_data <- prepare_mixed_data(data, center = FALSE, scale = FALSE)
  }
  metrics_table <- data.frame(
    metric = c("silhouette", "calinski_harabasz", "davies_bouldin", "total_within"),
    value = c(
      if (requireNamespace("cluster", quietly = TRUE)) compute_silhouette_metric(metric_data, x$clusters) else NA_real_,
      compute_calinski_harabasz(metric_data, x$clusters),
      compute_davies_bouldin(metric_data, x$clusters),
      compute_total_within(metric_data, x$clusters)
    ),
    scale = metric_metadata(c("silhouette", "calinski_harabasz", "davies_bouldin", "total_within"))$scale,
    direction = metric_metadata(c("silhouette", "calinski_harabasz", "davies_bouldin", "total_within"))$direction,
    stringsAsFactors = FALSE
  )
  if (!is.null(truth)) {
    metrics_table <- append_metric_row(
      metrics_table,
      "ari",
      adjusted_rand_index(truth, x$clusters)
    )
    metrics_table <- append_metric_row(
      metrics_table,
      "nmi",
      normalized_mutual_information(truth, x$clusters)
    )
  }
  stability <- if (x$method %in% c("kmeans", "pam", "gmm") && !is.null(x$params$k)) {
    bootstrap_cluster_stability(data, method = x$method, k = x$params$k, n_boot = n_boot, seed = x$params$seed)
  } else {
    NULL
  }
  if (!is.null(stability)) {
    metrics_table <- append_metric_row(metrics_table, "bootstrap_ari", stability$mean)
  }
  if (!is.null(metrics)) {
    metrics_table <- metrics_table[metrics_table$metric %in% metrics, , drop = FALSE]
  }
  if (!all(c("scale", "direction") %in% names(metrics_table))) {
    extra_meta <- metric_metadata(metrics_table$metric[is.na(metrics_table$scale)])
    metrics_table$scale[is.na(metrics_table$scale)] <- extra_meta$scale
    metrics_table$direction[is.na(metrics_table$direction)] <- extra_meta$direction
  }
  new_cluster_validation(
    metrics_table = metrics_table,
    per_cluster_table = compute_per_cluster_silhouette(metric_data, x$clusters),
    settings = list(method = x$method, n_boot = n_boot),
    object_type = class(x)[1],
    extras = list(stability = stability)
  )
}

#' @export
validate.metacluster_fit <- function(x, ..., truth = NULL, metrics = NULL, n_boot = 10) {
  data <- x$data_info$original_data
  metrics_table <- data.frame(
    metric = c("silhouette", "calinski_harabasz", "davies_bouldin"),
    value = c(
      if (requireNamespace("cluster", quietly = TRUE)) compute_silhouette_metric(data, x$final_clusters) else NA_real_,
      compute_calinski_harabasz(data, x$final_clusters),
      compute_davies_bouldin(data, x$final_clusters)
    ),
    scale = metric_metadata(c("silhouette", "calinski_harabasz", "davies_bouldin"))$scale,
    direction = metric_metadata(c("silhouette", "calinski_harabasz", "davies_bouldin"))$direction,
    stringsAsFactors = FALSE
  )
  if (!is.null(truth)) {
    metrics_table <- append_metric_row(
      metrics_table,
      "ari",
      adjusted_rand_index(truth, x$final_clusters)
    )
    metrics_table <- append_metric_row(
      metrics_table,
      "nmi",
      normalized_mutual_information(truth, x$final_clusters)
    )
  }
  if (!is.null(x$stability_summary)) {
    metrics_table <- append_metric_row(metrics_table, "pairwise_partition_agreement", x$stability_summary$mean_agreement)
  }
  if (!is.null(metrics)) {
    metrics_table <- metrics_table[metrics_table$metric %in% metrics, , drop = FALSE]
  }
  if (!all(c("scale", "direction") %in% names(metrics_table))) {
    extra_meta <- metric_metadata(metrics_table$metric[is.na(metrics_table$scale)])
    metrics_table$scale[is.na(metrics_table$scale)] <- extra_meta$scale
    metrics_table$direction[is.na(metrics_table$direction)] <- extra_meta$direction
  }
  new_cluster_validation(
    metrics_table = metrics_table,
    per_cluster_table = compute_per_cluster_silhouette(data, x$final_clusters),
    settings = list(method = "metacluster", n_boot = n_boot),
    object_type = class(x)[1],
    extras = list(selection_summary = x$selection_summary)
  )
}

#' @export
validate.default <- function(x,
                             ...,
                             method = "kmeans",
                             k = NULL,
                             truth = NULL,
                             metrics = NULL,
                             n_boot = 10) {
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Direct validation requires a numeric matrix or data frame.", call. = FALSE)
  }
  k_values <- check_k_grid(k)
  rows <- lapply(k_values, function(k_i) {
    fit <- cluster(x, method = method, k = k_i, ...)
    val <- validate(fit, truth = truth, metrics = metrics, n_boot = n_boot)
    out <- val$metrics_table
    out$k <- k_i
    out
  })
  new_cluster_validation(
    metrics_table = do.call(rbind, rows),
    settings = list(method = method, grid_k = k_values),
    object_type = "validation_grid"
  )
}
