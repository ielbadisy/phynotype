build_feature_summary <- function(data, clusters) {
  cluster_levels <- sort(unique(clusters))
  parts <- lapply(cluster_levels, function(cl) {
    subset <- data[clusters == cl, , drop = FALSE]
    data.frame(
      cluster = cl,
      feature = colnames(data),
      mean = vapply(seq_len(ncol(subset)), function(i) mean(subset[, i]), numeric(1)),
      sd = vapply(seq_len(ncol(subset)), function(i) stats::sd(subset[, i]), numeric(1)),
      median = vapply(seq_len(ncol(subset)), function(i) stats::median(subset[, i]), numeric(1)),
      min = vapply(seq_len(ncol(subset)), function(i) min(subset[, i]), numeric(1)),
      max = vapply(seq_len(ncol(subset)), function(i) max(subset[, i]), numeric(1))
    )
  })
  do.call(rbind, parts)
}

#' Per-feature cluster separation (eta-squared)
#'
#' For feature \eqn{j}, the separation statistic is the eta-squared coefficient
#'
#' \deqn{
#'   \eta^2_j = \frac{\mathrm{SS}_{B,j}}{\mathrm{SS}_{T,j}}
#'            = \frac{\sum_{g=1}^{k} n_g (\bar{x}_{gj} - \bar{x}_j)^2}
#'                   {\sum_{i=1}^{n} (x_{ij} - \bar{x}_j)^2},
#' }
#'
#' where \eqn{\mathrm{SS}_{B,j}} is the between-cluster sum of squares and
#' \eqn{\mathrm{SS}_{T,j}} is the total sum of squares for feature \eqn{j}.
#' Values near 1 indicate that the feature is highly discriminative across
#' clusters; values near 0 indicate that the feature carries little clustering
#' signal.
#'
#' @noRd
compute_separation_table <- function(data, clusters) {
  total_var <- apply(data, 2, stats::var)
  means <- rowsum(data, group = clusters) / as.vector(table(clusters))
  grand_mean <- colMeans(data)
  between <- vapply(seq_len(ncol(data)), function(i) {
    sum(as.vector(table(clusters)) * (means[, i] - grand_mean[i])^2)
  }, numeric(1))
  data.frame(
    feature = colnames(data),
    separation = between / ((nrow(data) - 1) * total_var)
  )
}
