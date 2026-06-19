#' Internal clustering metric metadata and formulas
#'
#' These helpers implement the internal validation criteria used by
#' `validate()` and `feature_importance()`.
#'
#' @name internal-metrics
#' @keywords internal
metric_metadata <- function(metric) {
  metadata <- data.frame(
    metric = c("silhouette", "bootstrap_ari", "davies_bouldin", "total_within", "calinski_harabasz"),
    scale = c("-1 to 1", "0 to 1", "positive, unbounded", "positive, unbounded", "positive, unbounded"),
    direction = c("higher is better", "higher is better", "lower is better", "lower is better", "higher is better"),
    stringsAsFactors = FALSE
  )
  out <- metadata[match(metric, metadata$metric), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Mean silhouette width
#'
#' The silhouette width of an observation is
#'
#' \deqn{
#' s(i) = \frac{b(i) - a(i)}{\max\{a(i), b(i)\}},
#' }
#'
#' where \eqn{a(i)} is the mean distance from observation \eqn{i} to the other
#' observations in its own cluster and \eqn{b(i)} is the minimum mean distance
#' from \eqn{i} to the observations in any other cluster. The returned value is
#' the mean over all observations.
#'
compute_silhouette_metric <- function(data, clusters) {
  if (length(unique(clusters)) < 2L) {
    return(NA_real_)
  }
  sil <- cluster::silhouette(as.integer(clusters), stats::dist(data))
  mean(sil[, "sil_width"])
}

#' Calinski-Harabasz index
#'
#' The Calinski-Harabasz index is
#'
#' \deqn{
#' \mathrm{CH} = \frac{\mathrm{BSS}/(k - 1)}{\mathrm{WSS}/(n - k)},
#' }
#'
#' where \eqn{k} is the number of clusters, \eqn{n} is the number of
#' observations, \eqn{\mathrm{BSS}} is the between-cluster sum of squares, and
#' \eqn{\mathrm{WSS}} is the within-cluster sum of squares.
#'
compute_calinski_harabasz <- function(data, clusters) {
  n <- nrow(data)
  k <- length(unique(clusters))
  overall <- colMeans(data)
  wss <- 0
  bss <- 0
  for (cl in sort(unique(clusters))) {
    idx <- which(clusters == cl)
    center <- colMeans(data[idx, , drop = FALSE])
    wss <- wss + sum(rowSums((data[idx, , drop = FALSE] - matrix(center, length(idx), ncol(data), byrow = TRUE))^2))
    bss <- bss + length(idx) * sum((center - overall)^2)
  }
  (bss / (k - 1)) / (wss / (n - k))
}

#' Davies-Bouldin index
#'
#' The Davies-Bouldin index is
#'
#' \deqn{
#' \mathrm{DB} = \frac{1}{k} \sum_{j=1}^k \max_{l \ne j}
#' \frac{s_j + s_l}{d(\mu_j, \mu_l)},
#' }
#'
#' where \eqn{s_j} is the average scatter of cluster \eqn{j},
#' \eqn{\mu_j} is its centroid, and \eqn{d(\mu_j, \mu_l)} is the distance
#' between cluster centroids.
#'
compute_davies_bouldin <- function(data, clusters) {
  cls <- sort(unique(clusters))
  centers <- do.call(rbind, lapply(cls, function(cl) colMeans(data[clusters == cl, , drop = FALSE])))
  scatters <- vapply(seq_along(cls), function(i) {
    idx <- which(clusters == cls[i])
    mean(sqrt(rowSums((data[idx, , drop = FALSE] - matrix(centers[i, ], length(idx), ncol(data), byrow = TRUE))^2)))
  }, numeric(1))
  center_dist <- as.matrix(stats::dist(centers))
  diag(center_dist) <- NA_real_
  ratios <- vapply(seq_along(cls), function(i) {
    max((scatters[i] + scatters[-i]) / center_dist[i, -i], na.rm = TRUE)
  }, numeric(1))
  mean(ratios)
}

#' Total within-cluster sum of squares
#'
#' The total within-cluster dispersion is
#'
#' \deqn{
#' \mathrm{WSS} = \sum_{j=1}^k \sum_{i \in C_j} \|x_i - \mu_j\|^2.
#' }
#'
compute_total_within <- function(data, clusters) {
  total <- 0
  for (cl in sort(unique(clusters))) {
    idx <- which(clusters == cl)
    center <- colMeans(data[idx, , drop = FALSE])
    total <- total + sum(rowSums((data[idx, , drop = FALSE] - matrix(center, length(idx), ncol(data), byrow = TRUE))^2))
  }
  total
}

#' Mean silhouette width by cluster
#'
#' This returns the average silhouette width within each cluster.
#'
compute_per_cluster_silhouette <- function(data, clusters) {
  if (!requireNamespace("cluster", quietly = TRUE) || length(unique(clusters)) < 2L) {
    return(NULL)
  }
  sil <- cluster::silhouette(as.integer(clusters), stats::dist(data))
  stats::aggregate(sil[, "sil_width"], by = list(cluster = sil[, "cluster"]), FUN = mean)
}
