compute_silhouette_metric <- function(data, clusters) {
  if (length(unique(clusters)) < 2L) {
    return(NA_real_)
  }
  sil <- cluster::silhouette(as.integer(clusters), stats::dist(data))
  mean(sil[, "sil_width"])
}

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

compute_total_within <- function(data, clusters) {
  total <- 0
  for (cl in sort(unique(clusters))) {
    idx <- which(clusters == cl)
    center <- colMeans(data[idx, , drop = FALSE])
    total <- total + sum(rowSums((data[idx, , drop = FALSE] - matrix(center, length(idx), ncol(data), byrow = TRUE))^2))
  }
  total
}

compute_per_cluster_silhouette <- function(data, clusters) {
  if (!requireNamespace("cluster", quietly = TRUE) || length(unique(clusters)) < 2L) {
    return(NULL)
  }
  sil <- cluster::silhouette(as.integer(clusters), stats::dist(data))
  stats::aggregate(sil[, "sil_width"], by = list(cluster = sil[, "cluster"]), FUN = mean)
}
