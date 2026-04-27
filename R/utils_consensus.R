compute_coassoc_matrix <- function(labels) {
  if (length(labels) < 1L) {
    stop("At least one candidate partition is required.", call. = FALSE)
  }
  n <- length(labels[[1]])
  accum <- matrix(0, nrow = n, ncol = n)
  for (lab in labels) {
    same <- outer(lab, lab, FUN = "==")
    same[lab == 0, ] <- FALSE
    same[, lab == 0] <- FALSE
    diag(same) <- TRUE
    accum <- accum + same
  }
  accum / length(labels)
}

pairwise_partition_agreement <- function(x, y) {
  mean(outer(x, x, FUN = "==") == outer(y, y, FUN = "=="))
}

select_consensus_k <- function(consensus_dist, k_values) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    return(list(final_k = k_values[[1]], score_table = data.frame(k = k_values, silhouette = NA_real_)))
  }
  hc <- stats::hclust(consensus_dist, method = "average")
  scores <- vapply(k_values, function(k) {
    cls <- stats::cutree(hc, k = k)
    sil <- cluster::silhouette(cls, consensus_dist)
    mean(sil[, "sil_width"])
  }, numeric(1))
  best_idx <- which.max(scores)
  list(final_k = as.integer(k_values[[best_idx]]), score_table = data.frame(k = k_values, silhouette = scores))
}
