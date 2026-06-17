#' Meta-cluster stability summaries
#'
#' Pairwise partition agreement between two labelings \eqn{x} and \eqn{y} is the
#' proportion of observation pairs that are consistently grouped in both
#' partitions:
#'
#' \deqn{
#' \mathrm{PPA}(x, y) = \frac{1}{\binom{n}{2}}
#' \sum_{i < j} I\{(x_i = x_j) = (y_i = y_j)\}.
#' }
#'
#' @noRd
compute_metacluster_stability <- function(candidate_labels) {
  if (length(candidate_labels) < 2L) {
    return(NULL)
  }
  pairs <- utils::combn(seq_along(candidate_labels), 2)
  agreements <- apply(pairs, 2, function(idx) {
    pairwise_partition_agreement(candidate_labels[[idx[1]]], candidate_labels[[idx[2]]])
  })
  data.frame(
    metric = "pairwise_partition_agreement",
    mean_agreement = mean(agreements),
    min_agreement = min(agreements),
    max_agreement = max(agreements)
  )
}

#' Bootstrap ARI stability
#'
#' For bootstrap repetition \eqn{b}, draw a sample with replacement from the
#' original data, refit the clustering model, and compute the adjusted Rand
#' index between the reference partition and the bootstrap partition on the
#' sampled observations. The reported bootstrap stability is the mean ARI over
#' repetitions.
#'
#' @noRd
bootstrap_cluster_stability <- function(x, method, k, n_boot = 10, seed = NULL, ...) {
  x <- as.matrix(x)
  reference <- cluster(x, method = method, k = k, seed = seed, ...)
  ref_clusters <- clusters(reference)
  scores <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    idx <- sample(seq_len(nrow(x)), replace = TRUE)
    boot_fit <- cluster(x[idx, , drop = FALSE], method = method, k = k, seed = seed, ...)
    mapped <- integer(nrow(x))
    mapped[idx] <- clusters(boot_fit)
    scores[i] <- adjusted_rand_index(ref_clusters[idx], mapped[idx])
  }
  data.frame(metric = "bootstrap_ari", mean = mean(scores), sd = stats::sd(scores))
}
