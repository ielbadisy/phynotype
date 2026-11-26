#' Co-association matrix
#'
#' For \eqn{B} candidate partitions \eqn{\{U^{(b)}\}_{b=1}^{B}}, the
#' co-association matrix \eqn{C \in [0,1]^{n \times n}} is
#'
#' \deqn{
#'   C_{ij} = \frac{1}{B} \sum_{b=1}^{B}
#'   I\!\left\{u^{(b)}_i = u^{(b)}_j\right\},
#' }
#'
#' the proportion of candidate partitions that assign observations \eqn{i} and
#' \eqn{j} to the same cluster. The diagonal is fixed to 1. Observations
#' labeled as noise (cluster 0) do not contribute to co-assignments.
#' The consensus dissimilarity is \eqn{D = 1 - C} (Fred and Jain, 2002).
#'
#' @references
#' Fred, A.L.N. and Jain, A.K. (2002). Data clustering using evidence
#' accumulation. *Proceedings of the 16th International Conference on Pattern
#' Recognition (ICPR'02)*, Vol. 4, pp. 276--280.
#'
#' @noRd
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

#' Pairwise partition agreement
#'
#' The proportion of observation pairs \eqn{(i,j)} with \eqn{i < j} for which
#' partitions \eqn{U} and \eqn{V} agree on whether \eqn{i} and \eqn{j} are
#' co-clustered:
#'
#' \deqn{
#'   \mathrm{PPA}(U, V) = \frac{1}{\binom{n}{2}}
#'   \sum_{i < j} I\!\left\{(u_i = u_j) = (v_i = v_j)\right\}.
#' }
#'
#' @noRd
pairwise_partition_agreement <- function(x, y) {
  mean(outer(x, x, FUN = "==") == outer(y, y, FUN = "=="))
}

#' Select the consensus k
#'
#' Cuts the average-linkage tree of the consensus dissimilarity at each
#' candidate \eqn{k}. With `select = "ari"` the selected \eqn{k} maximizes the
#' mean adjusted Rand index between the cut \eqn{\pi_k} and the \eqn{B}
#' candidate partitions (a median-partition criterion, Strehl and Ghosh, 2002):
#'
#' \deqn{
#'   k^* = \operatorname*{arg\,max}_{k \in \mathcal{K}}
#'   \frac{1}{B} \sum_{b=1}^{B} \mathrm{ARI}(\pi_k, U^{(b)}),
#' }
#'
#' restricted to cuts whose smallest cluster has at least `min_size`
#' observations (all cuts are eligible if none qualifies). Ties go to the
#' smaller \eqn{k}. The mean silhouette width on \eqn{D} is always reported;
#' it is the criterion with `select = "silhouette"`. It tends to grow with
#' \eqn{k} because \eqn{D} is built from the large-\eqn{k} candidates too.
#'
#' @noRd
select_consensus_k <- function(consensus_dist, k_values, labels, select = "ari", min_size = 2L) {
  hc <- stats::hclust(consensus_dist, method = "average")
  cuts <- lapply(k_values, function(k) stats::cutree(hc, k = k))
  sil <- vapply(cuts, function(cl) {
    if (length(unique(cl)) < 2L || !requireNamespace("cluster", quietly = TRUE)) return(NA_real_)
    mean(cluster::silhouette(cl, consensus_dist)[, "sil_width"])
  }, numeric(1))
  ari <- vapply(cuts, function(cl) mean(vapply(labels, adjustedrandindex, numeric(1), cl)), numeric(1))
  smallest <- vapply(cuts, function(cl) min(tabulate(cl)), integer(1))
  eligible <- smallest >= min_size
  if (!any(eligible)) eligible[] <- TRUE
  score <- if (select == "ari") ari else sil
  score[!eligible] <- NA
  best_idx <- which.max(score)
  if (!length(best_idx)) best_idx <- 1L
  list(
    final_k = as.integer(k_values[[best_idx]]),
    score_table = data.frame(k = k_values, mean_ari = ari, silhouette = sil, min_size = smallest, eligible = eligible)
  )
}
