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

#' Select consensus k via mean silhouette
#'
#' Hierarchically clusters the consensus dissimilarity matrix (average linkage)
#' and cuts the tree at each candidate \eqn{k}. The optimal \eqn{k} is the
#' one that maximizes the mean silhouette width:
#'
#' \deqn{
#'   k^* = \operatorname*{arg\,max}_{k \in \mathcal{K}}
#'   \bar{s}_k(D),
#' }
#'
#' where \eqn{\bar{s}_k(D)} is the mean silhouette width computed on the
#' consensus dissimilarity \eqn{D}.
#'
#' @noRd
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
