#' External clustering agreement metrics
#'
#' These helpers implement the reference-label metrics used by `validate()`.
#'
#' @name external-metrics
#' @keywords internal
NULL

#' Adjusted Rand Index
#'
#' Computes the Adjusted Rand Index (ARI) between two partitions, correcting
#' the Rand Index for chance agreement (Hubert and Arabie, 1985). ARI equals 1
#' for identical partitions and has expectation 0 under independent random
#' labelings.
#'
#' @param x Integer or factor vector of cluster labels (first partition).
#' @param y Integer or factor vector of cluster labels (second partition).
#'   Must have the same length as `x`.
#'
#' @return A single numeric value in \eqn{(-\infty, 1]}.
#'
#' @references
#' Hubert, L. and Arabie, P. (1985). Comparing partitions. *Journal of
#' Classification*, **2**(1), 193–218.
#'
#' @examples
#' adjusted_rand_index(c(1, 1, 2, 2), c(1, 1, 2, 3))
#'
#' @export
adjusted_rand_index <- function(x, y) {
  tab <- table(x, y)
  n <- sum(tab)
  a <- rowSums(tab)
  b <- colSums(tab)
  choose2 <- function(v) ifelse(v < 2, 0, v * (v - 1) / 2)
  sum_comb <- sum(choose2(tab))
  a_comb <- sum(choose2(a))
  b_comb <- sum(choose2(b))
  expected <- a_comb * b_comb / choose2(n)
  max_index <- 0.5 * (a_comb + b_comb)
  (sum_comb - expected) / (max_index - expected)
}

#' Normalized mutual information
#'
#' \deqn{
#'   \mathrm{NMI}(U, V) = \frac{I(U; V)}{\sqrt{H(U)\,H(V)}},
#' }
#'
#' where \eqn{I(U; V) = \sum_{u,v} p_{uv} \log\!\frac{p_{uv}}{p_u p_v}} is
#' the mutual information and \eqn{H(U) = -\sum_u p_u \log p_u} is the Shannon
#' entropy. Values range from 0 (independent partitions) to 1 (identical
#' partitions).
#'
#' @references
#' Strehl, A. and Ghosh, J. (2002). Cluster ensembles: A knowledge reuse
#' framework for combining multiple partitions. *Journal of Machine Learning
#' Research*, **3**, 583–617.
#'
#' @noRd
normalized_mutual_information <- function(x, y) {
  tab <- table(x, y)
  pxy <- tab / sum(tab)
  px <- rowSums(pxy)
  py <- colSums(pxy)
  nonzero <- pxy > 0
  mi <- sum(pxy[nonzero] * log(pxy[nonzero] / (px[row(pxy)][nonzero] * py[col(pxy)][nonzero])))
  hx <- -sum(px[px > 0] * log(px[px > 0]))
  hy <- -sum(py[py > 0] * log(py[py > 0]))
  mi / sqrt(hx * hy)
}
