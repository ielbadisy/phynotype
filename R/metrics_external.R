#' External clustering agreement metrics
#'
#' These helpers implement the reference-label metrics used by `validate()`.
#'
#' @noRd
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
#' \mathrm{NMI}(x, y) = \frac{I(x;y)}{\sqrt{H(x)H(y)}},
#' }
#'
#' where \eqn{I} is mutual information and \eqn{H} is Shannon entropy.
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
