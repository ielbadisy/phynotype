#' Compare two partitions with an external agreement metric
#'
#' Single entry point for comparing two label vectors (e.g. a clustering
#' result against ground-truth labels), as opposed to [validate()], which
#' scores a fitted `cluster_fit`/`metacluster_fit` object.
#'
#' \describe{
#'   \item{`"arand"`}{Adjusted Rand Index (Hubert and Arabie, 1985),
#'     correcting the Rand Index for chance agreement. Equals 1 for identical
#'     partitions and has expectation 0 under independent random labelings.}
#'   \item{`"nmi"`}{Normalized mutual information (Strehl and Ghosh, 2002).
#'     Ranges from 0 (independent partitions) to 1 (identical partitions).}
#' }
#'
#' @param x Integer or factor vector of cluster labels (first partition).
#' @param y Integer or factor vector of cluster labels (second partition).
#'   Must have the same length as `x`.
#' @param method Which agreement metric to compute: `"arand"` or `"nmi"`. See
#'   Details.
#'
#' @return A single numeric value.
#'
#' @seealso [validate()] for scoring a fitted clustering object.
#'
#' @references
#' Hubert, L. and Arabie, P. (1985). Comparing partitions. *Journal of
#' Classification*, **2**(1), 193--218.
#'
#' Strehl, A. and Ghosh, J. (2002). Cluster ensembles: A knowledge reuse
#' framework for combining multiple partitions. *Journal of Machine Learning
#' Research*, **3**, 583--617.
#'
#' @export
#'
#' @examples
#' evaluate(c(1, 1, 2, 2), c(1, 1, 2, 3))
#' evaluate(c(1, 1, 2, 2), c(1, 1, 2, 3), method = "nmi")
evaluate <- function(x, y, method = c("arand", "nmi")) {
  method <- match.arg(method)
  switch(method,
    arand = .arand_index(x, y),
    nmi = normalized_mutual_information(x, y)
  )
}
