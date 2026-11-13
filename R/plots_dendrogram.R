#' Plot a clustering dendrogram
#'
#' Draw the dendrogram from a hierarchical `cluster_fit` (fitted via
#' `"hclust"` or `"agnes"`) or the consensus hierarchical tree from a
#' `metacluster_fit`.
#'
#' @param x A hierarchical `cluster_fit` or a `metacluster_fit` object.
#' @param ... Additional arguments passed to the base `plot()` method for
#'   `hclust` objects.
#'
#' @return The underlying `hclust` object, invisibly.
#'
#' @seealso [cluster()], [metacluster()], [plotconsensus()]
#'
#' @export
#'
#' @examples
#' d <- mixeddistance(iris[, 1:4])
#' fit <- cluster(d, method = "hclust", k = 3)
#' plotdendrogram(fit)
#'
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"),
#'                     k = 2:3, seed = 1)
#' plotdendrogram(mfit)
plotdendrogram <- function(x, ...) {
  UseMethod("plotdendrogram")
}

#' @export
plotdendrogram.cluster_fit <- function(x, ...) {
  if (!inherits(x$fitted_object, "hclust")) {
    stop("`plotdendrogram()` is only available for hierarchical fits.", call. = FALSE)
  }
  graphics::plot(x$fitted_object, main = "Cluster dendrogram", xlab = "", sub = "")
  invisible(x$fitted_object)
}

#' @export
plotdendrogram.metacluster_fit <- function(x, ...) {
  graphics::plot(x$consensus_fit, main = "Consensus dendrogram", xlab = "", sub = "")
  invisible(x$consensus_fit)
}
