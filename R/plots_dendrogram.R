#' Plot a clustering dendrogram
#'
#' @param x A hierarchical `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return The underlying fitted dendrogram object, invisibly.
#' @export
plot_dendrogram <- function(x, ...) {
  UseMethod("plot_dendrogram")
}

#' @export
plot_dendrogram.cluster_fit <- function(x, ...) {
  if (!inherits(x$fitted_object, "hclust")) {
    stop("`plot_dendrogram()` is only available for hierarchical fits.", call. = FALSE)
  }
  graphics::plot(x$fitted_object, main = "Cluster dendrogram", xlab = "", sub = "")
  invisible(x$fitted_object)
}

#' @export
plot_dendrogram.metacluster_fit <- function(x, ...) {
  graphics::plot(x$consensus_fit, main = "Consensus dendrogram", xlab = "", sub = "")
  invisible(x$consensus_fit)
}
