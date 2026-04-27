#' Cluster assignments
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return An integer vector.
#' @export
clusters <- function(x, ...) {
  UseMethod("clusters")
}

#' @export
clusters.cluster_fit <- function(x, ...) {
  x$clusters
}

#' @export
clusters.metacluster_fit <- function(x, ...) {
  x$final_clusters
}

#' Membership matrix
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return A matrix or `NULL`.
#' @export
membership <- function(x, ...) {
  UseMethod("membership")
}

#' @export
membership.cluster_fit <- function(x, ...) {
  x$membership
}

#' @export
membership.metacluster_fit <- function(x, ...) {
  NULL
}

#' Cluster centers
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return A matrix or `NULL`.
#' @export
centers <- function(x, ...) {
  UseMethod("centers")
}

#' @export
centers.cluster_fit <- function(x, ...) {
  x$centers
}

#' @export
centers.metacluster_fit <- function(x, ...) {
  NULL
}

#' Cluster prototypes
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return A matrix, data frame, or `NULL`.
#' @export
prototypes <- function(x, ...) {
  UseMethod("prototypes")
}

#' @export
prototypes.cluster_fit <- function(x, ...) {
  x$prototypes
}

#' @export
prototypes.metacluster_fit <- function(x, ...) {
  NULL
}

#' Cluster sizes
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return Named integer vector.
#' @export
sizes <- function(x, ...) {
  UseMethod("sizes")
}

#' @export
sizes.cluster_fit <- function(x, ...) {
  tab <- table(x$clusters, useNA = "ifany")
  stats::setNames(as.integer(tab), names(tab))
}

#' @export
sizes.metacluster_fit <- function(x, ...) {
  tab <- table(x$final_clusters, useNA = "ifany")
  stats::setNames(as.integer(tab), names(tab))
}

#' Number of clusters
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return Integer scalar.
#' @export
n_clusters <- function(x, ...) {
  UseMethod("n_clusters")
}

#' @export
n_clusters.cluster_fit <- function(x, ...) {
  x$n_clusters
}

#' @export
n_clusters.metacluster_fit <- function(x, ...) {
  x$final_k
}

#' Method used
#'
#' @param x A clustering object.
#' @param ... Unused.
#'
#' @return Character scalar.
#' @export
method_used <- function(x, ...) {
  UseMethod("method_used")
}

#' @export
method_used.cluster_fit <- function(x, ...) {
  x$method
}

#' @export
method_used.metacluster_fit <- function(x, ...) {
  x$method
}
