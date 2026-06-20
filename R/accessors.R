#' Cluster assignments
#'
#' Extract the integer cluster assignment vector from a fitted clustering object.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return Integer vector of length equal to the number of training
#'   observations.
#'
#' @seealso [sizes()], [n_clusters()], [centers()], [membership()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' clusters(fit)
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
#' Extract the soft membership (posterior probability) matrix. Only methods
#' that return probabilistic assignments (e.g. GMM) populate this slot; all
#' other methods return `NULL`.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return A numeric matrix with one row per observation and one column per
#'   cluster, or `NULL`.
#'
#' @seealso [clusters()], [centers()], [prototypes()]
#'
#' @export
#'
#' @examples
#' if (requireNamespace("mclust", quietly = TRUE)) {
#'   fit <- cluster(iris[, 1:4], method = "gmm", k = 3, seed = 1)
#'   head(membership(fit))
#' }
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
#' Extract the numeric cluster center matrix. Available for k-means and
#' hierarchical methods trained on numeric data; returns `NULL` for PAM,
#' k-prototypes, and GMM (use [prototypes()] or [membership()] instead).
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return A numeric matrix with one row per cluster and one column per
#'   feature, or `NULL`.
#'
#' @seealso [prototypes()], [clusters()], [membership()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' centers(fit)
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
#' Extract the cluster prototype table. Prototypes are actual observations
#' (PAM medoids) or mixed-type representatives (k-prototypes, KMM); returns
#' `NULL` for methods that use centroids rather than prototypes.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return A matrix or data frame with one row per cluster, or `NULL`.
#'
#' @seealso [centers()], [clusters()]
#'
#' @export
#'
#' @examples
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   fit <- cluster(iris[, 1:4], method = "pam", k = 3)
#'   prototypes(fit)
#' }
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
#' Return the number of observations assigned to each cluster.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return Named integer vector, where names are the cluster labels.
#'
#' @seealso [clusters()], [n_clusters()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' sizes(fit)
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
#' Return the number of clusters in a fitted clustering object. For DBSCAN,
#' this excludes the noise cluster (label 0).
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return Integer scalar.
#'
#' @seealso [sizes()], [clusters()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' n_clusters(fit)
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
#' Return the name of the clustering method used to fit the object.
#'
#' @param x A `cluster_fit` or `metacluster_fit` object.
#' @param ... Unused.
#'
#' @return Character scalar (e.g. `"kmeans"`, `"pam"`, `"hclust"`).
#'
#' @seealso [cluster()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' method_used(fit)
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
