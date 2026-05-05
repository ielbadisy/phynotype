#' Fit a clustering solution
#'
#' Fit a single clustering solution using a supported method.
#'
#' @param x Numeric matrix/data frame for numeric methods, a `dist` object for
#'   hierarchical methods, or a mixed-type data frame for `"kproto"` and
#'   `"protomix"`.
#' @param method Clustering method. Supported values are `"kmeans"`, `"pam"`,
#'   `"hclust"`, `"agnes"`, `"dbscan"`, `"gmm"`, `"kproto"`, and
#'   `"protomix"`.
#' @param ... Additional method-specific arguments.
#' @param k Number of clusters for methods that require it.
#' @param scale Logical; if `TRUE`, scale columns before fitting.
#' @param center Logical; if `TRUE`, center columns before fitting.
#' @param seed Optional integer random seed.
#'
#' @return A `cluster_fit` object.
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' fit
cluster <- function(x,
                    method = "kmeans",
                    ...,
                    k = NULL,
                    scale = FALSE,
                    center = TRUE,
                    seed = NULL) {
  method <- normalize_method_name(method)
  registry <- get_cluster_registry()
  entry <- registry[[method]]
  if (is.null(entry)) {
    stop("Unsupported clustering method `", method, "`.", call. = FALSE)
  }
  if (!is.null(seed)) {
    validate_seed(seed)
  }

  params <- c(list(k = k, scale = scale, center = center, seed = seed), list(...))
  prepared <- prepare_cluster_input(x, method = method, scale = scale, center = center, k = k)
  entry$validate(prepared$data, params)

  result <- with_seed(seed, entry$fit(prepared$data, params))
  new_cluster_fit(
    method = method,
    call = match.call(),
    params = params,
    clusters = result$clusters,
    n_clusters = result$n_clusters,
    membership = result$membership,
    centers = result$centers,
    prototypes = result$prototypes,
    distance_info = result$distance_info,
    fitted_object = result$fitted_object,
    data_info = prepared$data_info,
    extras = result$extras
  )
}
